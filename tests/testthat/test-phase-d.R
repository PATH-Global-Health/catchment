# ---- Phase D: learned distance-decay -----------------------------------------
# Shared fixture: build a dat that carries a raw-travel template (dat$travel_sparse)
# using the SAME preprocessing as the golden master (n_fac_limit = 6,
# force_threshold = 300). Built once and reused across tests.

.d_dat <- local({
  dat <- NULL
  function() {
    if (!is.null(dat)) return(dat)
    skip_on_cran(); skip_if_not_installed("INLA")

    pop <- example_pop()
    data("example_locs", package = "catchment")
    locs <- example_locs[1:10, ]

    fric <- pop
    set.seed(1)
    terra::values(fric) <- stats::runif(terra::ncell(fric), 0.001, 0.02)

    dir <- withr::local_tempdir()
    suppressMessages(
      create_travel_surface(
        friction_surface = fric, extent_file = pop,
        points = locs, id_col = "label", x_col = "x", y_col = "y",
        output_dir = dir, individual_surfaces = TRUE, overwrite = TRUE)
    )
    tmat <- travel_mat_from_folder(dir = dir, reference = pop, progress = FALSE)

    # Pass the raw travel matrix directly so prepare_data builds travel_sparse.
    dat <<- suppressMessages(
      prepare_data(prob_mat_init = tmat, pop_raster = pop, location_data = locs,
                   n_fac_limit = 6, force_threshold = 300,
                   mesh.args = list(cutoff = 0.1, max.edge = c(0.2, 4)))
    )
    dat
  }
})

# ---- acceptance gate: power decay fixed at a = 2 reproduces golden master -----

test_that("power decay fixed at a=2 reproduces the golden master", {
  skip_on_cran(); skip_if_not_installed("INLA")
  snap_path <- testthat::test_path("fixtures/golden-master.rds")
  skip_if(!file.exists(snap_path), "Golden master fixture not found")
  golden <- readRDS(snap_path)

  mod <- suppressMessages(suppressWarnings(
    catchment_model(.d_dat(), family = "poisson", decay = "power",
                    estimate_decay = FALSE, decay_init = 2, time = FALSE)
  ))

  rep <- mod$obj$report()
  expect_equal(rep$case_hf, golden$case_hf, tolerance = 1e-3,
               label = "case_hf vs golden master (power a=2)")
  expect_equal(rep$pop_hf, golden$pop_hf, tolerance = 1e-3,
               label = "pop_hf vs golden master (power a=2)")
  expect_equal(catchment_populations(mod), golden$catchment_populations,
               tolerance = 1e-3,
               label = "catchment_populations vs golden master (power a=2)")
})

# ---- travel_sparse template is built and shaped correctly --------------------

test_that("prepare_data builds a travel template matching the legacy mask", {
  skip_on_cran(); skip_if_not_installed("INLA")
  dat <- .d_dat()
  expect_false(is.null(dat$travel_sparse))
  # [n_hf x n_pixel]
  expect_equal(nrow(dat$travel_sparse), length(dat$loc_labels))
  expect_equal(ncol(dat$travel_sparse), length(dat$pop_vec))
  # Sparsity pattern must equal the legacy decayed surface's nonzero pattern.
  leg <- t(as.matrix(dat$prob_mat_init))           # [n_hf x n_pixel]
  expect_equal(as.matrix(dat$travel_sparse) != 0, leg != 0)
  # Stored values are clamped travel times (>= minimum_time at nonzeros).
  vals <- as.matrix(dat$travel_sparse)
  expect_true(all(vals[vals != 0] >= 10 - 1e-9))
})

# ---- fallback when no travel template is present -----------------------------

test_that("decay request without a travel template falls back to none", {
  skip_on_cran(); skip_if_not_installed("INLA")
  pop  <- make_test_raster(); locs <- make_test_points(4)
  # Pre-built probability matrix dimensioned to the raster's valid pixels.
  pv   <- terra::values(pop, mat = FALSE)
  n_v  <- sum(!is.na(pv) & pv > 0)
  pmat <- matrix(runif(n_v * 4), nrow = n_v, ncol = 4)   # [n_pixel x n_hf]
  dat  <- suppressMessages(
    prepare_data(prob_mat_init = pmat, pop_raster = pop, location_data = locs)
  )
  expect_null(dat$travel_sparse)
  expect_message(
    obj <- make_model_object(dat, family = "poisson", decay = "exponential"),
    "falling back"
  )
  expect_equal(attr(obj, "decay"), "none")
})

# ---- decay argument is recorded ----------------------------------------------

test_that("catchment_model records the resolved decay family", {
  skip_on_cran(); skip_if_not_installed("INLA")
  mod <- suppressMessages(suppressWarnings(
    catchment_model(.d_dat(), decay = "exponential", time = FALSE)
  ))
  expect_equal(mod$decay, "exponential")
  expect_true("log_decay" %in% names(mod$fit$par))   # estimated by default
})

test_that("fixed decay leaves log_decay out of the optimisation par", {
  skip_on_cran(); skip_if_not_installed("INLA")
  mod <- suppressMessages(suppressWarnings(
    catchment_model(.d_dat(), decay = "power", estimate_decay = FALSE,
                    decay_init = 2, time = FALSE)
  ))
  expect_false("log_decay" %in% names(mod$fit$par))
})

# ---- R reconstruction matches the C++ REPORT ---------------------------------

test_that(".reconstruct_probs reproduces the C++ pop_hf (decay active)", {
  skip_on_cran(); skip_if_not_installed("INLA")
  mod <- suppressMessages(suppressWarnings(
    catchment_model(.d_dat(), decay = "exponential", time = FALSE)
  ))
  pm   <- catchment:::.reconstruct_probs(mod)
  pophf_r   <- as.vector(t(pm) %*% mod$data$pop_vec)
  pophf_cpp <- mod$obj$report()$pop_hf
  expect_equal(pophf_r, pophf_cpp, tolerance = 1e-4)
})

# ---- decay-parameter recovery on simulated data ------------------------------
# Simulate facility counts from a KNOWN decay, then check the estimate recovers
# it. Field and mass are held near zero so the decay parameter is the dominant
# driver of the relative allocation; large counts keep Poisson noise low.

.simulate_counts <- function(dat, decay_type, dpar, seed = 99, scale = 4000) {
  ts <- t(as.matrix(dat$travel_sparse))      # [n_pixel x n_hf] clamped travel
  nz <- ts != 0
  w  <- ts
  if (decay_type == "power") {
    w[nz] <- ts[nz] ^ (-dpar)
  } else {
    w[nz] <- exp(-ts[nz] / dpar)
  }
  w <- w / rowSums(w)                        # per-pixel allocation
  pop_hf <- as.vector(t(w) %*% dat$pop_vec)
  mu <- scale * pop_hf / sum(pop_hf)         # expected counts
  set.seed(seed)
  stats::rpois(length(mu), mu)
}

test_that("power decay parameter is recovered on simulated data", {
  skip_on_cran(); skip_if_not_installed("INLA")
  dat <- .d_dat()
  true_a <- 2.5
  sim_dat <- dat
  sim_dat$weights      <- .simulate_counts(dat, "power", true_a)
  sim_dat$which_not_NA <- as.numeric(!is.na(sim_dat$weights))

  mod <- suppressMessages(suppressWarnings(
    catchment_model(sim_dat, family = "poisson", decay = "power",
                    estimate_decay = TRUE, decay_init = 1.5, time = FALSE)
  ))
  a_hat <- exp(unname(mod$fit$par["log_decay"]))
  expect_equal(a_hat, true_a, tolerance = 0.35)   # within ~35%
})

test_that("exponential decay parameter is recovered on simulated data", {
  skip_on_cran(); skip_if_not_installed("INLA")
  dat <- .d_dat()
  true_tau <- 45
  sim_dat <- dat
  sim_dat$weights      <- .simulate_counts(dat, "exponential", true_tau)
  sim_dat$which_not_NA <- as.numeric(!is.na(sim_dat$weights))

  mod <- suppressMessages(suppressWarnings(
    catchment_model(sim_dat, family = "poisson", decay = "exponential",
                    estimate_decay = TRUE, decay_init = 90, time = FALSE)
  ))
  tau_hat <- exp(unname(mod$fit$par["log_decay"]))
  expect_equal(tau_hat, true_tau, tolerance = 0.35)
})
