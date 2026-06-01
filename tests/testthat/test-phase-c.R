# ---- shared fixture helpers --------------------------------------------------
# Build the dat object ONCE for the whole file (covers Phases C golden-master
# comparisons).  Subsequent helpers reuse this dat so create_travel_surface()
# runs only once.

.c_dat <- local({
  dat <- NULL
  function() {
    if (!is.null(dat)) return(dat)
    skip_on_cran()
    skip_if_not_installed("INLA")

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
        output_dir = dir, individual_surfaces = TRUE, overwrite = TRUE
      )
    )
    tmat  <- travel_mat_from_folder(dir = dir, reference = pop, progress = FALSE)
    pmat  <- suppressMessages(
      initial_access_surface(tmat, n_fac_limit = 6, force_threshold = 300,
                             sparse = FALSE)
    )
    dat <<- suppressMessages(
      prepare_data(prob_mat_init = pmat, pop_raster = pop, location_data = locs,
                   mesh.args = list(cutoff = 0.1, max.edge = c(0.2, 4)))
    )
    dat
  }
})

# Cached Poisson (default) fit.
.c_mod_pois <- local({
  mod <- NULL
  function() {
    if (!is.null(mod)) return(mod)
    skip_on_cran(); skip_if_not_installed("INLA")
    mod <<- suppressMessages(suppressWarnings(
      catchment_model(.c_dat(), family = "poisson", time = FALSE)
    ))
    mod
  }
})

# Cached NB fit (same dat, different family).
.c_mod_nb <- local({
  mod <- NULL
  function() {
    if (!is.null(mod)) return(mod)
    skip_on_cran(); skip_if_not_installed("INLA")
    mod <<- suppressMessages(suppressWarnings(
      catchment_model(.c_dat(), family = "nb", time = FALSE)
    ))
    mod
  }
})

# ---- golden master: Poisson / no covariates unchanged -----------------------

test_that("Poisson family with no covariates matches golden master", {
  skip_on_cran(); skip_if_not_installed("INLA")
  snap_path <- testthat::test_path("fixtures/golden-master.rds")
  skip_if(!file.exists(snap_path), "Golden master fixture not found")
  golden <- readRDS(snap_path)

  mod <- .c_mod_pois()
  rep <- mod$obj$report()

  expect_equal(rep$case_hf, golden$case_hf, tolerance = 1e-3,
               label = "case_hf vs golden master")
  expect_equal(rep$pop_hf,  golden$pop_hf,  tolerance = 1e-3,
               label = "pop_hf vs golden master")
  expect_equal(catchment_populations(mod), golden$catchment_populations,
               tolerance = 1e-3, label = "catchment_populations vs golden master")
})

# ---- family argument ---------------------------------------------------------

test_that("catchment_model family arg is stored in output", {
  skip_on_cran(); skip_if_not_installed("INLA")
  expect_equal(.c_mod_pois()$family, "poisson")
  expect_equal(.c_mod_nb()$family,   "nb")
})

test_that("invalid family arg raises error", {
  fake_dat <- structure(list(), class = "catchment_data")
  expect_error(catchment_model(fake_dat, family = "gamma"),
               "poisson.*nb|nb.*poisson", ignore.case = TRUE)
})

# ---- NB model ----------------------------------------------------------------

test_that("NB model is a valid catchment_fit", {
  skip_on_cran(); skip_if_not_installed("INLA")
  mod_nb <- .c_mod_nb()
  expect_s3_class(mod_nb, "catchment_fit")
  expect_equal(mod_nb$family, "nb")
})

test_that("NB model produces finite log_nb_phi estimate", {
  skip_on_cran(); skip_if_not_installed("INLA")
  par_names <- names(.c_mod_nb()$fit$par)
  expect_true("log_nb_phi" %in% par_names)
  expect_true(is.finite(.c_mod_nb()$fit$par["log_nb_phi"]))
})

test_that("Poisson model does NOT have log_nb_phi as a free parameter", {
  skip_on_cran(); skip_if_not_installed("INLA")
  expect_false("log_nb_phi" %in% names(.c_mod_pois()$fit$par))
})

test_that("NB catchment_populations close to Poisson (same data)", {
  skip_on_cran(); skip_if_not_installed("INLA")
  pop_p  <- catchment_populations(.c_mod_pois())
  pop_nb <- catchment_populations(.c_mod_nb())
  rel_dev <- max(abs(pop_p - pop_nb) / (pop_p + 1))
  expect_lt(rel_dev, 0.05)
})

test_that("NB sdreport produces finite SEs", {
  skip_on_cran(); skip_if_not_installed("INLA")
  sdr_rep   <- summary(.c_mod_nb()$sdr, "report")
  pop_rows  <- rownames(sdr_rep) == "pop_hf"
  expect_true(all(is.finite(sdr_rep[pop_rows, "Std. Error"])))
})

test_that("print.catchment_fit shows NB family", {
  skip_on_cran(); skip_if_not_installed("INLA")
  out <- capture.output(print(.c_mod_nb()))
  expect_true(any(grepl("nb", out)))
})

# ---- pixel covariates: prepare_data ------------------------------------------

test_that("prepare_data with NULL pixel_covariates gives zero-column X_pixel", {
  skip_if_not_installed("fmesher")
  pop  <- make_test_raster(); locs <- make_test_points(4)
  pmat <- matrix(runif(4 * 10), nrow = 10, ncol = 4)
  dat  <- suppressMessages(
    prepare_data(prob_mat_init = pmat, pop_raster = pop, location_data = locs,
                 pixel_covariates = NULL)
  )
  expect_equal(ncol(dat$X_pixel), 0L)
})

test_that("prepare_data extracts pixel covariates from a SpatRaster", {
  skip_if_not_installed("fmesher")
  pop  <- make_test_raster(); locs <- make_test_points(4)
  pmat <- matrix(runif(4 * 10), nrow = 10, ncol = 4)
  dat  <- suppressMessages(
    prepare_data(prob_mat_init = pmat, pop_raster = pop, location_data = locs,
                 pixel_covariates = make_test_raster(seed = 42))
  )
  expect_equal(nrow(dat$X_pixel), length(dat$pop_vec))
  expect_equal(ncol(dat$X_pixel), 1L)
  expect_true(all(is.finite(dat$X_pixel)))
})

test_that("prepare_data accepts a pre-built pixel covariate matrix", {
  skip_if_not_installed("fmesher")
  pop  <- make_test_raster(); locs <- make_test_points(4)
  pmat <- matrix(runif(4 * 10), nrow = 10, ncol = 4)
  n_v  <- sum(!is.na(terra::values(pop, mat = FALSE)) &
                terra::values(pop, mat = FALSE) > 0)
  X    <- matrix(rnorm(n_v * 2), n_v, 2,
                 dimnames = list(NULL, c("x1", "x2")))
  dat  <- suppressMessages(
    prepare_data(prob_mat_init = pmat, pop_raster = pop, location_data = locs,
                 pixel_covariates = X)
  )
  expect_equal(ncol(dat$X_pixel), 2L)
  expect_equal(colnames(dat$X_pixel), c("x1", "x2"))
})

test_that("prepare_data errors on pixel covariate row mismatch", {
  skip_if_not_installed("fmesher")
  pop  <- make_test_raster(); locs <- make_test_points(4)
  pmat <- matrix(runif(4 * 10), nrow = 10, ncol = 4)
  expect_error(
    suppressMessages(
      prepare_data(prob_mat_init = pmat, pop_raster = pop, location_data = locs,
                   pixel_covariates = matrix(rnorm(5 * 2), 5, 2))
    ), "valid pixels"
  )
})

# ---- facility covariates: prepare_data ---------------------------------------

test_that("prepare_data with NULL facility_covariates gives zero-column Z_hf", {
  skip_if_not_installed("fmesher")
  pop  <- make_test_raster(); locs <- make_test_points(4)
  pmat <- matrix(runif(4 * 10), nrow = 10, ncol = 4)
  dat  <- suppressMessages(
    prepare_data(prob_mat_init = pmat, pop_raster = pop, location_data = locs)
  )
  expect_equal(ncol(dat$Z_hf), 0L)
})

test_that("prepare_data stores facility covariates correctly", {
  skip_if_not_installed("fmesher")
  pop  <- make_test_raster(); locs <- make_test_points(4)
  pmat <- matrix(runif(4 * 10), nrow = 10, ncol = 4)
  Z    <- data.frame(beds = c(10, 20, 5, 15), type = c(1, 2, 1, 2))
  dat  <- suppressMessages(
    prepare_data(prob_mat_init = pmat, pop_raster = pop, location_data = locs,
                 facility_covariates = Z)
  )
  expect_equal(nrow(dat$Z_hf), 4L); expect_equal(ncol(dat$Z_hf), 2L)
  expect_equal(colnames(dat$Z_hf), c("beds", "type"))
})

test_that("prepare_data errors on facility covariate row mismatch", {
  skip_if_not_installed("fmesher")
  pop  <- make_test_raster(); locs <- make_test_points(4)
  pmat <- matrix(runif(4 * 10), nrow = 10, ncol = 4)
  expect_error(
    suppressMessages(
      prepare_data(prob_mat_init = pmat, pop_raster = pop, location_data = locs,
                   facility_covariates = data.frame(beds = 1:3))
    ), "facilities"
  )
})

# ---- model with pixel covariate: golden master invariance -------------------

test_that("all-zero pixel covariate reproduces golden master", {
  skip_on_cran(); skip_if_not_installed("INLA")
  snap_path <- testthat::test_path("fixtures/golden-master.rds")
  skip_if(!file.exists(snap_path), "Golden master fixture not found")
  golden <- readRDS(snap_path)

  # Attach an all-zero pixel covariate to the cached dat
  base_dat <- .c_dat()
  n_v <- length(base_dat$pop_vec)
  base_dat$X_pixel <- matrix(0.0, nrow = n_v, ncol = 1,
                              dimnames = list(NULL, "zero_cov"))

  mod <- suppressMessages(suppressWarnings(
    catchment_model(base_dat, family = "poisson", time = FALSE)
  ))

  rep   <- mod$obj$report()
  cpops <- catchment_populations(mod)

  expect_equal(rep$pop_hf, golden$pop_hf, tolerance = 1e-3,
               label = "pop_hf with zero pixel cov")
  expect_equal(cpops, golden$catchment_populations, tolerance = 1e-3,
               label = "catchment_populations with zero pixel cov")
})

# ---- model with facility covariate -------------------------------------------

test_that("model with facility covariate fits and gamma is estimated", {
  skip_on_cran(); skip_if_not_installed("INLA")

  # Attach a facility covariate to the cached dat (bypass prepare_data pipeline)
  base_dat <- .c_dat()
  n_hf <- length(base_dat$loc_labels)
  base_dat$Z_hf <- matrix(scale(seq_len(n_hf)), nrow = n_hf, ncol = 1,
                           dimnames = list(NULL, "size"))

  mod <- suppressMessages(suppressWarnings(
    catchment_model(base_dat, family = "poisson", time = FALSE)
  ))

  expect_s3_class(mod, "catchment_fit")
  par_names <- names(mod$fit$par)
  expect_true("gamma" %in% par_names)
  expect_true(is.finite(mod$fit$par["gamma"]))

  # Population should still partition correctly
  cpops <- catchment_populations(mod)
  expect_equal(sum(cpops), sum(base_dat$pop_vec), tolerance = 1e-6)
})

# ---- make_model_object passes beta/gamma when present -----------------------

test_that("make_model_object beta/gamma absent from par when no covariates", {
  skip_on_cran(); skip_if_not_installed("INLA")
  mod <- .c_mod_pois()
  # beta and gamma should NOT be in the outer optimisation par (length 0)
  expect_false("beta"  %in% names(mod$fit$par))
  expect_false("gamma" %in% names(mod$fit$par))
})
