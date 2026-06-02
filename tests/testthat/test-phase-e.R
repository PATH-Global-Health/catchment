# ---- Phase E: validation tooling & plot methods ------------------------------
# Shared fixture: a fitted model on the same raw-travel data used in Phase D.
# Fit once (Poisson, learned exponential decay) and reuse across tests.

.e_mod <- local({
  mod <- NULL
  function() {
    if (!is.null(mod)) return(mod)
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

    dat <- suppressMessages(
      prepare_data(prob_mat_init = tmat, pop_raster = pop, location_data = locs,
                   n_fac_limit = 6, force_threshold = 300,
                   mesh.args = list(cutoff = 0.1, max.edge = c(0.2, 4)))
    )
    mod <<- suppressMessages(suppressWarnings(
      catchment_model(dat, family = "poisson", decay = "exponential",
                      time = FALSE)
    ))
    mod
  }
})

# ---- posterior_predict -------------------------------------------------------

test_that("posterior_predict returns a labelled count matrix", {
  skip_on_cran(); skip_if_not_installed("INLA")
  mod  <- .e_mod()
  yrep <- posterior_predict(mod, nsim = 50, seed = 1)

  expect_true(is.matrix(yrep))
  expect_equal(nrow(yrep), 50)
  expect_equal(ncol(yrep), length(mod$data$loc_labels))
  expect_equal(colnames(yrep), mod$data$loc_labels)
  expect_true(all(yrep >= 0))
  expect_true(all(yrep == round(yrep)))            # integer counts
})

test_that("posterior_predict is reproducible with a seed", {
  skip_on_cran(); skip_if_not_installed("INLA")
  mod <- .e_mod()
  a <- posterior_predict(mod, nsim = 30, seed = 42)
  b <- posterior_predict(mod, nsim = 30, seed = 42)
  expect_identical(a, b)
})

# ---- pp_check ----------------------------------------------------------------

test_that("pp_check reports coverage, dispersion, and a summary frame", {
  skip_on_cran(); skip_if_not_installed("INLA")
  mod <- .e_mod()
  res <- pp_check(mod, nsim = 100, seed = 1, plot = FALSE)

  expect_named(res, c("summary", "coverage", "dispersion"))
  expect_s3_class(res$summary, "data.frame")
  expect_equal(nrow(res$summary), length(mod$data$loc_labels))
  expect_true(res$coverage >= 0 && res$coverage <= 1)
  expect_true(is.finite(res$dispersion))
  # bounds ordering
  expect_true(all(res$summary$lower <= res$summary$upper))
})

test_that("pp_check attaches a ggplot when requested", {
  skip_on_cran(); skip_if_not_installed("INLA")
  skip_if_not_installed("ggplot2")
  mod <- .e_mod()
  res <- pp_check(mod, nsim = 50, seed = 1, plot = TRUE)
  expect_s3_class(res$plot, "ggplot")
})

# ---- loo_facility_cv ---------------------------------------------------------

test_that("loo_facility_cv returns per-facility predictions with error metrics", {
  skip_on_cran(); skip_if_not_installed("INLA")
  mod <- .e_mod()
  cv  <- loo_facility_cv(mod)

  n_obs <- sum(mod$data$which_not_NA == 1)
  expect_s3_class(cv, "data.frame")
  expect_named(cv, c("label", "observed", "predicted", "error"))
  expect_equal(nrow(cv), n_obs)
  expect_true(all(is.finite(cv$predicted)))
  expect_equal(cv$error, cv$predicted - cv$observed)
  expect_true(is.finite(attr(cv, "rmse")))
  expect_true(is.finite(attr(cv, "mae")))
})

# ---- dominant_catchment ------------------------------------------------------

test_that("dominant_catchment returns a categorical facility raster", {
  skip_on_cran(); skip_if_not_installed("INLA")
  mod <- .e_mod()
  r   <- dominant_catchment(mod)

  expect_s4_class(r, "SpatRaster")
  expect_equal(names(r), "dominant_facility")

  vals  <- terra::values(r, mat = FALSE)
  ids   <- vals[!is.na(vals)]
  n_hf  <- length(mod$data$loc_labels)
  expect_true(all(ids >= 1 & ids <= n_hf))
  # valid pixels equal the model's pop pixels
  expect_equal(length(ids), length(mod$data$pop_vec))
})

# ---- plot methods (ggplot path) ----------------------------------------------

test_that("plot.catchment_fit produces a ggplot when tidyterra is available", {
  skip_on_cran(); skip_if_not_installed("INLA")
  skip_if_not_installed("ggplot2"); skip_if_not_installed("tidyterra")
  mod <- .e_mod()
  p   <- plot(mod)
  expect_s3_class(p, "ggplot")
})

test_that("plot_prob_surface produces a ggplot for a chosen facility", {
  skip_on_cran(); skip_if_not_installed("INLA")
  skip_if_not_installed("ggplot2"); skip_if_not_installed("tidyterra")
  mod <- .e_mod()
  lab <- mod$data$loc_labels[1]
  p   <- plot_prob_surface(mod, lab)
  expect_s3_class(p, "ggplot")
})
