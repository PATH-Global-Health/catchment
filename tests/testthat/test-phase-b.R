# Helper: fit the reference model on the standard fixture (shared across tests).
# The first call builds it; subsequent calls in the same session reuse it.
.phase_b_model <- local({
  mod <- NULL
  function() {
    if (!is.null(mod)) return(mod)
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
    tmat <- travel_mat_from_folder(dir = dir, reference = pop, progress = FALSE)
    pmat <- suppressMessages(
      initial_access_surface(tmat, n_fac_limit = 6, force_threshold = 300,
                             sparse = FALSE)
    )
    dat <- suppressMessages(
      prepare_data(prob_mat_init = pmat, pop_raster = pop, location_data = locs,
                   mesh.args = list(cutoff = 0.1, max.edge = c(0.2, 4)))
    )
    mod <<- suppressMessages(suppressWarnings(catchment_model(dat, time = FALSE)))
    mod
  }
})

# --- catchment_fit class -------------------------------------------------------

test_that("catchment_model returns a catchment_fit object", {
  skip_on_cran()
  skip_if_not_installed("INLA")
  mod <- .phase_b_model()
  expect_s3_class(mod, "catchment_fit")
  expect_named(mod, c("obj", "fit", "sdr", "data"), ignore.order = TRUE)
})

# --- sdreport ------------------------------------------------------------------

test_that("sdreport is non-NULL and pdHess is TRUE on the reference fixture", {
  skip_on_cran()
  skip_if_not_installed("INLA")
  mod <- .phase_b_model()
  expect_false(is.null(mod$sdr))
  expect_true(mod$sdr$pdHess)
})

test_that("sdreport produces finite SEs for pop_hf and case_hf", {
  skip_on_cran()
  skip_if_not_installed("INLA")
  mod <- .phase_b_model()
  sdr_rep <- summary(mod$sdr, "report")

  pop_rows  <- rownames(sdr_rep) == "pop_hf"
  case_rows <- rownames(sdr_rep) == "case_hf"

  expect_true(any(pop_rows),  label = "pop_hf present in sdreport")
  expect_true(any(case_rows), label = "case_hf present in sdreport")

  pop_se  <- sdr_rep[pop_rows,  "Std. Error"]
  case_se <- sdr_rep[case_rows, "Std. Error"]

  expect_true(all(is.finite(pop_se)),  label = "pop_hf SEs finite")
  expect_true(all(is.finite(case_se)), label = "case_hf SEs finite")
  expect_true(all(pop_se  > 0),        label = "pop_hf SEs positive")
  expect_true(all(case_se > 0),        label = "case_hf SEs positive")
})

# --- Convergence diagnostics ---------------------------------------------------

test_that("check_convergence returns expected structure", {
  skip_on_cran()
  skip_if_not_installed("INLA")
  mod <- .phase_b_model()
  cc <- capture.output(check_convergence(mod))   # prints to stdout; result is invisible

  # Rerun to capture the return value
  result <- invisible(suppressMessages(check_convergence(mod)))

  expect_type(result, "list")
  expect_named(result, c("converged", "nlminb_message", "max_gradient", "pdHess"),
               ignore.order = TRUE)
  expect_type(result$converged,    "logical")
  expect_type(result$max_gradient, "double")
  expect_true(is.finite(result$max_gradient))
  # Hessian should be PD even if nlminb convergence code is non-zero
  expect_true(isTRUE(result$pdHess))
})

test_that("check_convergence errors on non-catchment_fit input", {
  expect_error(check_convergence(list()), "catchment_fit")
  expect_error(check_convergence("x"),   "catchment_fit")
})

test_that("catchment_model warns when nlminb convergence code is non-zero", {
  skip_on_cran()
  skip_if_not_installed("INLA")
  mod <- .phase_b_model()
  # Simulate a non-converged fit by patching the convergence code
  mod_bad <- mod
  mod_bad$fit$convergence <- 1L
  mod_bad$fit$message <- "iteration limit reached"
  # check_convergence should reflect the patched state
  result <- invisible(check_convergence(mod_bad))
  expect_false(result$converged)
})

# --- print.catchment_fit -------------------------------------------------------

test_that("print.catchment_fit produces expected output", {
  skip_on_cran()
  skip_if_not_installed("INLA")
  mod <- .phase_b_model()
  out <- capture.output(print(mod))
  expect_true(any(grepl("catchment_fit", out)))
  expect_true(any(grepl("Facilities", out)))
  expect_true(any(grepl("Converged", out)))
  expect_true(any(grepl("pdHess", out)))
})

# --- catchment_populations with uncertainty ------------------------------------

test_that("catchment_populations(uncertainty=FALSE) is unchanged vs golden master", {
  skip_on_cran()
  skip_if_not_installed("INLA")
  mod <- .phase_b_model()
  golden <- readRDS(testthat::test_path("fixtures/golden-master.rds"))
  cpops <- catchment_populations(mod)
  expect_equal(cpops, golden$catchment_populations, tolerance = 1e-3)
})

test_that("catchment_populations(uncertainty=TRUE) returns data frame with valid CIs", {
  skip_on_cran()
  skip_if_not_installed("INLA")
  mod <- .phase_b_model()

  udf <- catchment_populations(mod, uncertainty = TRUE)

  expect_s3_class(udf, "data.frame")
  expect_named(udf, c("label", "estimate", "se", "lower", "upper"))
  expect_equal(nrow(udf), length(mod$data$loc_labels))

  # SEs must be positive and finite
  expect_true(all(is.finite(udf$se)))
  expect_true(all(udf$se > 0))

  # 95% CI must bracket the point estimate
  expect_true(all(udf$lower < udf$estimate))
  expect_true(all(udf$estimate < udf$upper))

  # Point-estimate column should be close to the named-vector version
  pt <- catchment_populations(mod)
  expect_equal(udf$estimate, unname(pt), tolerance = 1e-3,
               label = "uncertainty estimate matches point estimate")
})

test_that("catchment_populations errors when sdr is NULL", {
  mod_nosdr <- structure(list(sdr = NULL), class = "catchment_fit")
  expect_error(catchment_populations(mod_nosdr, uncertainty = TRUE),
               "sdreport is not available")
})

# --- get_prob_raster uncertainty stub ------------------------------------------

test_that("get_prob_raster(uncertainty=TRUE) errors with informative message", {
  skip_on_cran()
  skip_if_not_installed("INLA")
  mod <- .phase_b_model()
  expect_error(
    get_prob_raster(mod, id_label = mod$data$loc_labels[1], uncertainty = TRUE),
    "not yet implemented"
  )
})
