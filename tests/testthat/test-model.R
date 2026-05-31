test_that("the full model pipeline runs and conserves population", {
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
    create_travel_surface(friction_surface = fric, extent_file = pop,
                          points = locs, id_col = "label", x_col = "x", y_col = "y",
                          output_dir = dir, individual_surfaces = TRUE,
                          overwrite = TRUE)
  )

  tmat <- travel_mat_from_folder(dir = dir, reference = pop, progress = FALSE)
  pmat <- suppressMessages(
    initial_access_surface(tmat, n_fac_limit = 6, force_threshold = 300,
                           sparse = FALSE)
  )
  catch_dat <- suppressMessages(
    prepare_data(prob_mat_init = pmat, pop_raster = pop, location_data = locs,
                 mesh.args = list(cutoff = 0.1, max.edge = c(0.2, 4)))
  )

  # The dat$weights fix: there must be one weight per facility.
  expect_length(catch_dat$weights, nrow(locs))

  mod <- suppressMessages(catchment_model(catch_dat, time = FALSE))

  # log_hf_mass random effect must have one entry per facility (was empty
  # before the dat$weights fix).
  op <- mod$obj$env$last.par.best
  expect_equal(sum(names(op) == "log_hf_mass"), nrow(locs))

  cpops <- catchment_populations(mod)
  expect_length(cpops, nrow(locs))
  expect_named(cpops, as.character(locs$label))

  # Catchment populations partition the total population.
  expect_equal(sum(cpops), sum(catch_dat$pop_vec), tolerance = 1e-6)

  pr <- get_prob_raster(mod, id_label = locs$label[1])
  expect_s4_class(pr, "SpatRaster")
  rng <- range(terra::values(pr), na.rm = TRUE)
  expect_gte(rng[1], 0)
  expect_lte(rng[2], 1)
})
