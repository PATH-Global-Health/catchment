# ---- hf_mass_sd: how far the counts may reshape the allocation ---------------
# At the default (0.1) facility mass is effectively pinned near 1 and catchment
# populations are decided by travel geometry alone. A looser prior must let the
# observed counts move mass -- and therefore population -- between facilities.

test_that("hf_mass_sd widens the fitted mass range and shifts catchments", {
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

  mass_of <- function(mod) {
    op <- mod$obj$env$last.par.best
    exp(unname(op[names(op) == "log_hf_mass"]))
  }

  tight <- suppressMessages(catchment_model(catch_dat, time = FALSE))
  loose <- suppressMessages(catchment_model(catch_dat, hf_mass_sd = 0.5,
                                            time = FALSE))

  # The prior is the only thing holding mass at 1, so loosening it must let the
  # counts spread the masses further apart.
  expect_gt(diff(range(mass_of(loose))), diff(range(mass_of(tight))))

  # ...and that must actually move population between facilities.
  p_tight <- catchment_populations(tight)
  p_loose <- catchment_populations(loose)
  expect_gt(max(abs(p_loose - p_tight)), 0)

  # Both remain a partition of the total population: mass reallocates people,
  # it never creates them. This is why a catchment "floor" is not a parameter.
  expect_equal(sum(p_tight), sum(catch_dat$pop_vec), tolerance = 1e-6)
  expect_equal(sum(p_loose), sum(catch_dat$pop_vec), tolerance = 1e-6)

  expect_error(catchment_model(catch_dat, hf_mass_sd = 0), "positive")
})
