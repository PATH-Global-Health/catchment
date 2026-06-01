test_that("model output matches golden master (regression guard)", {
  skip_on_cran()
  skip_if_not_installed("INLA")

  snap_path <- testthat::test_path("fixtures/golden-master.rds")
  skip_if(!file.exists(snap_path), "Golden master fixture not found")
  golden <- readRDS(snap_path)

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
  mod <- suppressMessages(catchment_model(catch_dat, time = FALSE))

  rep <- mod$obj$report()
  expect_equal(rep$case_hf, golden$case_hf, tolerance = 1e-5,
               label = "case_hf vs golden master")
  expect_equal(rep$pop_hf, golden$pop_hf, tolerance = 1e-5,
               label = "pop_hf vs golden master")
  cpops <- catchment_populations(mod)
  expect_equal(cpops, golden$catchment_populations, tolerance = 1e-5,
               label = "catchment_populations vs golden master")
})
