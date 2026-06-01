test_that("prepare_data catches bad pop_raster type", {
  skip_if_not_installed("fmesher")
  locs <- make_test_points()
  pmat <- matrix(runif(4 * 10), nrow = 10, ncol = 4)  # dense [n_pixel x n_hf]
  expect_error(
    prepare_data(prob_mat_init = pmat, pop_raster = "not_a_raster",
                 location_data = locs),
    "SpatRaster"
  )
})

test_that("prepare_data catches non-data-frame location_data", {
  skip_if_not_installed("fmesher")
  pop <- make_test_raster()
  pmat <- matrix(runif(4 * 10), nrow = 10, ncol = 4)
  expect_error(
    prepare_data(prob_mat_init = pmat, pop_raster = pop,
                 location_data = list(label = "a")),
    "data frame"
  )
})

test_that("prepare_data catches missing column names", {
  skip_if_not_installed("fmesher")
  pop <- make_test_raster()
  locs <- make_test_points()
  pmat <- matrix(runif(4 * 10), nrow = 10, ncol = 4)
  expect_error(
    prepare_data(prob_mat_init = pmat, pop_raster = pop,
                 location_data = locs, id_col = "nonexistent"),
    "not found"
  )
})

test_that("prepare_data catches duplicate id labels", {
  skip_if_not_installed("fmesher")
  pop <- make_test_raster()
  locs <- make_test_points(4)
  locs$label[2] <- locs$label[1]  # introduce duplicate
  pmat <- matrix(runif(4 * 10), nrow = 10, ncol = 4)
  expect_error(
    prepare_data(prob_mat_init = pmat, pop_raster = pop,
                 location_data = locs),
    "unique"
  )
})

test_that("prepare_data catches prob_mat_init dimension mismatch (sparse)", {
  skip_if_not_installed("fmesher")
  pop <- make_test_raster()
  locs <- make_test_points(4)
  # sparse Matrix with wrong number of rows (should be n_hf=4)
  wrong_sparse <- Matrix::Matrix(matrix(runif(3 * 10), nrow = 3), sparse = TRUE)
  expect_error(
    prepare_data(prob_mat_init = wrong_sparse, pop_raster = pop,
                 location_data = locs),
    "4 facilities"
  )
})

test_that("prepare_data catches prob_mat_init dimension mismatch (dense)", {
  skip_if_not_installed("fmesher")
  pop <- make_test_raster()
  locs <- make_test_points(4)
  # dense matrix with wrong number of columns (should be n_hf=4)
  wrong_dense <- matrix(runif(10 * 3), nrow = 10, ncol = 3)
  expect_error(
    prepare_data(prob_mat_init = wrong_dense, pop_raster = pop,
                 location_data = locs),
    "4 facilities"
  )
})

test_that("catchment_model rejects non-catchment_data input", {
  expect_error(catchment_model(list()), "catchment_data")
  expect_error(catchment_model("bad"), "catchment_data")
})

test_that("print.catchment_data produces output without error", {
  skip_on_cran()
  skip_if_not_installed("INLA")
  skip_if_not_installed("fmesher")

  pop <- make_test_raster()
  locs <- make_test_points(4)
  fric <- make_test_raster(seed = 99)

  dir <- withr::local_tempdir()
  suppressMessages(
    create_travel_surface(friction_surface = fric, extent_file = pop,
                          points = locs, id_col = "label", x_col = "x", y_col = "y",
                          output_dir = dir, individual_surfaces = TRUE,
                          overwrite = TRUE)
  )
  tmat <- travel_mat_from_folder(dir = dir, reference = pop, progress = FALSE)
  pmat <- suppressMessages(initial_access_surface(tmat, sparse = FALSE))
  dat <- suppressMessages(
    prepare_data(prob_mat_init = pmat, pop_raster = pop, location_data = locs)
  )

  expect_output(print(dat), "catchment_data")
  expect_output(print(dat), "Facilities")
  out <- capture.output(summary(dat))
  expect_true(any(grepl("Total population", out)))
})
