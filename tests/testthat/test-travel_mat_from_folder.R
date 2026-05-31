test_that("travel matrix has one column per raster and correct class", {
  ref <- make_test_raster()
  # population reference: some cells zero/NA, rest positive
  pop <- ref
  terra::values(pop) <- rep(c(0, 5, 10, NA), length.out = terra::ncell(pop))

  dir <- withr::local_tempdir()
  for (nm in c("a", "b", "c")) {
    r <- make_test_raster(seed = nchar(nm) + utf8ToInt(nm))
    terra::writeRaster(r, file.path(dir, paste0(nm, ".tif")))
  }

  tmat <- travel_mat_from_folder(dir = dir, reference = pop, progress = FALSE)

  n_valid <- sum(!is.na(terra::values(pop, mat = FALSE)) &
                   terra::values(pop, mat = FALSE) > 0)
  expect_equal(dim(tmat), c(n_valid, 3))
  expect_s3_class(tmat, "travel_mat")
})

test_that("sparse = FALSE keeps every pixel", {
  ref <- make_test_raster()
  pop <- ref
  terra::values(pop) <- rep(c(0, 5), length.out = terra::ncell(pop))

  dir <- withr::local_tempdir()
  terra::writeRaster(make_test_raster(seed = 9), file.path(dir, "a.tif"))

  tmat <- suppressWarnings(
    travel_mat_from_folder(dir = dir, reference = pop, sparse = FALSE,
                           progress = FALSE)
  )
  expect_equal(nrow(tmat), terra::ncell(pop))
})
