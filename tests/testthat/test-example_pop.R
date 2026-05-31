test_that("example_pop() returns a WGS84 SpatRaster", {
  pop <- example_pop()
  expect_s4_class(pop, "SpatRaster")
  expect_match(terra::crs(pop, describe = TRUE)$code, "4326")
})

test_that("example_shp is an sf polygon and example_locs has expected columns", {
  data("example_shp", package = "catchment")
  data("example_locs", package = "catchment")
  expect_s3_class(example_shp, "sf")
  expect_true(all(c("label", "x", "y", "weight") %in% names(example_locs)))
})
