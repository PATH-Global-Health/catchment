test_that("points are shifted to the nearest valid coordinate", {
  valid <- data.frame(x = c(0, 10), y = c(0, 10))
  org <- data.frame(x = c(1, 9), y = c(1, 9))
  out <- shift_coord(org, valid)
  expect_equal(out$x, c(0, 10))
  expect_equal(out$y, c(0, 10))
})

test_that("a data frame input returns a data frame with the same names", {
  valid <- data.frame(lon = c(0, 10), lat = c(0, 10))
  org <- data.frame(lon = 1, lat = 1)
  out <- shift_coord(org, valid)
  expect_s3_class(out, "data.frame")
  expect_named(out, c("lon", "lat"))
})

test_that("a matrix input returns a matrix", {
  valid <- cbind(x = c(0, 10), y = c(0, 10))
  org <- cbind(x = 9, y = 9)
  out <- shift_coord(org, valid)
  expect_true(is.matrix(out))
})
