test_that("normalized rows sum to 1", {
  tmat <- matrix(c(10, 20, 30, 40, 50, 60), nrow = 2, byrow = TRUE)
  class(tmat) <- c("travel_mat", class(tmat))
  pmat <- initial_access_surface(tmat, minimum_time = 5, force_threshold = NA,
                                 sparse = FALSE)
  expect_equal(unname(rowSums(pmat)), c(1, 1))
})

test_that("inverse distance squared down-weights farther facilities", {
  tmat <- matrix(c(10, 100), nrow = 1)
  class(tmat) <- c("travel_mat", class(tmat))
  pmat <- initial_access_surface(tmat, minimum_time = 5, force_threshold = NA,
                                 sparse = FALSE)
  expect_gt(pmat[1, 1], pmat[1, 2])
})

test_that("numeric transform applies a custom exponent", {
  tmat <- matrix(c(10, 20), nrow = 1)
  class(tmat) <- c("travel_mat", class(tmat))
  p1 <- initial_access_surface(tmat, transform = 1, minimum_time = 5,
                               force_threshold = NA, sparse = FALSE)
  # exponent 1: weights proportional to 1/t -> 20:10 reversed share
  expect_equal(unname(p1[1, ]), c((1/10) / (1/10 + 1/20), (1/20) / (1/10 + 1/20)))
})

test_that("function transform is applied", {
  tmat <- matrix(c(10, 20), nrow = 1)
  class(tmat) <- c("travel_mat", class(tmat))
  p <- initial_access_surface(tmat, transform = function(x) 1 / x,
                              minimum_time = 5, force_threshold = NA,
                              sparse = FALSE)
  expect_equal(which.max(p[1, ]), 1L)
})

test_that("force_threshold sends remote pixels to nearest facility", {
  # Both facilities beyond threshold; pixel must go entirely to the nearest.
  tmat <- matrix(c(400, 500), nrow = 1)
  class(tmat) <- c("travel_mat", class(tmat))
  pmat <- initial_access_surface(tmat, minimum_time = 5, force_threshold = 300,
                                 sparse = FALSE)
  expect_equal(unname(pmat[1, ]), c(1, 0))
})

test_that("n_fac_limit keeps only the nearest k facilities", {
  tmat <- matrix(c(10, 20, 30, 40), nrow = 1)
  class(tmat) <- c("travel_mat", class(tmat))
  pmat <- initial_access_surface(tmat, n_fac_limit = 2, minimum_time = 5,
                                 force_threshold = NA, sparse = FALSE)
  expect_equal(sum(pmat[1, ] > 0), 2)
})

test_that("sparse output is a transposed dgCMatrix", {
  tmat <- matrix(c(10, 20, 30, 40, 50, 60), nrow = 2, byrow = TRUE)
  class(tmat) <- c("travel_mat", class(tmat))
  pmat <- initial_access_surface(tmat, minimum_time = 5, force_threshold = NA,
                                 sparse = TRUE)
  expect_s4_class(pmat, "Matrix")
  expect_equal(dim(pmat), c(3, 2))  # transposed: facilities x pixels
})

test_that("NA travel times are imputed to the maximum", {
  tmat <- matrix(c(10, NA, 30, 40), nrow = 2, byrow = TRUE)
  class(tmat) <- c("travel_mat", class(tmat))
  pmat <- initial_access_surface(tmat, minimum_time = 5, force_threshold = NA,
                                 sparse = FALSE)
  expect_false(anyNA(pmat))
})
