# Regression: travel_mat_from_folder() returns columns in file-name order, which
# need not be the row order of location_data. prepare_data() must match by id.

# Facility ids deliberately NOT in alphabetical order.
.order_fixture <- function(env = parent.frame()) {
  locs <- make_test_points(5)
  locs$label <- c("delta", "alpha", "echo", "charlie", "bravo")
  fric <- make_test_raster(seed = 99)
  pop  <- make_test_raster(vmin = 1, vmax = 100, seed = 7)

  dir <- withr::local_tempdir(.local_envir = env)
  suppressMessages(
    create_travel_surface(friction_surface = fric, extent_file = pop,
                          points = locs, id_col = "label", x_col = "x",
                          y_col = "y", output_dir = dir,
                          individual_surfaces = TRUE, overwrite = TRUE)
  )
  tmat <- travel_mat_from_folder(dir = dir, reference = pop, progress = FALSE)
  list(locs = locs, pop = pop, tmat = tmat, dir = dir)
}

.prep <- function(...) suppressMessages(prepare_data(...))

# Reorder a catchment_data's facility-indexed pieces to a given id order.
.by_id <- function(dat, ids) {
  i <- match(ids, dat$loc_labels)
  list(labels = dat$loc_labels[i], weights = dat$weights[i],
       coords = unname(as.matrix(dat$loc_coords[i, ])),
       Z = dat$Z_hf[i, , drop = FALSE],
       prob = as.matrix(dat$prob_mat_init)[, i],
       travel = as.matrix(dat$travel_sparse)[i, ])
}

test_that("travel_mat_from_folder names columns after the .tif files", {
  fx <- .order_fixture()
  expect_s3_class(fx$tmat, "travel_mat")
  expect_equal(colnames(fx$tmat), sort(fx$locs$label, method = "radix"))
  # each named column really is that file's surface
  valid <- which(terra::values(fx$pop, mat = FALSE) > 0)
  for (id in fx$locs$label) {
    r <- terra::rast(file.path(fx$dir, paste0(id, ".tif")))
    expect_equal(unname(fx$tmat[, id]), terra::values(r, mat = FALSE)[valid])
  }
})

test_that("prepare_data matches travel columns to location_data by id", {
  fx   <- .order_fixture()
  locs <- fx$locs
  expect_false(identical(locs$label, colnames(fx$tmat)))   # the bug's setup

  dat <- .prep(fx$tmat, fx$pop, locs, n_fac_limit = 3,
               facility_covariates = data.frame(z = seq_len(nrow(locs))))

  # labels, weights and names stay in location_data order ...
  expect_equal(dat$loc_labels, locs$label)
  expect_equal(dat$weights, locs$weight)
  expect_equal(colnames(dat$prob_mat_init), locs$label)
  expect_equal(rownames(dat$travel_sparse), locs$label)
  expect_equal(dim(dat$travel_sparse), c(nrow(locs), length(dat$pop_vec)))

  # ... and each facility is paired with its own surface: every kept entry of
  # its row is that facility's own .tif, clamped at minimum_time.
  valid <- which(terra::values(fx$pop, mat = FALSE) > 0)
  tt <- as.matrix(dat$travel_sparse)
  for (id in locs$label) {
    own  <- terra::values(terra::rast(file.path(fx$dir, paste0(id, ".tif"))),
                          mat = FALSE)[valid]
    kept <- tt[id, ] != 0
    expect_true(any(kept))
    expect_equal(unname(tt[id, kept]), pmax(own, 10)[kept])
  }
  # the surfaces differ enough that a mis-pairing could not pass the check above
  expect_false(isTRUE(all.equal(unname(tt[1, ]), unname(tt[2, ]))))

  # Identical to the alphabetically ordered case after matching by id.
  locs_sorted <- locs[order(locs$label), ]
  dat_sorted  <- .prep(fx$tmat, fx$pop, locs_sorted, n_fac_limit = 3,
                       facility_covariates =
                         data.frame(z = seq_len(nrow(locs))[order(locs$label)]))
  expect_equal(colnames(dat_sorted$prob_mat_init), sort(locs$label))
  a <- .by_id(dat, locs_sorted$label)
  b <- .by_id(dat_sorted, locs_sorted$label)
  # prob is row-normalised, so summation order leaves ~1e-17 float noise
  expect_equal(a$prob, b$prob, tolerance = 1e-12)
  a$prob <- b$prob <- NULL
  expect_identical(a, b)
})

test_that("pre-built probability matrices are matched by id (dense and sparse)", {
  fx   <- .order_fixture()
  locs <- fx$locs
  ref  <- .prep(fx$tmat, fx$pop, locs)$prob_mat_init

  dense <- suppressMessages(initial_access_surface(fx$tmat, sparse = FALSE))
  expect_equal(.prep(dense, fx$pop, locs)$prob_mat_init, ref)

  sp  <- suppressMessages(initial_access_surface(fx$tmat, sparse = TRUE))
  expect_equal(rownames(sp), colnames(fx$tmat))            # facility-by-pixel
  out <- .prep(sp, fx$pop, locs)$prob_mat_init
  expect_equal(rownames(out), locs$label)
  expect_equal(t(as.matrix(out)), ref, ignore_attr = TRUE)
})

test_that("prepare_data errors on missing or unmatched facility ids", {
  fx <- .order_fixture()

  renamed <- fx$locs
  renamed$label[1] <- "zulu"
  expect_error(.prep(fx$tmat, fx$pop, renamed),
               "id\\(s\\) with no matching column: zulu")
  expect_error(.prep(fx$tmat, fx$pop, renamed),
               "column\\(s\\) with no matching id: delta")

  # a facility dropped from location_data leaves an unmatched surface
  expect_error(.prep(fx$tmat, fx$pop, fx$locs[-2, ]),
               "column\\(s\\) with no matching id: alpha")
  # a surface missing from the folder leaves an unmatched id
  short <- fx$tmat[, -1]
  class(short) <- class(fx$tmat)
  expect_error(.prep(short, fx$pop, fx$locs),
               "id\\(s\\) with no matching column: alpha")
})

test_that("prepare_data warns when facility order cannot be verified", {
  fx <- .order_fixture()
  bare <- fx$tmat
  colnames(bare) <- NULL
  expect_warning(.prep(bare, fx$pop, fx$locs), "cannot be verified")
  expect_no_warning(.prep(fx$tmat, fx$pop, fx$locs))
})
