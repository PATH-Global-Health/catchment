test_that("individual surfaces are written, one per point", {
  fric <- make_test_raster()
  pts <- make_test_points(4)
  out <- withr::local_tempdir()

  suppressMessages(
    create_travel_surface(friction_surface = fric, extent_file = fric,
                          points = pts, id_col = "label", x_col = "x", y_col = "y",
                          output_dir = out, individual_surfaces = TRUE,
                          overwrite = TRUE)
  )

  tifs <- list.files(out, pattern = "\\.tif$")
  expect_length(tifs, 4)
  expect_setequal(tools::file_path_sans_ext(tifs), pts$label)

  r <- terra::rast(file.path(out, paste0(pts$label[1], ".tif")))
  expect_s4_class(r, "SpatRaster")
  expect_true(min(terra::values(r), na.rm = TRUE) >= 0)
})

test_that("single surface mode writes one file", {
  fric <- make_test_raster()
  pts <- make_test_points(4)
  out <- withr::local_tempdir()

  suppressMessages(
    create_travel_surface(friction_surface = fric, extent_file = fric,
                          points = pts, id_col = "label", x_col = "x", y_col = "y",
                          output_dir = out, individual_surfaces = FALSE,
                          overwrite = TRUE)
  )
  expect_true(file.exists(file.path(out, "HF_accessibility.tif")))
})

test_that("check_existing skips points that already have output", {
  fric <- make_test_raster()
  pts <- make_test_points(3)
  out <- withr::local_tempdir()

  suppressMessages(
    create_travel_surface(friction_surface = fric, extent_file = fric,
                          points = pts, id_col = "label", x_col = "x", y_col = "y",
                          output_dir = out, individual_surfaces = TRUE,
                          overwrite = TRUE)
  )
  mtimes <- file.info(list.files(out, full.names = TRUE))$mtime

  Sys.sleep(1)
  suppressMessages(
    create_travel_surface(friction_surface = fric, extent_file = fric,
                          points = pts, id_col = "label", x_col = "x", y_col = "y",
                          output_dir = out, individual_surfaces = TRUE,
                          check_existing = TRUE)
  )
  mtimes2 <- file.info(list.files(out, full.names = TRUE))$mtime
  expect_equal(mtimes, mtimes2)  # untouched
})

test_that("duplicate identifiers raise an error", {
  fric <- make_test_raster()
  pts <- make_test_points(3)
  pts$label[2] <- pts$label[1]
  out <- withr::local_tempdir()

  expect_error(
    suppressMessages(
      create_travel_surface(friction_surface = fric, extent_file = fric,
                            points = pts, id_col = "label", x_col = "x", y_col = "y",
                            output_dir = out, individual_surfaces = TRUE)
    ),
    "unique identifiers"
  )
})

test_that("an invalid extent_file is rejected", {
  fric <- make_test_raster()
  pts <- make_test_points(2)
  out <- withr::local_tempdir()
  expect_error(
    suppressMessages(
      create_travel_surface(friction_surface = fric, extent_file = 42,
                            points = pts, id_col = "label", x_col = "x", y_col = "y",
                            output_dir = out, individual_surfaces = TRUE)
    )
  )
})
