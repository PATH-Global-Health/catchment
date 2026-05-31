# Small synthetic friction/population raster on a WGS84 grid for fast tests.
make_test_raster <- function(nrow = 20, ncol = 20, vmin = 0.001, vmax = 0.02,
                             seed = 1) {
  r <- terra::rast(nrows = nrow, ncols = ncol,
                   xmin = 30, xmax = 30.4, ymin = -1, ymax = -0.6,
                   crs = "EPSG:4326")
  set.seed(seed)
  terra::values(r) <- stats::runif(terra::ncell(r), vmin, vmax)
  r
}

# A handful of points that fall inside make_test_raster()'s extent.
make_test_points <- function(n = 4) {
  set.seed(2)
  data.frame(
    label = paste0("hf", seq_len(n)),
    x = stats::runif(n, 30.05, 30.35),
    y = stats::runif(n, -0.95, -0.65),
    weight = stats::runif(n, 5, 50)
  )
}
