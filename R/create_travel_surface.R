#' Create travel time surfaces
#'
#' Computes accumulated cost-distance (travel time) surfaces from a friction
#' surface using [terra::costDist()]. Either a single surface giving the travel
#' time to the nearest point, or one surface per point, can be produced.
#'
#' @param friction_surface A [terra::SpatRaster] (or path to a raster file)
#'   containing the travel cost per unit distance (e.g. minutes per metre).
#' @param points A data frame (or path to a CSV) containing label, x, and y
#'   fields for the point features.
#' @param extent_file A [terra::SpatRaster], an [sf::sf] polygon object, or a
#'   path to a raster (`.tif`) or shapefile (`.shp`) used to define the extent
#'   to which the friction surface is cropped. If `NA`, the full friction
#'   surface extent is used.
#' @param id_col A character string naming the label column in `points`. Used to
#'   name the individual surface files. Must be unique per point.
#' @param x_col A character string naming the x coordinate column in `points`.
#' @param y_col A character string naming the y coordinate column in `points`.
#' @param output_dir A character string for the output directory.
#' @param clip_flag TRUE/FALSE: use the exact extent of `extent_file` (TRUE) or
#'   a slightly expanded extent (FALSE, the default) when cropping.
#' @param individual_surfaces TRUE/FALSE: create one surface per point (TRUE) or
#'   a single surface to the nearest point (FALSE).
#' @param check_existing TRUE/FALSE: skip points whose output file already
#'   exists in `output_dir`.
#' @param overwrite TRUE/FALSE: overwrite existing output rasters.
#' @param ... Additional arguments (currently unused).
#'
#' @importFrom terra rast costDist crop project ext vect cellFromXY writeRaster
#' @importFrom fs path
#'
#' @export
#' @return Invisibly returns the output directory. Rasters are written to
#'   `output_dir`.
create_travel_surface <- function(friction_surface,
                                  points,
                                  extent_file = NA,
                                  id_col = NA,
                                  x_col = "x",
                                  y_col = "y",
                                  clip_flag = FALSE,
                                  individual_surfaces = FALSE,
                                  output_dir = NA,
                                  check_existing = FALSE,
                                  overwrite = FALSE, ...) {

  std_crs <- "EPSG:4326"

  # Load the friction surface --------------------------------------------------
  if (is.character(friction_surface)) {
    friction <- terra::rast(friction_surface)
  } else if (inherits(friction_surface, "SpatRaster")) {
    friction <- friction_surface
  } else {
    friction <- terra::rast(friction_surface)  # coerce RasterLayer etc.
  }

  if (is.na(terra::crs(friction)) || terra::crs(friction) == "") {
    terra::crs(friction) <- std_crs
  } else if (!terra::same.crs(friction, std_crs)) {
    message("Reprojecting friction surface to ", std_crs)
    friction <- terra::project(friction, std_crs)
  }

  # Define the cropping extent -------------------------------------------------
  new_extent <- .resolve_extent(extent_file, std_crs, clip_flag)

  if (!is.null(new_extent)) {
    friction <- terra::crop(friction, new_extent)
  }

  # Load the points ------------------------------------------------------------
  if (is.character(points)) {
    points <- utils::read.csv(file = points)
  } else {
    points <- as.data.frame(points)
  }
  n_points <- nrow(points)

  coords <- as.matrix(points[, c(x_col, y_col)])
  cells <- terra::cellFromXY(friction, coords)

  if (anyNA(cells)) {
    warning(sum(is.na(cells)), " point(s) fall outside the friction surface ",
            "extent and will be skipped.")
  }

  if (!is.na(output_dir) && !dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }

  # Single surface: travel time to the nearest point ---------------------------
  if (!individual_surfaces) {
    message("Creating single accessibility surface.")
    output_filename <- fs::path(output_dir, "HF_accessibility.tif")

    target <- friction
    valid_cells <- cells[!is.na(cells)]
    target[valid_cells] <- 0
    acc <- terra::costDist(target, target = 0)

    terra::writeRaster(acc, output_filename, overwrite = overwrite)
    return(invisible(output_dir))
  }

  # Individual surfaces: one per point -----------------------------------------
  labels <- points[[id_col]]
  if (anyDuplicated(labels)) {
    stop("Error: the points do not have unique identifiers in '", id_col, "'.")
  }

  message("Creating ", n_points, " accessibility surfaces.")
  pb <- utils::txtProgressBar(min = 0, max = n_points, style = 3, width = 80)
  on.exit(close(pb), add = TRUE)

  for (i in seq_len(n_points)) {
    output_filename <- fs::path(output_dir, paste0(labels[i], ".tif"))

    if (check_existing && file.exists(output_filename)) {
      utils::setTxtProgressBar(pb, i)
      next
    }

    if (is.na(cells[i])) {
      utils::setTxtProgressBar(pb, i)
      next
    }

    target <- friction
    target[cells[i]] <- 0
    acc <- terra::costDist(target, target = 0)

    terra::writeRaster(acc, output_filename, overwrite = overwrite)
    utils::setTxtProgressBar(pb, i)
  }

  invisible(output_dir)
}


# Resolve a cropping extent (SpatExtent) from a raster, sf polygon, or file path.
# Returns NULL when extent_file is NA (use full friction extent).
.resolve_extent <- function(extent_file, std_crs, clip_flag) {

  if (is.atomic(extent_file) && length(extent_file) == 1 && is.na(extent_file)) {
    return(NULL)
  }

  if (inherits(extent_file, "SpatRaster")) {
    r <- extent_file
    if (!terra::same.crs(r, std_crs)) r <- terra::project(r, std_crs)
    return(terra::ext(r))
  }

  if (inherits(extent_file, c("sf", "sfc", "SpatVector"))) {
    v <- terra::vect(extent_file)
    if (!terra::same.crs(v, std_crs)) v <- terra::project(v, std_crs)
    return(.maybe_expand(terra::ext(v), clip_flag))
  }

  if (is.character(extent_file) && grepl("\\.shp$", extent_file)) {
    message("Loading shapefile")
    v <- terra::vect(extent_file)
    if (!terra::same.crs(v, std_crs)) v <- terra::project(v, std_crs)
    return(.maybe_expand(terra::ext(v), clip_flag))
  }

  if (is.character(extent_file) && grepl("\\.tif", extent_file)) {
    message("Loading raster")
    r <- terra::rast(extent_file)
    if (!terra::same.crs(r, std_crs)) r <- terra::project(r, std_crs)
    return(terra::ext(r))
  }

  stop("`extent_file` must be a SpatRaster, sf polygon, or a path to a ",
       "shapefile or raster.")
}


# Expand a SpatExtent by a small relative margin unless clip_flag is TRUE.
.maybe_expand <- function(e, clip_flag, frac = 0.005) {
  if (clip_flag) return(e)
  dx <- (e$xmax - e$xmin) * frac
  dy <- (e$ymax - e$ymin) * frac
  terra::ext(e$xmin - dx, e$xmax + dx, e$ymin - dy, e$ymax + dy)
}
