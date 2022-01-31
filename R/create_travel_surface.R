

#' Create travel time surfaces
#'
#' @param friction_surface A raster containing the travel cost per pixel.
#' @param points A dataframe containing fields for LABEL, X, and Y for point-level features.
#' @param extent_file A raster or shapefile used to define extent of points and friction surface.
#' @param id_col A character string corresponding to the column in points dataframe for LABEL field.
#' @param x_col A character string corresponding to the column in points dataframe for x coordinate field.
#' @param y_col A character string corresponding to the column in points dataframe for y coordinate field.
#' @param output_dir A character string for output directory (rasters and intermediate outputs).
#' @param clip_flag TRUE/FALSE: Should the friction surface extent be clipped?
#' @param transition_matrix_exists_flag TRUE/FALSE: Has the already been constructed, must be in output_dir.
#' @param individual_surfaces TRUE/FALSE: Should individual travel surface be created for each point?
#' @param parallel TRUE/FALSE: Should individual surfaces be created using parallel processing?
#' @param cores an integer defining the number of CPU cores used for parallel processing
#' @param check_existing TRUE/FALSE: Should rasters with LABEL in the output_dir be ignored?
#' @param overwrite TRUE/FALSE: Should rasters in output_dir be overwritten?
#' @param ...
#'
#' @importFrom raster raster extent projectRaster crop writeRaster
#' @importFrom sp proj4string
#' @importFrom gdistance transition geoCorrection accCost
#' @importFrom parallel detectCores
#' @importFrom foreach foreach %dopar%
#' @importFrom doSNOW registerDoSNOW
#' @importFrom snow makeSOCKcluster stopCluster
#' @importFrom fs path
#'
#' @export
#' @return
#'

create_travel_surface <- function(friction_surface,
                                  points,
                                  extent_file = NA,
                                  id_col = NA,
                                  x_col = "x",
                                  y_col = "y",
                                  clip_flag = FALSE,
                                  transition_matrix_exists_flag = FALSE,
                                  individual_surfaces = FALSE,
                                  output_dir = NA,
                                  parallel = TRUE,
                                  cores = floor(parallel::detectCores() / 2),
                                  check_existing = FALSE,
                                  overwrite = FALSE, ...) {
  std_projection <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"

  # Creating extent for the accessibility surfaces ----------
  ## we will use either a raster or a polygon to define the required extent for
  ## the new rasters

  if (grepl("shp", extent_file)) {
    message("Loading shapefile")
    poly <- rgdal::readOGR(
      dsn = extent_file,
      layer = gsub(".*/(.*).shp", "\\1", extent_file)
    )

    # need to make sure our polygon has the right projection
    if (sp::proj4string(poly) != std_projection) {
      message("Reprojecting shapefile")
      poly <- sp::spTransform(poly, sp::CRS(std_projection))
    }

    ## if your general_shapefile path is directly to a known extent, you don't
    ## need to calculate a new extent
    if (clip_flag == TRUE) {
      new_extent <- as(raster::extent(extent_matrix), "SpatialPolygons")
    } else {
      ## calculates some boundary around the polygon of interest
      extent_matrix <- as.matrix(extent(poly))
      extent_matrix_signs <- extent_matrix / abs(extent_matrix)
      extent_matrix_multiplier <- as.data.frame(extent_matrix_signs) %>%
        dplyr::mutate(
          xmin = dplyr::case_when(
            min[1] > 0 ~ 0.995,
            min[1] < 0 ~ 1.005
          ),
          xmax = dplyr::case_when(
            max[1] > 0 ~ 1.005,
            max[1] < 0 ~ 0.995
          ),
          ymin = dplyr::case_when(
            min[2] > 0 ~ 0.99,
            min[2] < 0 ~ 1.01
          ),
          ymax = dplyr::case_when(
            max[2] > 0 ~ 1.01,
            max[2] < 0 ~ 0.99
          )
        ) %>%
        dplyr::slice(1) %>%
        dplyr::select(xmin, ymin, xmax, ymax)

      ### this defines the boundary, need to be careful with this
      extent_matrix <- extent_matrix * as.numeric(extent_matrix_multiplier)
      new_extent <- as(raster::extent(extent_matrix), "SpatialPolygons")
    }

    sp::proj4string(new_extent) <- sp::proj4string(poly)
  } else if ("RasterLayer" %in% class(extent_file)) {
    raster <- extent_file
    if (sp::proj4string(raster) != std_projection) {
      message("Reprojecting raster")
      raster <- raster::projectRaster(raster, crs = std_projection)
    }

    new_extent <- as(raster::extent(raster), "SpatialPolygons")
    sp::proj4string(new_extent) <- sp::proj4string(raster)
  } else if (grepl("tif", extent_file)) {
    message("Loading raster")

    ## load in the raster and ensure it has the right projection
    raster <- raster::raster(extent_file)
    if (sp::proj4string(raster) != std_projection) {
      message("Reprojecting raster")
      raster <- raster::projectRaster(raster, crs = std_projection)
    }

    new_extent <- as(raster::extent(raster), "SpatialPolygons")
    sp::proj4string(new_extent) <- sp::proj4string(raster)
  } else {
    stop("File must be a shapefile or raster")
  }


  # Defining the spatial template and creating new transition matrices ----
  if (class(friction_surface) == "character") {
    friction <- raster::raster(friction_surface)
  } else {
    friction <- friction_surface
  }

  fs1 <- crop(friction, new_extent)

  # Apply geocorrections ----

  # if the geo-corrected graph has already been made, loading it in saves time.
  # Uses the same T.GC.filename as specified using the T.GC.filename variable.
  # Else, make the graph and the geo-corrected version of the graph

  if (transition_matrix_exists_flag) {
    # Read in the transition matrix object if it has been pre-computed
    message("Reading in transition matrix")
    T.GC <- readRDS(fs::path(output_dir, "HF.T.GC.rds"))
  } else {
    message("Calculating transition matrix")

    T.filename <- fs::path(output_dir, "HF.T.rds")
    T.GC.filename <- fs::path(output_dir, "HF.T.GC.rds")

    # Make and geocorrect the transition matrix (i.e., the graph)
    # Making the transition matrix is RAM intensive and can be very slow for large areas
    Tr <- gdistance::transition(fs1, function(x) 1 / mean(x), 8)
    saveRDS(Tr, T.filename)
    T.GC <- gdistance::geoCorrection(Tr)
    saveRDS(T.GC, T.GC.filename)
  }

  # Calculating the cost surface(s) -----------

  # load the points file
  if ("character" %in% class(points) == TRUE) {
    points <- read.csv(file = points)
  } else {
    points <- data.frame(points)
  }

  # For looping through all points, initialize this
  temp <- dim(points)
  n.points <- temp[1]

  ## we may be running this script for either:
  ##        - one overall surface representing time to any HF
  ##        - time to each individual HF


  if (!individual_surfaces) {
    message("Creating single accessibility surface.")

    # creating only one file
    output.filename <- fs::path(output_dir, "HF_accessibility.tif")

    # Convert the points into a matrix
    xy.data.frame <- data.frame()
    xy.data.frame[1:n.points, 1] <- points[, grep(x_col, names(points))]
    xy.data.frame[1:n.points, 2] <- points[, grep(y_col, names(points))]
    xy.matrix <- as.matrix(xy.data.frame)

    # Run the accumulated cost algoritm to make the final output map
    temp.raster <- gdistance::accCost(T.GC, xy.matrix)
    plot(temp.raster)
    # Write the resulting raster
    raster::writeRaster(temp.raster, output.filename, overwrite = FALSE)
  } else if (individual_surfaces) {
    # looping over all of the points and calculating the time from all pixels
    # to that point
    HF_list <- unique(points[, which(names(points) %in% c(id_col))]) # get a vector of the HF_ids

    if (n.points != length(HF_list)) {
      stop("Error: the points do not have unique identifiers")
    }

    # Write distance rasters in parallel
    if (parallel == TRUE) {
      message("Setting up parallel processing on ", cores, " cores.")

      # Check for previous versions and overwrite
      if (overwrite == TRUE) {
        fn <- list.files(output_dir, pattern = "*.tif$", full.names = TRUE)
        if (length(fn) == 0) {
          cat("No files to remove")
        } else {
          cat("Overwriting", length(fn), "files (in 5 seconds). \n")
          Sys.sleep(5)
          cat("Goodbye! \n")
          file.remove(fn)
        }
      }

      cl <- snow::makeSOCKcluster(cores)
      doSNOW::registerDoSNOW(cl)

      message("Creating accessibility surfaces.")

      mypb <- txtProgressBar(
        min = 0, max = n.points, initial = 0,
        width = 80, style = 3
      )
      progress <- function(n) setTxtProgressBar(mypb, n)
      opts <- list(progress = progress)

      foreach::foreach(
        i = 1:n.points,
        .packages = c("gdistance", "raster"),
        .options.snow = opts
      ) %dopar% {
        output.filename <- fs::path(output_dir, paste0(HF_list[i], ".tif"))

        HF.coords <- c(points[i, x_col], points[i, y_col])
        HF.raster <- gdistance::accCost(T.GC, HF.coords)

        raster::writeRaster(HF.raster, output.filename, overwrite = TRUE)
        gc()
      }

      close(mypb)
      snow::stopCluster(cl)
    } else {
      message("Creating accessibility surfaces")
      mypb <- txtProgressBar(min = 0, max = n.points, initial = 0, width = 80, style = 3)

      for (i in 1:n.points) {
        output.filename <- fs::path(output_dir, paste0(HF_list[i], ".tif"))

        HF.coords <- c(points[i, x_col], points[i, y_col])
        HF.raster <- gdistance::accCost(T.GC, HF.coords)

        raster::writeRaster(HF.raster, output.filename, overwrite = TRUE)

        setTxtProgressBar(mypb, i, label = i)
      }
      close(mypb)
    }
  }
}
