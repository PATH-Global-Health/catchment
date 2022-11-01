# Create example datasets
library(terra)
library(exactextractr)
library(raster)
library(catchment)


example_shp <- malariaAtlas::getShp("Zambia",admin_level = "admin2")[3,] |>
  sf::st_as_sf() |>
  sf::st_geometry() |>
  as("Spatial")

catalogue <- wopr::getCatalogue()
catalogue_sub <- subset(catalogue,
                        country == 'ZMB' &
                          category == 'Population' &
                          version == 'v1.0' &
                          filetype == 'gridded')

f <- tempfile()
wopr::downloadData(catalogue_sub, wopr_dir = f)
unzip(list.files(f, recursive = T, pattern = "*.zip", full.names = T),
      exdir = f)

list.dirs(f, recursive = T)
pop <- terra::rast(
  list.files(f, recursive = T, pattern = "*gridded.tif$", full.names = T))

# Get to 1 km res
pop <- crop(pop, vect(shp), mask = T) |>
  terra::aggregate(10, fun = "sum", na.rm = T) |>
  raster()

plot(pop)
plot(vect(shp), add = T)
res(pop)

example_pop <- pop

# Create 100 fake facilities
f <- tempfile()
tpop <- sum(raster::values(pop), na.rm = T)
pval <- raster::values(pop)[which(!is.na(raster::values(pop)))]

# Points data
nhf <- 100
sp   <- sample(which(!is.na(raster::values(pop))), nhf, prob = pval/tpop)
crds <- data.frame(
  label = stringi::stri_rand_strings(nhf, 6),
  raster::coordinates(pop)[sp,],
  weight = (abs(ceiling(rnorm(nhf, mean = raster::values(pop)[sp],
                              median(pval))))) +1)
crds <- dplyr::arrange(crds, label)
raster::plot(pop)
raster::plot(shp, add = T)
points(crds[,c("x", "y")])

example_locs <- crds

# Get friction surface
fric <- PATHtools::get_friction_surface(shp)

values(pop)[values(pop) == 0] <- NA

# Resample to get rasters aligned
fric <- raster::resample(fric, pop, na.rm = T)
raster::stack(pop, fric)  # can only stack if aligned
names(pop) <- "pop"; names(fric) <- "fric"

# Get travel time surfaces -------
fs::dir_create(fs::path(f, "tt"))

create_travel_surface(
  friction_surface = fric,
  extent_file = pop,
  points = crds,
  id_col = "label",
  x_col = "x",
  y_col = "y",
  output_dir = fs::path(f, "tt"),
  individual_surfaces = T,
  parallel = T,
  cores = 5)

tts <- lapply(list.files(fs::path(f, "tt"), pattern = "*.tif", full.names = T), raster::raster)

raster::plot(tts[[1]])
points(crds[1,c("x", "y")])

# Create travel time matrix from rasters --------------

tmat <- travel_mat_from_folder(
  dir = fs::path(f, "tt"),
  reference = pop)

# Create initial access surface -------------

pmat <- initial_access_surface(tmat, sparse = F, n_fac_limit = 10, force_threshold = 300)
# spmat <- initial_access_surface(tmat)

# Check to make sure all pixel probabilities sum to 1
sum(round(rowSums(pmat), 5) == 1) == nrow(pmat)

# Get pre-model catchment estimates
t(pmat) %*% raster::values(pop)[!is.na(raster::values(pop))]

# Make sure there are no locations zero population
# If so, may want to adjust parameters in initial_access_surface()
sum(t(pmat) %*% raster::values(pop)[!is.na(raster::values(pop))] < 1) == 0

# Design model inputs ----------

catch_dat <- prepare_data(
  prob_mat_init = pmat,
  pop_raster = pop,
  location_data = crds,
  mesh.args = list(cutoff = 0.1,max.edge = c(0.1, 4)))

plot(catch_dat$mesh)
plot(vect(shp), add = T, border = "red", lwd = 2)

# Fit model --------------------
mod <- catchment_model(catch_dat)

# Outputs ---------------
catchment_populations(mod)
