
<!-- README.md is generated from README.Rmd. Please edit that file -->

# catchment

<!-- badges: start -->
<!-- badges: end -->

The goal of catchment is to …

## Installation

Before installing this package, first you will need to install the
[R-INLA](https://www.r-inla.org/home) and [Template Model Builder
(TMB)](https://github.com/kaskr/adcomp) packages. These packages are not
on CRAN, and the installation process may depend on the type of computer
you are using. I recommend following the [installation instructions for
INLA](https://www.r-inla.org/download-install) first, and then install
TMB. Be sure to close out of any active R session before installation!

The catchment package is also not on CRAN (yet!). You can install the
development version of catchment from [GitHub](https://github.com/)
with:

``` r
# install.packages("devtools")
devtools::install_github("PATH-Global-Health/catchment")
```

## Example

We are currently developing documentation and tutorial materials.

``` r
# Load libraries
library(raster)
library(catchment)

# 1. Pre-processing --------------------------------------
## Load example dataset
data("example_shp")
data("example_pop")
data("example_locs")

## Get friction surface
fric <- PATHtools::get_friction_surface(example_shp) |> 
  raster::resample(example_pop, fun = "mean")

## Create output folder
f <- tempfile()
fs::dir_create(fs::path(f, "tt"))

# 2. Travel time and intial access surfaces --------------
## Create individual travel time rasters
create_travel_surface(friction_surface = fric, extent_file = example_pop,
  points = example_locs, id_col = "label", x_col = "x", y_col = "y",
  output_dir = fs::path(f, "tt"), individual_surfaces = T, parallel = T, cores = 5)

## Organize travel time matrix
tmat <- travel_mat_from_folder(dir = fs::path(f, "tt"), reference = example_pop)

## Create initial accessbility matrix
pmat <- initial_access_surface(tmat, n_fac_limit = 10, force_threshold = 300, sparse = F)

# 3. Fit catchment model ---------------------------------
## Organize input data
catch_dat <- prepare_data(prob_mat_init = pmat, pop_raster = example_pop,
  location_data = example_locs, mesh.args = list(cutoff = 0.1,max.edge = c(0.1, 4)))

## View INLA mesh
plot(catch_dat$mesh)
plot(example_shp, add = T, border = "red", lwd = 2)
points(example_locs$x, example_locs$y)

## Fit catchment model
mod <- catchment_model(catch_dat)

# 4. Post-processing -------------------------------------
## Estimated catchment populations
catchment_populations(mod)
example_locs$est_pop <- catchment_populations(mod)
```
