# Run once to (re)generate the golden-master fixture used in test-golden-master.R.
# Run from the package root: Rscript inst/generate-golden-master.R
# Uses devtools::load_all() so it always reflects the current source.
library(catchment)

pop <- example_pop()
data("example_locs", package = "catchment")
locs <- example_locs[1:10, ]

fric <- pop
set.seed(1)
terra::values(fric) <- stats::runif(terra::ncell(fric), 0.001, 0.02)

dir <- tempfile()
dir.create(dir)
suppressMessages(
  create_travel_surface(friction_surface = fric, extent_file = pop,
                        points = locs, id_col = "label", x_col = "x", y_col = "y",
                        output_dir = dir, individual_surfaces = TRUE,
                        overwrite = TRUE)
)

tmat <- travel_mat_from_folder(dir = dir, reference = pop, progress = FALSE)
pmat <- suppressMessages(
  initial_access_surface(tmat, n_fac_limit = 6, force_threshold = 300,
                         sparse = FALSE)
)
catch_dat <- suppressMessages(
  prepare_data(prob_mat_init = pmat, pop_raster = pop, location_data = locs,
               mesh.args = list(cutoff = 0.1, max.edge = c(0.2, 4)))
)

mod <- suppressMessages(catchment_model(catch_dat, time = FALSE))

snap <- list(
  case_hf    = mod$obj$report()$case_hf,
  pop_hf     = mod$obj$report()$pop_hf,
  catchment_populations = catchment_populations(mod)
)

saveRDS(snap, file.path("tests/testthat/fixtures/golden-master.rds"))
message("Golden master saved.")
