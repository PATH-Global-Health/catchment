# Generate the bundled example data (data/example_locs.rda).
#
# The facility "weight" column is the OBSERVED event count fed to the model as
# Y_hf. To give a well-identified example that converges with a positive-
# definite Hessian (and therefore exercises the uncertainty / pp_check / LOFO-CV
# tooling), the counts are SIMULATED from the gravity model itself: population is
# allocated to facilities through a known exponential distance-decay and mild
# facility masses, then thinned to event counts with Poisson noise.
#
# Friction surface is pulled with the traveltime package
# (https://github.com/idem-lab/traveltime). example_pop.tif and example_shp.rda
# are reused as-is. Run from the package root with the catchment package built.

library(terra)
library(catchment)

set.seed(2024)

# --- Inputs -------------------------------------------------------------------
pop <- example_pop()
load("data/example_shp.rda")          # example_shp (sf polygon)

# --- Friction surface via traveltime ------------------------------------------
fric <- traveltime::get_friction_surface(surface = "walk2020", extent = pop) |>
  terra::mask(terra::vect(example_shp))
fric <- terra::resample(fric, pop, method = "average")

# Drop zero-population pixels from the allocation domain.
pv <- terra::values(pop, mat = FALSE)
pv[!is.na(pv) & pv == 0] <- NA
terra::values(pop) <- pv
valid <- which(!is.na(terra::values(pop, mat = FALSE)))
pop_vec <- terra::values(pop, mat = FALSE)[valid]

# --- Facility locations -------------------------------------------------------
# ~30 facilities sampled by population so every catchment is non-empty.
n_hf  <- 30
probs <- pop_vec / sum(pop_vec)
cells <- sample(valid, n_hf, prob = probs)
xy    <- terra::xyFromCell(pop, cells)

locs <- data.frame(
  label = sprintf("HF%02d", seq_len(n_hf)),
  x     = xy[, 1],
  y     = xy[, 2],
  weight = 1,                          # placeholder, overwritten below
  stringsAsFactors = FALSE
)

# --- Travel-time matrix -------------------------------------------------------
tt_dir <- file.path(tempdir(), "tt")
dir.create(tt_dir, showWarnings = FALSE)
create_travel_surface(
  friction_surface = fric, extent_file = pop,
  points = locs, id_col = "label", x_col = "x", y_col = "y",
  output_dir = tt_dir, individual_surfaces = TRUE, overwrite = TRUE)
tmat <- travel_mat_from_folder(dir = tt_dir, reference = pop)

# --- Simulate model-consistent observed counts --------------------------------
# Use prepare_data() so we decay exactly the same clamped/masked travel template
# the model uses (dat$travel_sparse, [n_hf x n_pixel]).
TRUE_TAU       <- 45      # exponential decay scale (minutes)
TRUE_MASS_SD   <- 0.10    # facility-mass spread (matches the model's prior)
INCIDENCE_RATE <- 0.25    # events per person in the catchment

catch_dat0 <- prepare_data(
  prob_mat_init = tmat, pop_raster = pop, location_data = locs,
  minimum_time = 10, force_threshold = 300, n_fac_limit = 10,
  mesh.args = list(cutoff = 0.1, max.edge = c(0.1, 4)))

tt <- t(as.matrix(catch_dat0$travel_sparse))       # [n_pixel x n_hf] clamped
nz <- tt != 0

w <- tt
w[nz] <- exp(-tt[nz] / TRUE_TAU)                   # exponential decay
mass  <- exp(stats::rnorm(n_hf, 0, TRUE_MASS_SD))  # facility attractiveness
w <- sweep(w, 2, mass, `*`)                        # re-weight by mass
w <- w / rowSums(w)                                # per-pixel allocation

pop_hf <- as.vector(t(w) %*% catch_dat0$pop_vec)   # catchment population
mu     <- INCIDENCE_RATE * pop_hf                  # expected event counts
counts <- stats::rpois(n_hf, mu)

locs$weight <- counts
example_locs <- dplyr::arrange(locs, label)
cat("Simulated counts summary:\n"); print(summary(example_locs$weight))

# --- Save ---------------------------------------------------------------------
save(example_locs, file = "data/example_locs.rda", compress = "xz")

# --- Verify the fit (should converge with pdHess = TRUE) ----------------------
catch_dat <- prepare_data(
  prob_mat_init = tmat, pop_raster = pop, location_data = example_locs,
  minimum_time = 10, force_threshold = 300, n_fac_limit = 10,
  mesh.args = list(cutoff = 0.1, max.edge = c(0.1, 4)))
mod <- catchment_model(catch_dat, time = FALSE)

cat("\n--- verification ---\n")
cat("converged:", mod$fit$convergence == 0, "\n")
cat("pdHess   :", mod$sdr$pdHess, "\n")
cat("decay tau (true 45):", round(mod$decay_param, 1), "\n")
print(utils::head(catchment_populations(mod, uncertainty = TRUE)))
