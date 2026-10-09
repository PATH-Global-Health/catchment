# ----------------------------------------------------------------------------
# catchment: new-features showcase
# ----------------------------------------------------------------------------
# An end-to-end, runnable tour of the features added in the recent modelling
# overhaul:
#
#   * friction surface pulled with the traveltime package (idem-lab walk2020)
#   * LEARNED distance-decay (exponential or power) estimated from the data,
#     instead of a baked-in fixed surface
#   * choice of likelihood family (Poisson or negative-binomial)
#   * automatic convergence diagnostics (check_convergence / print method)
#   * uncertainty on catchment populations propagated via TMB::sdreport()
#   * model validation: posterior-predictive checks (pp_check) and
#     leave-one-facility-out cross-validation (loo_facility_cv)
#   * ggplot2 / tidyterra plot methods for catchments and probability surfaces
#
# Run from the package root with the package built/loaded. The model fit is
# computationally intensive; the bundled example data keeps it modest.
#
# See also inst/examples/synthetic-showcase.R for a fully self-contained version
# that builds its own grid/population/friction and simulates model-consistent
# counts, so the fit converges cleanly and recovery can be checked against a
# known ground truth.
# ----------------------------------------------------------------------------

library(catchment)
library(terra)

set.seed(2024)

# --- 1. Load the bundled example data ---------------------------------------
# example_shp : study boundary (sf)         -- lazy-loaded
# example_locs: facility locations + weights -- lazy-loaded data frame
# example_pop : population raster            -- via example_pop()
data("example_shp")
data("example_locs")
pop <- example_pop()

# Output scratch space for the per-facility travel-time rasters.
f <- tempfile()
fs::dir_create(fs::path(f, "tt"))

# --- 2. Friction surface via the traveltime package -------------------------
# NEW: friction is pulled from the Malaria Atlas Project walking-only surface
# rather than hand-rolled. Mask to the boundary and align to the population grid.
#   remotes::install_github("idem-lab/traveltime")
fric <- traveltime::get_friction_surface(surface = "walk2020", extent = pop) |>
  terra::mask(terra::vect(example_shp))
fric <- terra::resample(fric, pop, method = "average")

# --- 3. Per-facility travel-time surfaces -----------------------------------
# costDist-based accumulated travel time from every facility. Saved to disk so
# this (slow) step only runs once.
create_travel_surface(
  friction_surface = fric,
  extent_file      = pop,
  points           = example_locs,
  id_col           = "label",
  x_col            = "x",
  y_col            = "y",
  output_dir       = fs::path(f, "tt"),
  individual_surfaces = TRUE)

# Collapse the rasters into a sparse pixel-by-facility travel-time matrix,
# keeping only populated pixels.
tmat <- travel_mat_from_folder(dir = fs::path(f, "tt"), reference = pop)

# --- 4. Prepare data (learned-decay path) -----------------------------------
# NEW: pass the RAW travel-time matrix and let the model learn the decay shape.
# The sparsity controls (minimum_time / force_threshold / n_fac_limit) live here.
catch_dat <- prepare_data(
  prob_mat_init   = tmat,
  pop_raster      = pop,
  location_data   = example_locs,
  minimum_time    = 10,
  force_threshold = 300,
  n_fac_limit     = 10,
  mesh.args = list(cutoff = 0.1, max.edge = c(0.1, 4)))

print(catch_dat)   # catchment_data print method
plot(catch_dat$mesh)

# --- 5. Fit the model -------------------------------------------------------
# Default: Poisson likelihood + LEARNED exponential decay exp(-d / tau).
mod <- catchment_model(catch_dat)

# The learned decay is stored on the fitted object:
mod$decay          # "exponential"
mod$decay_param    # estimated tau (minutes)

# Alternative model specifications (commented to keep the script quick):
#   mod_pow <- catchment_model(catch_dat, decay = "power")  # learn exponent a in d^-a
#   mod_nb  <- catchment_model(catch_dat, family = "nb")    # overdispersed counts

# --- 6. Convergence diagnostics ---------------------------------------------
# catchment_model() already warns on failure; inspect the detail explicitly.
check_convergence(mod)   # converged / max gradient / pdHess
print(mod)               # family, decay, convergence, pdHess

# --- 7. Catchment populations, with uncertainty -----------------------------
# Point estimates:
catchment_populations(mod)

# NEW: standard errors + ~95% CIs propagated through TMB::sdreport().
pop_est <- catchment_populations(mod, uncertainty = TRUE)
print(pop_est)

# --- 8. Validation ----------------------------------------------------------
# Posterior-predictive check: does the model reproduce observed facility counts?
ppc <- pp_check(mod, nsim = 1000, seed = 1)
ppc$coverage     # fraction of observed counts inside their predictive interval
ppc$dispersion   # ~1 indicates Poisson-consistent dispersion
# ppc$plot       # ggplot of observed vs predictive intervals (needs ggplot2)

# Leave-one-facility-out cross-validation (refits with each count held out).
cv <- loo_facility_cv(mod)
attr(cv, "rmse")
attr(cv, "mae")
print(cv)

# --- 9. Visualise catchments ------------------------------------------------
# Dominant-catchment map (the default plot() for a fitted model). Uses
# ggplot2 + tidyterra when available, base terra otherwise.
plot(mod)

# Access-probability surface for a single facility.
plot_prob_surface(mod, id_label = example_locs$label[1])

# Or pull the raster directly for custom mapping.
prob_r <- get_prob_raster(mod, id_label = example_locs$label[1])

# ----------------------------------------------------------------------------
# Cleanup
# ----------------------------------------------------------------------------
unlink(f, recursive = TRUE)
