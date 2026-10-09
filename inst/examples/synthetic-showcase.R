# ----------------------------------------------------------------------------
# catchment: synthetic, self-contained feature showcase
# ----------------------------------------------------------------------------
# This script builds an ENTIRELY SYNTHETIC world -- grid, boundary, population,
# friction, facility locations -- and simulates facility counts FROM THE MODEL'S
# OWN gravity process. Nothing depends on the bundled example data.
#
# Why simulate the counts from the model? Because it gives us a known ground
# truth (a true decay scale and true catchment populations), so the showcase can
# demonstrate that the model RECOVERS them, and the fit is well-identified
# (converges with a positive-definite Hessian -- which is exactly what the
# uncertainty / pp_check / LOFO-CV tooling needs).
#
# Features exercised:
#   * create_travel_surface() over a structured friction field (road + barrier)
#   * LEARNED exponential distance-decay (tau estimated from data)
#   * automatic convergence diagnostics (check_convergence / print)
#   * catchment populations WITH uncertainty (TMB::sdreport) vs. ground truth
#   * posterior-predictive checks (pp_check) + leave-one-facility-out CV
#   * ggplot2 / tidyterra plot methods
#
# Run from the package root. Self-contained and fast (~1-2 min).
#
# See also inst/examples/new-features.R for the same feature tour run on the
# bundled real-world example data (a realistic workflow, but not guaranteed to
# converge as cleanly as this controlled synthetic setup).
# ----------------------------------------------------------------------------

library(catchment)
library(terra)
library(sf)

set.seed(42)

# ============================================================================
# 1. A synthetic landscape
# ============================================================================
# A small lon/lat grid near the equator so cost-distance returns sensible
# travel times in minutes. ~0.4 degrees square, 70 x 70 pixels.
ext_deg <- 0.4
n_side  <- 70
r <- rast(xmin = 0, xmax = ext_deg, ymin = 0, ymax = ext_deg,
          nrows = n_side, ncols = n_side, crs = "EPSG:4326")
xy <- crds(r, na.rm = FALSE)

# --- Study boundary: a wobbly polygon (demonstrates masking) -----------------
cx <- 0.2; cy <- 0.2
theta <- seq(0, 2 * pi, length.out = 60)
rad   <- 0.17 * (1 + 0.18 * sin(3 * theta))
ring  <- cbind(cx + rad * cos(theta), cy + rad * sin(theta))
ring[nrow(ring), ] <- ring[1, ]                       # close the ring
boundary <- st_sf(geometry = st_sfc(st_polygon(list(ring)), crs = 4326))

# --- Population: a few Gaussian "towns" on a low background ------------------
towns <- rbind(c(0.14, 0.16), c(0.27, 0.24), c(0.20, 0.30), c(0.30, 0.13))
tmass <- c(45, 70, 35, 25)
tsd   <- c(0.035, 0.045, 0.030, 0.028)
popv  <- rep(2, nrow(xy))                              # background
for (i in seq_len(nrow(towns))) {
  d2   <- (xy[, 1] - towns[i, 1])^2 + (xy[, 2] - towns[i, 2])^2
  popv <- popv + tmass[i] * exp(-d2 / (2 * tsd[i]^2))
}
pop <- setValues(r, popv)
pop <- mask(round(pop), vect(boundary))               # clip to boundary
pop[pop < 1] <- NA                                    # drop empty pixels

# --- Friction surface: baseline walk + a fast road + a slow barrier ----------
# Units are minutes per metre (matches MAP walk surfaces). Lower = faster.
fric_v <- rep(0.012, nrow(xy))                         # ~5 km/h walking
road   <- abs((xy[, 2] - 0.05) - 0.9 * xy[, 1])        # diagonal corridor
fric_v[road < 0.015] <- 0.004                          # fast road
barr   <- (xy[, 1] - 0.24)^2 + (xy[, 2] - 0.20)^2      # high-friction blob
fric_v[barr < 0.012^2 * 30] <- 0.045                   # barrier (river/swamp)
fric <- mask(setValues(r, fric_v), vect(boundary))

# --- Facilities: sampled proportional to population (non-empty catchments) ---
valid <- which(!is.na(values(pop, mat = FALSE)))
pvec  <- values(pop, mat = FALSE)[valid]
n_hf  <- 22
cells <- sample(valid, n_hf, prob = pvec / sum(pvec))
fxy   <- xyFromCell(pop, cells)
locs  <- data.frame(label  = sprintf("HF%02d", seq_len(n_hf)),
                    x      = fxy[, 1],
                    y      = fxy[, 2],
                    weight = NA_real_)        # counts filled in below

# ============================================================================
# 2. Travel-time surfaces over the friction field
# ============================================================================
tt_dir <- file.path(tempdir(), "synthetic_tt")
unlink(tt_dir, recursive = TRUE); dir.create(tt_dir)

create_travel_surface(
  friction_surface = fric, extent_file = pop,
  points = locs, id_col = "label", x_col = "x", y_col = "y",
  output_dir = tt_dir, individual_surfaces = TRUE, overwrite = TRUE)

# Sparse pixel-by-facility travel-time matrix (populated pixels only).
tmat <- travel_mat_from_folder(dir = tt_dir, reference = pop)

# ============================================================================
# 3. Ground truth: simulate counts from the gravity model itself
# ============================================================================
# Allocate each pixel's population across facilities via a KNOWN exponential
# decay and mild facility masses, sum to per-facility catchment populations,
# then thin to event counts with Poisson noise. We keep the true values so we
# can check recovery later.
TRUE_TAU     <- 40       # true decay scale (minutes)
TRUE_MASS_SD <- 0.10     # facility attractiveness spread
INCIDENCE    <- 0.12     # events per person in the catchment

mesh_args <- list(cutoff = 0.01, max.edge = c(0.03, 0.1))

# prepare_data() also builds the clamped sparse travel template the model uses,
# so decaying THAT guarantees the simulation matches the model's geometry.
catch0 <- prepare_data(
  prob_mat_init = tmat, pop_raster = pop, location_data = locs,
  minimum_time = 10, force_threshold = 300, n_fac_limit = 8,
  mesh.args = mesh_args)

tt <- t(as.matrix(catch0$travel_sparse))               # [n_pixel x n_hf]
nz <- tt != 0
w  <- tt
w[nz]    <- exp(-tt[nz] / TRUE_TAU)                     # exponential decay
true_mass <- exp(rnorm(n_hf, 0, TRUE_MASS_SD))
w  <- sweep(w, 2, true_mass, `*`)                      # weight by mass
w  <- w / rowSums(w)                                   # per-pixel allocation

true_catchment_pop <- as.vector(t(w) %*% catch0$pop_vec)
locs$weight <- rpois(n_hf, INCIDENCE * true_catchment_pop)

cat("Simulated facility counts:\n"); print(summary(locs$weight))

# ============================================================================
# 4. Visualise the synthetic data
# ============================================================================
# Everything below is drawn straight from the constructed inputs -- no model
# involved yet. The four panels show the population, the friction field, an
# example travel-time surface, and the simulated counts.
op <- par(mfrow = c(2, 2), mar = c(3, 3, 3, 4))

# (a) Population with boundary; facilities sized by their simulated count.
plot(pop, main = "Population + facilities")
plot(st_geometry(boundary), add = TRUE, border = "red", lwd = 2)
points(locs$x, locs$y, pch = 21, bg = "white",
       cex = 0.6 + 2 * locs$weight / max(locs$weight))

# (b) Friction field -- note the fast diagonal road and the slow barrier blob.
plot(fric, main = "Friction (min/m): road + barrier")
plot(st_geometry(boundary), add = TRUE, border = "red", lwd = 2)
points(locs$x, locs$y, pch = 19, cex = 0.5)

# (c) Travel-time surface from one facility; the road/barrier bend the contours
#     away from simple straight-line distance.
tt_r <- rast(file.path(tt_dir, paste0(locs$label[1], ".tif")))
plot(tt_r, main = paste("Travel time (min) from", locs$label[1]))
plot(st_geometry(boundary), add = TRUE, border = "red", lwd = 2)
points(locs$x[1], locs$y[1], pch = 17, cex = 1.6)

# (d) Simulated counts against the (known) true catchment population.
plot(true_catchment_pop, locs$weight, pch = 19, col = "steelblue",
     xlab = "true catchment pop", ylab = "simulated count",
     main = "Counts vs. true catchment pop")
par(op)

# ============================================================================
# 5. Prepare data + fit (learned exponential decay, Poisson)
# ============================================================================
catch_dat <- prepare_data(
  prob_mat_init = tmat, pop_raster = pop, location_data = locs,
  minimum_time = 10, force_threshold = 300, n_fac_limit = 8,
  mesh.args = mesh_args)
print(catch_dat)

mod <- catchment_model(catch_dat)

# ============================================================================
# 6. Did we recover the truth?
# ============================================================================
cat("\n-- decay recovery --\n")
cat(sprintf("true tau = %.1f   estimated tau = %.1f (%s)\n",
            TRUE_TAU, mod$decay_param, mod$decay))

# Convergence diagnostics (should be: converged TRUE, pdHess TRUE).
check_convergence(mod)
print(mod)

# ============================================================================
# 7. Catchment populations with uncertainty vs. ground truth
# ============================================================================
est <- catchment_populations(mod, uncertainty = TRUE)
est$true_pop <- true_catchment_pop
est$covered  <- est$true_pop >= est$lower & est$true_pop <= est$upper
cat("\n-- catchment population estimates vs truth --\n")
print(utils::head(est))
cat(sprintf("90/95-style CI coverage of true catchment pop: %.0f%%\n",
            100 * mean(est$covered)))

# ============================================================================
# 8. Validation: posterior-predictive check + leave-one-facility-out CV
# ============================================================================
ppc <- pp_check(mod, nsim = 1000, seed = 1)
cat(sprintf("\npp_check: coverage = %.2f, dispersion = %.2f\n",
            ppc$coverage, ppc$dispersion))

cv <- loo_facility_cv(mod)
cat(sprintf("LOFO-CV: rmse = %.1f, mae = %.1f\n",
            attr(cv, "rmse"), attr(cv, "mae")))

# ============================================================================
# 9. Compare decay families and likelihoods (model selection)
# ============================================================================
# Refit with alternative specifications and rank by AIC (lower = better). The
# TMB objective is the negative log marginal likelihood, so
# AIC = 2 * objective + 2 * (number of fixed parameters). Because the data were
# generated with EXPONENTIAL decay + POISSON noise, those specifications should
# come out on top.
fit_aic <- function(m) 2 * m$fit$objective + 2 * length(m$fit$par)

mod_pow <- catchment_model(catch_dat, decay  = "power")  # learn exponent a in d^-a
mod_nb  <- catchment_model(catch_dat, family = "nb")     # overdispersed counts

comp <- data.frame(
  spec   = c("exp / poisson (truth)", "power / poisson", "exp / nb"),
  npar   = c(length(mod$fit$par),
             length(mod_pow$fit$par),
             length(mod_nb$fit$par)),
  AIC    = c(fit_aic(mod), fit_aic(mod_pow), fit_aic(mod_nb)))
comp <- comp[order(comp$AIC), ]
comp$dAIC <- round(comp$AIC - min(comp$AIC), 1)
cat("\n-- model comparison (lower AIC = better) --\n")
print(comp, row.names = FALSE)

# ============================================================================
# 10. Model-output maps (ggplot2 + tidyterra when available)
# ============================================================================
# (Synthetic inputs were plotted in section 4; these are the fitted results.)
print(plot(mod))                                       # dominant-catchment map
print(plot_prob_surface(mod, id_label = locs$label[1]))# access surface, 1 facility
if (!is.null(ppc$plot)) print(ppc$plot)                # observed vs predicted

# ----------------------------------------------------------------------------
# Cleanup
# ----------------------------------------------------------------------------
unlink(tt_dir, recursive = TRUE)
