# 6a-convergence-gof-assessment.R: script to do some basic exploration of model fit
#                                  results, including convergence assessment and residual
#                                  diagnostics. 
# Author: Jeffrey W. Doser
rm(list = ls())
library(spOccupancy)
library(spAbundance)
library(coda)

# Load the data -----------------------------------------------------------
load('data/ba_spAbundance_data.rda')
# NOTE: hardcoded
n.models <- 6

# Stage 2 Convergence Assessment ------------------------------------------
# Non-spatial null --------------------
# ALL CONVERGED
load('results/ba-univ-nonspatial-only-1e+05-samples-2026-01-13.rda')
out.ns.only <- out
summary(out.ns.only)
# Non-spatial no RE -------------------
# ALL CONVERGED
load('results/ba-univ-nonspatial-noRE-1e+05-samples-2026-01-13.rda')
out.ns <- out
summary(out.ns)
# Non-spatial RE ----------------------
# ALL CONVERGED
load('results/ba-univ-nonspatial-1e+05-samples-2026-01-13.rda')
out.ns.re <- out
summary(out.ns.re)
# Spatial null ------------------------
load('results/ba-univ-spatial-only-1e+05-samples-2026-01-13.rda')
out.sp.only <- out
summary(out.sp.only)
# Spatial no RE -----------------------
load('results/ba-univ-spatial-noRE-1e+05-samples-2026-01-13.rda')
out.sp <- out
summary(out.sp)
# Spatial RE --------------------------
load('results/ba-univ-spatial-1e+05-samples-2026-01-13.rda')
out.sp.re <- out
summary(out.sp.re)

# Stage 2 Model Assessment ------------------------------------------------
# TODO: can change the model as desired. 
curr.mod <- out.sp.only
y.rep.quants <- apply(curr.mod$y.rep.samples, c(2), quantile, c(0.025, 0.5, 0.975))

max.val <- max(c(data.list$y, y.rep.quants))
min.val <- max(c(data.list$y, y.rep.quants))
plot(data.list$y, y.rep.quants[2, ], pch = 19,
     xlab = 'True', ylab = 'Fitted', 
     ylim = c(min.val, max.val), xlim = c(min.val, max.val))
abline(0, 1)
print(cor(data.list$y, y.rep.quants[2, ]))

# WAIC --------------------------------------------------------------------
waicAbund(out.ns.only)
waicAbund(out.ns)
waicAbund(out.ns.re)
# TODO: this needs to be fixed in spAbundance, particularly before any new update 
#       is rolled out. Currently, just manipulating this myself. 
out.sp.only$like.samples <- array(c(out.sp.only$like.samples), dim = c(dim(out.sp.only$like.samples), 1))
waicAbund(out.sp.only)
out.sp$like.samples <- array(out.sp$like.samples, dim = c(dim(out.sp$like.samples), 1))
waicAbund(out.sp)
out.sp.re$like.samples <- array(out.sp.re$like.samples, dim = c(dim(out.sp.re$like.samples), 1))
waicAbund(out.sp.re)

# RMSE (random hold out) --------------
# TODO: need to update for working with BA. 
apply(rmspe.ests, 2, mean)
