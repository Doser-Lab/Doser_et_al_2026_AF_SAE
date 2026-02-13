# 05b-convergence-assessment.R: script to do some basic exploration of model fit
#                               results, including convergence assessment and residual
#                               diagnostics. 
# Author: Jeffrey W. Doser
rm(list = ls())
library(spOccupancy)
library(spAbundance)
library(coda)
library(tidyverse)
library(pROC)

# Load the data -----------------------------------------------------------
load('data/spOccupancy_data.rda')
load('data/spAbundance_data.rda')
sp.names <- dimnames(data.list.1$y)[[1]]
N <- nrow(data.list.2$y)
# NOTE: hardcoded
n.models <- 6

# Stage 1 Convergence Assessment ------------------------------------------
# Non-spatial null --------------------
# ALL CONVERGED
load('results/stage-1-nonspatial-only-2e+05-samples-4-factors-2026-01-09.rda')
out.ns.only.1 <- out
summary(out.ns.only.1)
# Non-spatial no RE -------------------
# ALL CONVERGED
load('results/stage-1-nonspatial-noRE-2e+05-samples-4-factors-2026-01-09.rda')
out.ns.1 <- out
summary(out.ns.1)
# Non-spatial RE ----------------------
# ALL CONVERGED
load('results/stage-1-nonspatial-2e+05-samples-4-factors-2026-01-09.rda')
out.ns.re.1 <- out
summary(out.ns.re.1)
# Spatial null ------------------------
load('results/stage-1-spatial-only-2e+05-samples-4-factors-2026-01-09.rda')
out.sp.only.1 <- out
summary(out.sp.only.1)
# Spatial no RE -----------------------
load('results/stage-1-spatial-noRE-2e+05-samples-4-factors-2026-01-09.rda')
out.sp.1 <- out
summary(out.sp.1)
# Spatial RE --------------------------
# CONVEGED
load('results/stage-1-spatial-2e+05-samples-4-factors-2026-01-09.rda')
out.sp.re.1 <- out
summary(out.sp.re.1)


# Stage 2 Convergence Assessment ------------------------------------------
# TODO: all spatial models might need a bit more, but they are generally quite good.
# Non-spatial null --------------------
# ALL CONVERGED
load('results/stage-2-nonspatial-only-2e+05-samples-4-factors-2026-01-09.rda')
out.ns.only.2 <- out
summary(out.ns.only.2)
# Non-spatial no RE -------------------
# ALL CONVERGED
load('results/stage-2-nonspatial-noRE-2e+05-samples-4-factors-2026-01-09.rda')
out.ns.2 <- out
summary(out.ns.2)
# Non-spatial RE ----------------------
# ALL CONVERGED
load('results/stage-2-nonspatial-2e+05-samples-4-factors-2026-01-09.rda')
out.ns.re.2 <- out
summary(out.ns.re.2)
# Spatial null ------------------------
load('results/stage-2-spatial-only-2e+05-samples-4-factors-2026-01-09.rda')
out.sp.only.2 <- out
summary(out.sp.only.2)
# Spatial no RE -----------------------
load('results/stage-2-spatial-noRE-2e+05-samples-4-factors-2026-01-09.rda')
out.sp.2 <- out
summary(out.sp.2)
# Spatial RE --------------------------
load('results/stage-2-spatial-2e+05-samples-4-factors-2026-01-09.rda')
out.sp.re.2 <- out
summary(out.sp.re.2)

# Stage 2 Model Assessment ------------------------------------------------
# TODO: change the model you're using here as needed
top.2 <- out.sp.re.2
y.rep.quants <- apply(top.2$y.rep.samples, c(2, 3), quantile, c(0.025, 0.5, 0.975))

par(mfrow = c(1, 2))
for (i in 1:N) {
  keep.indx <- which(data.list.2$y[i, ] != 0)
  max.val <- max(c(data.list.2$y[i, keep.indx], 
                   y.rep.quants[2, i, keep.indx]))
  min.val <- min(c(data.list.2$y[i, keep.indx], 
                   y.rep.quants[2, i, keep.indx]))
  plot(data.list.2$y[i, keep.indx], y.rep.quants[2, i, keep.indx], pch = 19,
       xlab = 'True', ylab = 'Fitted', main = sp.names[i], 
       ylim = c(min.val, max.val), xlim = c(min.val, max.val))
  abline(0, 1)
  hist(data.list.2$y[i, keep.indx], main = sp.names[i])
  hist(y.rep.quants[2, i, keep.indx], add = TRUE, col = 'red')
  print(cor(data.list.2$y[i, keep.indx], y.rep.quants[2, i, keep.indx]))
  Sys.sleep(1)
}

# A bit of funkiness here, but things generally seem alright. Correlation 
# coefficients from above suggest general good pattern, but that the model 
# tends to underpredict at high values and overpredict at low values.
resids.2 <- data.list.2$y - y.rep.quants[2, , ]
par(mfrow = c(1, 3))
for (i in 1:N) {
  keep.indx <- which(data.list.2$y[i, ] != 0)
  plot(y.rep.quants[2, i, keep.indx], resids.2[i, keep.indx], pch = 19,
       xlab = 'Fitted', ylab = 'Residuals', main = sp.names[i])
  abline(h = 0, lty = 2, col = 'grey')
  hist(resids.2[i, keep.indx], main = sp.names[i])
  qqnorm(resids.2[i, keep.indx], main = sp.names[i])
  qqline(resids.2[i, keep.indx], main = sp.names[i])
  Sys.sleep(2)
}
par(mfrow = c(1, 1))

# Stage 1 Model Comarison -------------------------------------------------

# WAIC --------------------------------
waic.ests.1 <- matrix(NA, N, n.models)
waic.ests.1[, 1] <- waicOcc(out.ns.only.1, by.sp = TRUE)[, 3]
waic.ests.1[, 2] <- waicOcc(out.ns.1, by.sp = TRUE)[, 3]
waic.ests.1[, 3] <- waicOcc(out.ns.re.1, by.sp = TRUE)[, 3]
waic.ests.1[, 4] <- waicOcc(out.sp.only.1, by.sp = TRUE)[, 3]
waic.ests.1[, 5] <- waicOcc(out.sp.1, by.sp = TRUE)[, 3]
waic.ests.1[, 6] <- waicOcc(out.sp.re.1, by.sp = TRUE)[, 3]

waic.ests.1
apply(waic.ests.1, 2, sum)

# AUC (random hold out) ---------------
load('results/ho-random-auc-rmspe.rda')
apply(auc.ests, 2, mean)

# Stage 2 Model Comparison ------------------------------------------------
# WAIC --------------------------------
waic.ests.2 <- matrix(NA, N, n.models)
waic.ests.2[, 1] <- waicAbund(out.ns.only.2, by.sp = TRUE)[, 3]
waic.ests.2[, 2] <- waicAbund(out.ns.2, by.sp = TRUE)[, 3]
waic.ests.2[, 3] <- waicAbund(out.ns.re.2, by.sp = TRUE)[, 3]
waic.ests.2[, 4] <- waicAbund(out.sp.only.2, by.sp = TRUE)[, 3]
waic.ests.2[, 5] <- waicAbund(out.sp.2, by.sp = TRUE)[, 3]
waic.ests.2[, 6] <- waicAbund(out.sp.re.2, by.sp = TRUE)[, 3]

waic.ests.2
apply(waic.ests.2, 2, sum)

# RMSE (random hold out) --------------
apply(rmspe.ests, 2, mean)
