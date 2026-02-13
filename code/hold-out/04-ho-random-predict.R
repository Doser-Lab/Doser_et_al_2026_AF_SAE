# 05-ho-random-predict.R: script to predict presence/absence and biomass at 
#                         hold-out locations used in the analysis. 
rm(list = ls())
library(spOccupancy)
library(spAbundance)
library(pROC)

# NOTE: this script will not run using the files available on GitHub, because
# the results files are too large to include on GitHub. Instead, if you run
# the previous main model running scripts, you can read in the output file into this file
# to use in subsequent predictions.

# Specify directory -------------------------------------------------------
# NOTE: this is used to determine the directories for reading in and writing out
#       data. You should set out.dir and data.dir depending on where you are
#       running this code.
machine.name <- Sys.info()['nodename']
if (machine.name == 'pop-os') {
  out.dir <- 'results/'
  data.dir <- 'data/'
} else { # Running on NCSU HPC
  out.dir <- '/share/doserlab/jwdoser/DSKW24/results/'
  data.dir <- '/share/doserlab/jwdoser/DSKW24/data/'
}

# Functions and CRS -------------------------------------------------------
quants <- function(x){
  quantile(x, prob=c(0.5, 0.025, 0.25, 0.75, 0.975), na.rm = TRUE)
}

# Load data from model fitting --------------------------------------------
load(paste0(data.dir, "spOccupancy_random_holdout_fit.rda"))
load(paste0(data.dir, "spAbundance_random_holdout_fit.rda"))
elev.mean <- mean(data.fit.1$covs$Elevation)
elev.sd <- sd(data.fit.1$covs$Elevation)
ppt.mean <- mean(data.fit.1$covs$ppt)
ppt.sd <- sd(data.fit.1$covs$ppt)

# Load model objects ------------------------------------------------------
# NOTE: hardcoded. Will need to change file names as needed
n.models <- 6
stage.1.models <- c('ho-random-stage-1-nonspatial-only-2e+05-samples-4-factors-2026-01-09.rda', 
                    'ho-random-stage-1-nonspatial-noRE-2e+05-samples-4-factors-2026-01-09.rda', 
                    'ho-random-stage-1-nonspatial-2e+05-samples-4-factors-2026-01-09.rda', 
                    'ho-random-stage-1-spatial-only-1e+05-samples-4-factors-2026-01-09.rda', 
                    'ho-random-stage-1-spatial-noRE-2e+05-samples-4-factors-2026-01-09.rda', 
                    'ho-random-stage-1-spatial-2e+05-samples-4-factors-2026-01-09.rda')
stage.2.models <- c('ho-random-stage-2-nonspatial-only-2e+05-samples-4-factors-2026-01-09.rda', 
                    'ho-random-stage-2-nonspatial-noRE-2e+05-samples-4-factors-2026-01-09.rda', 
                    'ho-random-stage-2-nonspatial-2e+05-samples-4-factors-2026-01-09.rda', 
                    'ho-random-stage-2-spatial-only-2e+05-samples-4-factors-2026-01-09.rda', 
                    'ho-random-stage-2-spatial-noRE-2e+05-samples-4-factors-2026-01-09.rda', 
                    'ho-random-stage-2-spatial-2e+05-samples-4-factors-2026-01-09.rda')
# Load the prediction data and standardize things accordingly
load(paste0(data.dir, 'spOccupancy_random_holdout_hold.rda'))
load(paste0(data.dir, 'spAbundance_random_holdout_hold.rda'))
elev.pred <- (data.hold.1$covs$Elevation - elev.mean) / elev.sd
ppt.pred <- (data.hold.1$covs$ppt - ppt.mean) / ppt.sd
X.0 <- cbind(1, elev.pred, elev.pred^2, ppt.pred, ppt.pred^2, 
             data.hold.1$covs$Province_num)
dimnames(X.0)[[2]] <- c('(Intercept)', 'scale(Elevation)', 'I(scale(Elevation)^2)', 
                        'scale(ppt)', 'I(scale(ppt)^2)', 'Province_num')
coords.0 <- data.hold.1$coords
# Subset the covariate matrix depending on the model
X.0.intercept <- X.0[, 1, drop = FALSE]
X.0.no.re <- X.0[, -ncol(X.0), drop = FALSE]
X.0.list <- list()
X.0.list[[1]] <- X.0.intercept
X.0.list[[4]] <- X.0.intercept
X.0.list[[2]] <- X.0.no.re
X.0.list[[5]] <- X.0.no.re
X.0.list[[3]] <- X.0
X.0.list[[6]] <- X.0

# Do the prediction -------------------------------------------------------
# Do the prediction for all models, one by one.
N <- nrow(data.fit.1$y)
auc.ests <- matrix(NA, N, n.models)
rmspe.ests <- matrix(NA, N, n.models)
for (l in 1:n.models) {
  # Stage 1 ---------------------------
  load(paste0(out.dir, stage.1.models[l]))
  out.1 <- out
  n.samples <- out.1$n.post * out.1$n.chains
  auc.vals <- matrix(NA, n.samples, N)
  out.pred.1 <- predict(out.1, X.0.list[[l]], coords.0, verbose = FALSE) 
  for (i in 1:N) {
    for (j in 1:n.samples) {
      auc.vals[j, i] <- auc(response = data.hold.1$y[i, ],
		  	  predictor = out.pred.1$z.0.samples[j, i, ])
    }
  }
  auc.ests[, l] <- apply(auc.vals, 2, mean)
  # Stage 2 ---------------------------
  load(paste0(out.dir, stage.2.models[l]))
  out.2 <- out
  rmspe.vals <- matrix(NA, n.samples, N)
  out.pred.2 <- predict(out.2, X.0.list[[l]], coords.0, verbose = FALSE, 
                        z.0.samples = out.pred.1$z.0.samples)
  for (i in 1:N) {
    for (j in 1:n.samples) {
      rmspe.vals[j, i] <- sqrt(mean((data.hold.2$y[i, ] - out.pred.2$y.0.samples[j, i, ])^2))
    }
  }
  rmspe.ests[, l] <- apply(rmspe.vals, 2, mean)
}
apply(auc.ests, 2, mean)
apply(rmspe.ests, 2, mean)

# Save results to hard drive ----------------------------------------------
save(auc.ests, rmspe.ests, file = 'results/ho-random-auc-rmspe.rda')
