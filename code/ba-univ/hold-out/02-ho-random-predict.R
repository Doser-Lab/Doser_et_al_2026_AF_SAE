# 05-ho-random-predict.R: script to predict presence/absence and biomass at 
#                         hold-out locations used in the analysis. 
rm(list = ls())
library(spOccupancy)
library(spAbundance)

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
load(paste0(data.dir, "ba_random_holdout_fit.rda"))
elev.mean <- mean(data.fit$covs$Elevation)
elev.sd <- sd(data.fit$covs$Elevation)
ppt.mean <- mean(data.fit$covs$ppt)
ppt.sd <- sd(data.fit$covs$ppt)

# Load model objects ------------------------------------------------------
# NOTE: hardcoded. Will need to change file names as needed
n.models <- 6
models <- c("ho-ba-univ-nonspatial-only-1e+05-samples-2026-01-30.rda", 
            "ho-ba-univ-nonspatial-noRE-1e+05-samples-2026-01-30.rda", 
            "ho-ba-univ-nonspatial-1e+05-samples-2026-01-30.rda", 
            "ho-ba-univ-spatial-only-1e+05-samples-2026-01-30.rda", 
            "ho-ba-univ-spatial-noRE-1e+05-samples-2026-01-30.rda", 
            "ho-ba-univ-spatial-1e+05-samples-2026-01-30.rda") 

# Load the prediction data and standardize things accordingly
load(paste0(data.dir, 'ba_random_holdout_hold.rda'))
elev.pred <- (data.hold$covs$Elevation - elev.mean) / elev.sd
ppt.pred <- (data.hold$covs$ppt - ppt.mean) / ppt.sd
X.0 <- cbind(1, elev.pred, elev.pred^2, ppt.pred, ppt.pred^2, 
             data.hold$covs$Province_num)
dimnames(X.0)[[2]] <- c('(Intercept)', 'scale(Elevation)', 'I(scale(Elevation)^2)', 
                        'scale(ppt)', 'I(scale(ppt)^2)', 'Province_num')
coords.0 <- data.hold$coords
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
rmspe.ests <- rep(NA, n.models)
for (l in 1:n.models) {
  print(paste0("Currently on model ", l, " out of ", n.models))
  load(paste0(out.dir, models[l]))
  n.samples <- out$n.post * out$n.chains
  rmspe.vals <- rep(NA, n.samples)
  out.pred <- predict(out, X.0.list[[l]], coords.0, verbose = FALSE, ignore.RE = FALSE) 
  for (j in 1:n.samples) {
    rmspe.vals[j] <- sqrt(mean((data.hold$y - out.pred$y.0.samples[j, , 1])^2))
  }
  rmspe.ests[l] <- mean(rmspe.vals)
}
rmspe.ests

# Save results to hard drive ----------------------------------------------
save(rmspe.ests, file = 'results/ba-ho-random-rmspe.rda')
