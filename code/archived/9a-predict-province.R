# 4a-predict-province.R: script to generate small area estimates across
#                        10 provinces in Eastern Afghanistan 
# Author: Jeffrey W. Doser
rm(list = ls())
library(spOccupancy)
library(spAbundance)

# Functions and CRS -------------------------------------------------------
quants <- function(x){
  quantile(x, prob=c(0.5, 0.025, 0.25, 0.75, 0.975), na.rm = TRUE)
}
my.crs <- 32642

# Load Stage 1 data -------------------------------------------------------
load('data/spOccupancy_data.rda')
ppt.mean <- mean(data.list.1$covs$ppt)
ppt.sd <- sd(data.list.1$covs$ppt)
Elevation.mean <- mean(data.list.1$covs$Elevation)
Elevation.sd <- sd(data.list.1$covs$Elevation)
forest.mean <- mean(data.list.1$covs$forest)
forest.sd <- sd(data.list.1$covs$forest)

# Load Stage 2 data -------------------------------------------------------
load('data/spAbundance_data.rda')
# Same covariates in Stage 2 as those used in Stage 1, so don't need to do 
# any standardizing. 

# Load resulting model objects --------------------------------------------
# Stage 1
load('results/stage-1-spatial-1e+05-samples-4-factors-2025-03-05.rda')
out.1 <- out
# Stage 2
load('results/stage-2-spatial-1e+05-samples-4-factors-2025-03-05.rda')
out.2 <- out

# Read in prediction object -----------------------------------------------
load('data/prediction_data.rda')
ppt.pred <- (pred.covs.df$ppt - ppt.mean) / ppt.sd
elev.pred <- (pred.covs.df$elev - Elevation.mean) / Elevation.sd
forest.pred <- (pred.covs.df$forest - forest.mean) / forest.sd
X.0 <- cbind(1, elev.pred, elev.pred^2, forest.pred, ppt.pred, ppt.pred^2, 
             pred.covs.df$Province_num)
dimnames(X.0)[[2]] <- c(dimnames(out.1$X)[[2]], dimnames(out.1$X.re)[[2]])
# Predict for one province at a time --------------------------------------
# Minimum number of saved posterior samples from each stage. 
n.samples <- min(out.1$n.post, out.2$n.post)
province <- pred.covs.df$Province
n.provinces <- length(unique(province))
unique.provinces <- unique(province)
n.sp <- nrow(data.list.1$y)
# Save posterior samples for each species in each county.
biomass.mean.samples <- array(NA, dim = c(n.samples, n.sp, n.provinces))
for (j in 1:n.provinces) {
  # Stage 1 ---------------------------
  print(paste0("Currently on county ", j, " out of ", n.provinces))
  curr.indx <- which(province == unique.provinces[j])
  J.0 <- length(curr.indx)
  biomass.samples <- array(NA, dim = c(n.samples, n.sp, J.0))
  out.pred.1 <- predict(out.1, X.0[curr.indx, ], coords.pred.mat[curr.indx, ],
                        n.omp.threads = 5, verbose = FALSE)
  # Stage 2 ---------------------------
  out.pred.2 <- predict(out.2, X.0[curr.indx, ], coords.pred.mat[curr.indx, ],
                        n.omp.threads = 5, verbose = FALSE,
                        z.0.samples = out.pred.1$z.0.samples[1:n.samples, , ])
  # Get biomass samples ---------------
  for (i in 1:n.sp) {
    biomass.samples[, i, ] <- ifelse(out.pred.1$z.0.samples[1:n.samples, i, ] == 1,
                                     exp(out.pred.2$y.0.samples[, i, ]),
                                     out.pred.2$y.0.samples[, i, ])
  }
  biomass.mean.samples[, , j] <- apply(biomass.samples, c(1, 2), mean)
  rm(out.pred.1, out.pred.2, biomass.samples)
  gc()
}
save(biomass.mean.samples, file = 'results/biomass_sae_province_means.rda')


