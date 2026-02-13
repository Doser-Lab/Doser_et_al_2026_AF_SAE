# 05-predict.R: script to generate small area estimates across
#                        provinces in Eastern Afghanistan 
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
rangeland.mean <- mean(data.list.1$covs$range_cov)
rangeland.sd <- sd(data.list.1$covs$range_cov)
bareground.mean <- mean(data.list.1$covs$bareland_cov)
bareground.sd <- sd(data.list.1$covs$bareland_cov)

# Load Stage 2 data -------------------------------------------------------
load('data/spAbundance_data.rda')

# Load resulting model objects --------------------------------------------
# TODO: may need to change these later on depending on what the model assessment 
#       results suggest. 
# Stage 1
load('results/stage-1-spatial-1e+05-samples-4-factors-2026-01-08.rda')
out.1 <- out
# Stage 2
load('results/stage-2-spatial-1e+05-samples-4-factors-2026-01-08.rda')
out.2 <- out

# Read in prediction object -----------------------------------------------
# TODO: change the resolution here as desired. 
curr.res <- 2000
load(paste0('data/prediction_data_', curr.res, 'm.rda'))
ppt.pred <- (pred.covs.df$ppt - ppt.mean) / ppt.sd
elev.pred <- (pred.covs.df$elev - Elevation.mean) / Elevation.sd
forest.pred <- (pred.covs.df$forest - forest.mean) / forest.sd
rangeland.pred <- (pred.covs.df$rangeland - rangeland.mean) / rangeland.sd
bareground.pred <- (pred.covs.df$bareground - bareground.mean) / bareground.sd
X.0.stage.1 <- cbind(1, elev.pred, elev.pred^2, forest.pred, rangeland.pred, 
                     bareground.pred, ppt.pred, ppt.pred^2, 
                     pred.covs.df$Province_num)
X.0.stage.1[, -c(1, ncol(X.0.stage.1))] <- 0
X.0.stage.2 <- cbind(1, elev.pred, elev.pred^2, forest.pred, ppt.pred, ppt.pred^2, 
                     pred.covs.df$Province_num)
X.0.stage.2[, -c(1, ncol(X.0.stage.2))] <- 0
dimnames(X.0.stage.1)[[2]] <- c(dimnames(out.1$X)[[2]], dimnames(out.1$X.re)[[2]])
dimnames(X.0.stage.2)[[2]] <- c(dimnames(out.2$X)[[2]], dimnames(out.2$X.re)[[2]])
# Predict for one district at a time --------------------------------------
# Minimum number of saved posterior samples from each stage. 
n.samples <- min(out.1$n.post, out.2$n.post)
# District information
district <- pred.covs.df$District
n.districts <- length(unique(district))
unique.districts <- unique(district)
n.sp <- nrow(data.list.1$y)
J <- nrow(X.0.stage.1)
# Save posterior samples for each species in each county.
basal.area.mean.samples <- array(NA, dim = c(n.samples, n.sp, n.districts))
psi.quants <- array(NA, dim = c(5, n.sp, J))
w.quants.1 <- array(NA, dim = c(5, out.1$q, J))
w.quants.2 <- array(NA, dim = c(5, out.2$q, J))
ba.quants <- array(NA, dim = c(5, n.sp, J))
forest.indx <- which(pred.covs.df$forest != 0)
for (j in 1:n.districts) {
  # Stage 1 ---------------------------
  print(paste0("Currently on county ", j, " out of ", n.districts))
  curr.indx <- which(district == unique.districts[j])
  J.0 <- length(curr.indx)
  # Only predict in cells that are classified as forestland. 
  curr.indx <- curr.indx[which(curr.indx %in% forest.indx)]
  J.0 <- length(curr.indx)
  basal.area.samples <- array(NA, dim = c(n.samples, n.sp, J.0))
  if (length(curr.indx) > 0) {
    out.pred.1 <- predict(out.1, X.0.stage.1[curr.indx, , drop = FALSE], 
                          coords.pred.mat[curr.indx, , drop = FALSE],
                          n.omp.threads = 5, verbose = FALSE)
    psi.quants[, , curr.indx] <- apply(out.pred.1$psi.0.samples, c(2, 3), quants)
    w.quants.1[, , curr.indx] <- apply(out.pred.1$w.0.samples, c(2, 3), quants)
    z.samples <- out.pred.1$z.0.samples
  }
  rm(out.pred.1)
  gc()
  # Stage 2 ---------------------------
  # Only generate predictions in cells that are classified as forest.
  if (length(curr.indx) > 0) {
    out.pred.2 <- predict(out.2, X.0.stage.2[curr.indx, , drop = FALSE], 
                          coords.pred.mat[curr.indx, , drop = FALSE],
                          n.omp.threads = 5, verbose = FALSE,
                          z.0.samples = z.samples)
    # Get basal.area samples ---------------
    for (i in 1:n.sp) {
      basal.area.samples[, i, ] <- ifelse(z.samples[1:n.samples, i, ] == 1,
                                          exp(out.pred.2$y.0.samples[, i, ]),
                                          out.pred.2$y.0.samples[, i, ])
    }
    ba.quants[, , curr.indx] <- apply(basal.area.samples, c(2, 3), quants)
    w.quants.2[, , curr.indx] <- apply(out.pred.2$w.0.samples, c(2, 3), quants)
    basal.area.mean.samples[, , j] <- apply(basal.area.samples, c(1, 2), mean)
  }
  rm(z.samples, out.pred.2, basal.area.samples)
  gc()
}
# Can ignore the warnings from above, they just come from removing objects 
# that don't exist. 
save(psi.quants, ba.quants, basal.area.mean.samples, w.quants.1, w.quants.2, 
     file = paste0('results/test_top_model_prediction_results_', curr.res, 'm.rda'))
