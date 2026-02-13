# 04-predict.R: script to generate small area estimates across
#               provinces in Eastern Afghanistan 
# Author: Jeffrey W. Doser
rm(list = ls())
library(spOccupancy)
# NOTE: this is the local version of spAbundance to accommodate a dimensions 
#       problem. 
library(spAbundance)

# Directories -------------------------------------------------------------
# Determine what machine you're on and change directories as needed.
machine.name <- Sys.info()['nodename']
if (machine.name == 'pop-os') {
  out.dir <- 'results/'
  data.dir <- 'data/'
} else { # Running on NCSU machine
  out.dir <- '/share/doserlab/jwdoser/DSKW25/results/'
  data.dir <- '/share/doserlab/jwdoser/DSKW25/data/'
}

# Functions and CRS -------------------------------------------------------
quants <- function(x){
  quantile(x, prob=c(0.5, 0.025, 0.25, 0.75, 0.975), na.rm = TRUE)
}
my.crs <- 32642

# Load data ---------------------------------------------------------------
load(paste0(data.dir, 'ba_spAbundance_data.rda'))
ppt.mean <- mean(data.list$covs$ppt)
ppt.sd <- sd(data.list$covs$ppt)
Elevation.mean <- mean(data.list$covs$Elevation)
Elevation.sd <- sd(data.list$covs$Elevation)
forest.mean <- mean(data.list$covs$forest)
forest.sd <- sd(data.list$covs$forest)

# Load resulting model objects --------------------------------------------
# TODO: may need to change this depending on what the top model suggests. 
load(paste0(out.dir, 'ba-univ-spatial-noRE-1e+05-samples-2026-01-13.rda'))

pred.files <- list.files(paste0(data.dir, "pred_pieces"))
res.files <- sapply(pred.files, function(a) substr(a, nchar(a) - 7, nchar(a))) 
pred.files <- pred.files[which(res.files == '100m.rda')]
n.districts <- length(pred.files)

# Read in prediction object -----------------------------------------------
# Save posterior samples for each species in each county.
n.samples <- out$n.post 
basal.area.mean.samples <- array(NA, dim = c(n.samples, n.districts))
for (j in 1:n.districts) {
  print(paste0("Currently on county ", j, " out of ", n.districts))
  # Notice the use of the 100m resolution data
  load(paste0(data.dir, 'pred_pieces/', pred.files[j]))
  if (is.null(pred.covs.df)) next
  n.0 <- nrow(pred.covs.df)
  ppt.pred <- (pred.covs.df$ppt - ppt.mean) / ppt.sd
  elev.pred <- (pred.covs.df$elev - Elevation.mean) / Elevation.sd
  # TODO: switch back if using the full model
  # X.0 <- cbind(1, elev.pred, elev.pred^2, ppt.pred, ppt.pred^2, 
  #                      pred.covs.df$Province_num)
  # dimnames(X.0)[[2]] <- c(dimnames(out$X)[[2]], dimnames(out$X.re)[[2]])
  X.0 <- cbind(1, elev.pred, elev.pred^2, ppt.pred, ppt.pred^2)
  dimnames(X.0)[[2]] <- c(dimnames(out$X)[[2]])
  vals <- split(1:n.0, ceiling(seq_along(1:n.0) / 100))
  # Stage 1 ---------------------------
  basal.area.samples <- array(NA, dim = c(n.samples, n.0))
  for (l in 1:length(vals)) {
    print(paste0("Currently on piece ", l, " out of ", length(vals)))
    curr.indx <- vals[[l]]
    out.pred <- predict(out, X.0[curr.indx, , drop = FALSE], 
                        coords.pred.mat[curr.indx, , drop = FALSE],
                        n.omp.threads = 3, verbose = FALSE)
    basal.area.samples[, curr.indx] <- matrix(out.pred$y.0.samples^4, 
                                              n.samples, length(curr.indx))
  }
  basal.area.mean.samples[, j] <- apply(basal.area.samples, 1, mean)
  rm(basal.area.samples)
  gc()
}
# Can ignore the warnings from above, they just come from removing objects 
# that don't exist. 
save(basal.area.mean.samples,  
     file = 'results/ba_univ_top_model_SAE_100m.rda')
