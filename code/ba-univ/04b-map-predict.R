# 04-predict.R: script to generate small area estimates across
#               provinces in Eastern Afghanistan 
# Author: Jeffrey W. Doser
rm(list = ls())
library(spOccupancy)
# NOTE: this is the local version of spAbundance to accommodate a dimensions 
#       problem. 
library(spAbundance)
library(sf)
library(stars)

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
  quantile(x, prob=c(0.5, 0.025, 0.975), na.rm = TRUE)
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
# Load land use data to only predict at forest sites. 
landuse.dat <- read_stars('data/land-use-data/data/LANDCOVER_2018.tif')

# Read in prediction object -----------------------------------------------
load("data/map_prediction_data_1000m.rda")
n.0 <- nrow(pred.covs.df)
ba.quants <- array(NA, dim = c(3, n.0))
# Save posterior samples for each species in each county.
n.samples <- out$n.post 
ppt.pred <- (pred.covs.df$ppt - ppt.mean) / ppt.sd
elev.pred <- (pred.covs.df$elev - Elevation.mean) / Elevation.sd
X.0 <- cbind(1, elev.pred, elev.pred^2, ppt.pred, ppt.pred^2)
dimnames(X.0)[[2]] <- c(dimnames(out$X)[[2]])
# TODO: switch back if need be. 
# X.0 <- cbind(1, elev.pred, elev.pred^2, ppt.pred, ppt.pred^2, 
#                      pred.covs.df$Province_num)
# dimnames(X.0)[[2]] <- c(dimnames(out$X)[[2]], dimnames(out$X.re)[[2]])
vals <- split(1:n.0, ceiling(seq_along(1:n.0) / 500))
# Stage 1 ---------------------------
for (l in 1:length(vals)) {
  print(paste0("Currently on piece ", l, " out of ", length(vals)))
  curr.indx <- vals[[l]]
  lc.vals <- st_extract(landuse.dat, at = coords.pred.sf[curr.indx, ])
  # Figure out pixels that fall within land classified  
  forest.indx <- which(lc.vals$LANDCOVER_2018.tif == 4)
  curr.indx <- curr.indx[forest.indx]
  if (length(curr.indx) > 0) {
    out.pred <- predict(out, X.0[curr.indx, , drop = FALSE], 
                        coords.pred.mat[curr.indx, , drop = FALSE],
                        n.omp.threads = 3, verbose = FALSE)
    basal.area.samples <- matrix(out.pred$y.0.samples^4, 
                                 n.samples, length(curr.indx))
    ba.quants[, curr.indx] <- apply(basal.area.samples, 2, quants)
  }
}

# Save results to hard drive 
save(ba.quants, file = 'results/ba_univ_top_model_prediction_1000m.rda')
