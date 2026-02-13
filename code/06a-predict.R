# 06a-predict.R: script to generate small area estimates across districts in 
#                Eastern Afghanistan. 
# Author: Jeffrey W. Doser
rm(list = ls())
library(spOccupancy)
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
  quantile(x, prob=c(0.5, 0.025, 0.975), na.rm = TRUE)
}
my.crs <- 32642

# Load Stage 1 data -------------------------------------------------------
load(paste0(data.dir, 'spOccupancy_data.rda'))
ppt.mean <- mean(data.list.1$covs$ppt)
ppt.sd <- sd(data.list.1$covs$ppt)
Elevation.mean <- mean(data.list.1$covs$Elevation)
Elevation.sd <- sd(data.list.1$covs$Elevation)

# Load Stage 2 data -------------------------------------------------------
load(paste0(data.dir, 'spAbundance_data.rda'))

pred.files <- list.files(paste0(data.dir, "pred_pieces"))
res.files <- sapply(pred.files, function(a) substr(a, nchar(a) - 7, nchar(a)))
pred.files <- pred.files[which(res.files == '100m.rda')]
n.districts <- length(pred.files)
# Load resulting model objects --------------------------------------------
# Stage 1
load(paste0(out.dir, 'stage-1-spatial-2e+05-samples-4-factors-2026-01-09.rda'))
out.1 <- out
# Stage 2
load(paste0(out.dir, 'stage-2-spatial-2e+05-samples-4-factors-2026-01-09.rda'))
out.2 <- out


# Read in prediction object -----------------------------------------------
# Save posterior samples for each species in each county.
n.samples <- min(out.1$n.post, out.2$n.post)
n.sp <- nrow(data.list.1$y)
basal.area.mean.samples <- array(NA, dim = c(n.samples, n.sp, n.districts))
for (j in 1:n.districts) {
  print(paste0("Currently on county ", j, " out of ", n.districts))
  load(paste0(data.dir, 'pred_pieces/', pred.files[j]))
  if (is.null(pred.covs.df)) next
  n.0 <- nrow(pred.covs.df)
  ppt.pred <- (pred.covs.df$ppt - ppt.mean) / ppt.sd
  elev.pred <- (pred.covs.df$elev - Elevation.mean) / Elevation.sd
  X.0 <- cbind(1, elev.pred, elev.pred^2, ppt.pred, ppt.pred^2, 
                       pred.covs.df$Province_num)
  dimnames(X.0)[[2]] <- c(dimnames(out.1$X)[[2]], dimnames(out.1$X.re)[[2]])
  vals <- split(1:n.0, ceiling(seq_along(1:n.0) / 500))
  # Stage 1 ---------------------------
  basal.area.samples <- array(NA, dim = c(n.samples, n.sp, n.0))
  for (l in 1:length(vals)) {
    print(paste0("Currently on piece ", l, " out of ", length(vals)))
    curr.indx <- vals[[l]]
    out.pred.1 <- predict(out.1, X.0[curr.indx, , drop = FALSE], 
                          coords.pred.mat[curr.indx, ],
                          n.omp.threads = 5, verbose = FALSE)
    z.samples <- out.pred.1$z.0.samples
    rm(out.pred.1)
    gc()
    # Stage 2 ---------------------------
    out.pred.2 <- predict(out.2, X.0[curr.indx, , drop = FALSE], coords.pred.mat[curr.indx, ],
                          n.omp.threads = 5, verbose = FALSE,
                          z.0.samples = z.samples)
    # Get basal.area samples ---------------
    for (i in 1:n.sp) {
      basal.area.samples[, i, curr.indx] <- ifelse(z.samples[1:n.samples, i, ] == 1,
                                                   exp(out.pred.2$y.0.samples[, i, ]),
                                                   out.pred.2$y.0.samples[, i, ])
    }
  }
  basal.area.mean.samples[, , j] <- apply(basal.area.samples, c(1, 2), mean)
  rm(z.samples, out.pred.2, basal.area.samples)
  gc()
}
# Can ignore the warnings from above, they just come from removing objects 
# that don't exist. 
save(basal.area.mean.samples, 
     file = paste0(out.dir, 'top_model_SAE_100m.rda'))
