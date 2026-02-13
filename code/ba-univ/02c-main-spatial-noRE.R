# 02c-main-spatial-noRE.R: script to fit the spatial linear mixed model 
#                          without the province-level random effects. 
rm(list = ls())
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

# Read in the data set ----------------------------------------------------
load(paste0(data.dir, 'ba_spAbundance_data.rda'))

# Specify model inputs ----------------------------------------------------
# Priors ------------------------------
dist.mat <- dist(data.list.2$coords)
# The average distance between provinces in the study area.
high.dist <- 152116.2
# Setting to 1km to avoid overfitting.
low.dist <- 1000
prior.list <- list(beta.normal = list(mean = 0, var = 10),
                   tau.sq.ig = c(a = 0.001, b = 0.001),
                   phi.unif = c(3 / high.dist, 3 / low.dist))

# Starting values
inits.list <- list(beta = 0, tau.sq = 0.5,
                   phi = 3 / mean(c(high.dist, low.dist)))
# Tuning
tuning.list <- list(phi = 0.75)

# MCMC criteria
n.batch <- 4000
batch.length <- 25
n.burn <- 40000
n.thin <- 20
n.chains <- 1

# Fit the model -----------------------------------------------------------
out <- spAbund(formula = ~ scale(Elevation) + I(scale(Elevation)^2) + 
                           scale(ppt) + I(scale(ppt)^2), 
               data = data.list,
               n.batch = n.batch,
               batch.length = batch.length,
               inits = inits.list,
               family = 'Gaussian',
               tuning = tuning.list,
               n.neighbors = 15,
               cov.model = 'exponential',
               priors = prior.list,
               accept.rate = 0.43,
               n.omp.threads = 1,
               verbose = TRUE,
               n.report = 10,
               n.burn = n.burn,
               n.thin = n.thin,
               n.chains = n.chains)

# Save the results to a hard drive ----------------------------------------
n.samples <- n.batch * batch.length
save(out, file = paste0(out.dir, 'ba-univ-spatial-noRE-', n.samples, '-samples-', 
                        Sys.Date(), '.rda'))
