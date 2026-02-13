# 02e-main-spatial-only.R: script to fit the main spatial linear mixed model 
#                     for small area estimation of biomass across eastern 
#                     Afghanistan. 
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
load(paste0(data.dir, 'ba_random_holdout_fit.rda'))

# Specify model inputs ----------------------------------------------------
# Priors ------------------------------
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
out <- spAbund(formula = ~ 1, 
               data = data.fit,
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
save(out, file = paste0(out.dir, 'ho-ba-univ-spatial-only-', n.samples, '-samples-', 
                        Sys.Date(), '.rda'))
