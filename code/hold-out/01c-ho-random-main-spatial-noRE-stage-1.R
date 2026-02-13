# 2c-main-spatial-noRE-stage-1.R: script to fit Stage 1 of the multivariate
#                                 SAE model with spatial random effects
#                                 and without unstructured province level effects.
rm(list = ls())
library(spOccupancy)

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
load(paste0(data.dir, 'spOccupancy_random_holdout_fit.rda'))
# Need to remove Slope from data frame since it has an NA value that will 
# cause the function to break. 
data.fit.1$covs$Slope <- NULL

# Specify model inputs ----------------------------------------------------
# Priors ------------------------------
dist.mat <- dist(data.fit.1$coords)
# The average distance between provinces in the study area.
high.dist <- 152116.2
# Setting to 1km to avoid overfitting. 
low.dist <- 1000
prior.list <- list(beta.comm.normal = list(mean = 0, var = 2.72),
                   tau.sq.beta.ig = list(a = 0.1, b = 0.1), 
                   phi.unif = list(3 / high.dist, 3 / low.dist))

# Starting values
inits.list <- list(beta.comm = 0, beta = 0, tau.sq.beta = 1,
                   phi = 3 / mean(c(high.dist, low.dist)), fix = TRUE)
# Tuning
tuning.list <- list(phi = 0.75)

# MCMC criteria
n.batch <- 8000
batch.length <- 25
n.burn <- 50000
n.thin <- 50
n.chains <- 1

# Number of latent factors
n.factors <- 4

# Fit the model -----------------------------------------------------------
out <- sfJSDM(formula = ~ scale(Elevation) + I(scale(Elevation)^2) +
                          scale(ppt) + I(scale(ppt)^2),
              data = data.fit.1,
              n.batch = n.batch,
              batch.length = batch.length,
              inits = inits.list,
              tuning = tuning.list,
              n.neighbors = 15,
              cov.model = 'exponential',
              priors = prior.list,
              n.factors = n.factors,
              accept.rate = 0.43,
              n.omp.threads = 1,
              verbose = TRUE,
              n.report = 10,
              n.burn = n.burn,
              n.thin = n.thin,
              n.chains = n.chains)

# Save the results to a hard drive ----------------------------------------
n.samples <- n.batch * batch.length
save(out, file = paste0(out.dir, 'ho-random-stage-1-spatial-noRE-', n.samples, '-samples-', n.factors,
                        '-factors-', Sys.Date(), '.rda'))
