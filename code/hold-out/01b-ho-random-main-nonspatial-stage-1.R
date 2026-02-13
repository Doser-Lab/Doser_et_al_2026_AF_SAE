# 2b-main-nonspatial-stage-1.R: script to fit Stage 1 of the multivariate
#                            SAE model without spatial random effects
#                            and with unstructured province level effects.
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
prior.list <- list(beta.comm.normal = list(mean = 0, var = 2.72),
                   tau.sq.beta.ig = list(a = 0.1, b = 0.1))

# Starting values
inits.list <- list(beta.comm = 0, beta = 0, tau.sq.beta = 1, fix = TRUE)

# MCMC criteria
n.samples <- 200000
n.burn <- 50000
n.thin <- 50
n.chains <- 1

# Number of latent factors
n.factors <- 4

# Fit the model -----------------------------------------------------------
out <- lfJSDM(formula = ~ scale(Elevation) + I(scale(Elevation)^2) +
                          scale(ppt) + I(scale(ppt)^2) + (1 | Province_num),
              data = data.fit.1,
              n.samples = n.samples,
              inits = inits.list,
              priors = prior.list,
              n.factors = n.factors,
              n.omp.threads = 1,
              verbose = TRUE,
              n.report = 1000,
              n.burn = n.burn,
              n.thin = n.thin,
              n.chains = n.chains)

# Save the results to a hard drive ----------------------------------------
save(out, file = paste0(out.dir, 'ho-random-stage-1-nonspatial-', n.samples, '-samples-', n.factors,
                        '-factors-', Sys.Date(), '.rda'))
