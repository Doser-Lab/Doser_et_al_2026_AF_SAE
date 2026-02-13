# 02b-main-nonspatial.R: script to fit the non-spatial model for estimating 
#                        basal area across eastern Afghanistan. 
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
prior.list <- list(beta.normal = list(mean = 0, var = 10))

# Starting values
inits.list <- list(beta = 0, tau.sq = 0.5)
# Tuning
tuning.list <- list(phi = 0.75)

# MCMC criteria
n.batch <- 4000
batch.length <- 25
n.burn <- 40000
n.thin <- 20
n.chains <- 1

# Fit the model -----------------------------------------------------------
out <- abund(formula = ~ scale(Elevation) + I(scale(Elevation)^2) + 
                         scale(ppt) + I(scale(ppt)^2) + 
                         (1 | Province_num), 
               data = data.list,
               n.batch = n.batch,
               batch.length = batch.length,
               inits = inits.list,
               family = 'Gaussian',
               tuning = tuning.list,
               priors = prior.list,
               accept.rate = 0.43,
               n.omp.threads = 1,
               verbose = TRUE,
               n.report = 100,
               n.burn = n.burn,
               n.thin = n.thin,
               n.chains = n.chains)

# Save the results to a hard drive ----------------------------------------
n.samples <- n.batch * batch.length
save(out, file = paste0(out.dir, 'ba-univ-nonspatial-', n.samples, '-samples-', 
                        Sys.Date(), '.rda'))
