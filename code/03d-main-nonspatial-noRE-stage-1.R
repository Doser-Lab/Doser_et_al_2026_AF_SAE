# 2d-main-nonspatial-noRE-stage-1.R: script to fit Stage 1 of the multivariate
#                                    SAE model without spatial random effects
#                                    and without unstructured province level effects.
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
load(paste0(data.dir, 'spOccupancy_data.rda'))
# Need to remove Slope from data frame since it has an NA value that will 
# cause the function to break. 
data.list.1$covs$Slope <- NULL

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
                          scale(ppt) + I(scale(ppt)^2),
              data = data.list.1,
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
save(out, file = paste0(out.dir, 'stage-1-nonspatial-noRE-', n.samples, '-samples-', n.factors,
                        '-factors-', Sys.Date(), '.rda'))

# Save a small subset -----------------------------------------------------
beta.samples <- out$beta.samples
lambda.samples <- out$lambda.samples
beta.comm.samples <- out$beta.comm.samples
tau.sq.beta.samples <- out$tau.sq.beta.samples
psi.means <- apply(out$psi.samples, c(2, 3), mean)
psi.sds <- apply(out$psi.samples, c(2, 3), sd)
w.means <- apply(out$w.samples, c(2, 3), mean)
w.sds <- apply(out$w.samples, c(2, 3), sd)
save(beta.samples, lambda.samples, beta.comm.samples,
     tau.sq.beta.samples, psi.means, w.means, w.sds, psi.sds,
     file = paste0(out.dir, 'small-stage-1-nonspatial-noRE-', n.samples, '-samples-',
                   n.factors, '-factors-', Sys.Date(), '.rda'))
