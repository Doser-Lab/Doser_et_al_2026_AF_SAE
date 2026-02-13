# 3d-main-nonspatial-noRE-stage-2.R: script to fit Stage 2 of the multivariate
#                                    SAE model without spatial random effects
#                                    or unstructured province level effects.
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
load(paste0(data.dir, 'spAbundance_data.rda'))

# Specify model inputs ----------------------------------------------------
# Priors ------------------------------
prior.list <- list(beta.comm.normal = list(mean = 0, var = 10),
                   tau.sq.beta.ig = list(a = 0.1, b = 0.1), 
                   tau.sq.ig = list(a = 0.001, b = 0.001))

# Starting values
inits.list <- list(beta.comm = 0, beta = 0, tau.sq.beta = 1, fix = TRUE)

# MCMC criteria
n.batch <- 8000
batch.length <- 25
n.burn <- 50000
n.thin <- 50
n.chains <- 1

# Number of latent factors
n.factors <- 4

# Fit the model -----------------------------------------------------------
out <- lfMsAbund(formula = ~ scale(Elevation) + I(scale(Elevation)^2) +
                             scale(ppt) + I(scale(ppt)^2),
               data = data.list.2,
               n.batch = n.batch,
               batch.length = batch.length,
               inits = inits.list,
               family = 'zi-Gaussian',
               priors = prior.list,
               n.factors = n.factors,
               accept.rate = 0.43,
               n.omp.threads = 1,
               verbose = TRUE,
               n.report = 20,
               n.burn = n.burn,
               n.thin = n.thin,
               n.chains = n.chains)

# Save the results to a hard drive ----------------------------------------
n.samples <- n.batch * batch.length
save(out, file = paste0(out.dir, 'stage-2-nonspatial-noRE-', n.samples, '-samples-', n.factors,
                        '-factors-', Sys.Date(), '.rda'))

# Save a small subset -----------------------------------------------------
beta.samples <- out$beta.samples
lambda.samples <- out$lambda.samples
beta.comm.samples <- out$beta.comm.samples
tau.sq.beta.samples <- out$tau.sq.beta.samples
mu.means <- apply(out$mu.samples, c(2, 3), mean)
mu.sds <- apply(out$mu.samples, c(2, 3), sd)
w.means <- apply(out$w.samples, c(2, 3), mean)
w.sds <- apply(out$w.samples, c(2, 3), sd)
y.rep.quants <- apply(out$y.rep.samples, c(2, 3), quantile, c(0.025, 0.5, 0.975))
save(beta.samples, lambda.samples, beta.comm.samples,
     tau.sq.beta.samples, mu.means, w.means, w.sds, mu.sds, y.rep.quants,
     file = paste0(out.dir, 'small-stage-2-nonspatial-noRE-', n.samples, '-samples-',
                   n.factors, '-factors-', Sys.Date(), '.rda'))
