# 2a-main-lfMsAbund.R: script to fit the multivariate spatial JSDM for 
#                      estimating diameter distributions for the 10 species
#                      in the data set 
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
                   sigma.sq.mu.ig = list(a = 0.1, b = 0.1))

# Starting values
inits.list <- list(beta.comm = 0, beta = 0, tau.sq.beta = 1)
# Tuning
tuning.list <- list(beta = 0.1, beta.star = 0.1, lambda = 1, w = 0.5)

# MCMC criteria
# n.batch <- 4000
# batch.length <- 25
# n.burn <- 50000
# n.thin <- 10
# n.chains <- 1

n.batch <- 2000
batch.length <- 25
n.burn <- 25000
n.thin <- 5
n.chains <- 1

# n.batch <- 1200
# batch.length <- 25
# n.burn <- 10000
# n.thin <- 10
# n.chains <- 1

# n.batch <- 400
# batch.length <- 25
# n.burn <- 5000
# n.thin <- 5
# n.chains <- 1

# Small
# n.batch <- 100
# batch.length <- 25
# n.burn <- 500
# n.thin <- 2
# n.chains <- 1

# Testing
# n.batch <- 1
# batch.length <- 25
# n.burn <- 0
# n.thin <- 1
# n.chains <- 1

# Number of latent factors
n.factors <- 4

# Fit the model -----------------------------------------------------------
out <- lfMsAbund(formula = ~ scale(Elevation) + I(scale(Elevation)^2) +
                             scale(forest_cov) + I(scale(forest_cov)^2) + 
                             scale(ppt) + I(scale(ppt)^2) + 
                             (1 | d_class),
               data = data.list,
               n.batch = n.batch,
               batch.length = batch.length,
               inits = inits.list,
               family = 'Poisson',
               tuning = tuning.list,
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
save(out, file = paste0(out.dir, 'lfMsAbund-', n.samples, '-samples-', n.factors,
                        '-factors-', Sys.Date(), '.rda'))

# Save a small subset -----------------------------------------------------
beta.samples <- out$beta.samples
theta.samples <- out$theta.samples
lambda.samples <- out$lambda.samples
beta.comm.samples <- out$beta.comm.samples
sigma.sq.mu.samples <- out$sigma.sq.mu.samples
beta.star.samples <- out$beta.star.samples
tau.sq.beta.samples <- out$tau.sq.beta.samples
mu.means <- apply(out$mu.samples, c(2, 3, 4), mean)
mu.sds <- apply(out$mu.samples, c(2, 3, 4), sd)
w.means <- apply(out$w.samples, c(2, 3), mean)
w.sds <- apply(out$w.samples, c(2, 3), sd)
y.rep.quants <- apply(out$y.rep.samples, c(2, 3, 4), quantile, c(0.025, 0.5, 0.975))
save(beta.samples, theta.samples, lambda.samples, beta.comm.samples,
     tau.sq.beta.samples, mu.means, w.means, w.sds, mu.sds, y.rep.quants,
     sigma.sq.mu.samples, beta.star.samples, 
     file = paste0(out.dir, 'small-lfMsAbund-', n.samples, '-samples-',
                   n.factors, '-factors-', Sys.Date(), '.rda'))
