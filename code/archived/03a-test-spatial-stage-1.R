# 2a-main-spatial-stage-1.R: script to fit Stage 1 of the multivariate
#                            SAE model with spatial random effects
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
load(paste0(data.dir, 'spOccupancy_data.rda'))
# Need to remove Slope from data frame since it has an NA value that will 
# cause the function to break. 
data.list.1$covs$Slope <- NULL

# Specify model inputs ----------------------------------------------------
# Priors ------------------------------
dist.mat <- dist(data.list.1$coords)
# The average distance between provinces in the study area.
high.dist <- 152116.2
# Setting to 1km to avoid overfitting. 
low.dist <- 1000

low.dist <- 100000
prior.list <- list(beta.normal = list(mean = 0, var = 2.72),
                   phi.unif = list(3 / high.dist, 3 / low.dist), 
                   sigma.sq.ig = list(2, 1))

# Starting values
inits.list <- list(beta = 0, sigma.sq = 2,
                   phi = 3 / mean(c(high.dist, low.dist)))
# Tuning
tuning.list <- list(phi = 0.75)

curr.sp <- 1
data.list.one.sp <- data.list.1
data.list.one.sp$y <- data.list.one.sp$y[curr.sp, ]
data.list.one.sp$weights <- rep(1, length(data.list.one.sp$y))

# MCMC criteria
n.batch <- 4000
batch.length <- 25
n.burn <- 40000
n.thin <- 20
n.chains <- 1

n.batch <- 1000
batch.length <- 25
n.burn <- 10000
n.thin <- 5
n.chains <- 1

n.batch <- 500
batch.length <- 25
n.burn <- 5000
n.thin <- 2
n.chains <- 1

# Fit the model -----------------------------------------------------------
out <- svcPGBinom(formula = ~ scale(Elevation) + I(scale(Elevation)^2) +
                              scale(forest_cov) + scale(range_cov) + scale(bareland_cov) +  
                              scale(ppt) + I(scale(ppt)^2) + (1 | Province_num),
              data = data.list.one.sp,
              n.batch = n.batch,
              batch.length = batch.length,
              svc.cols = 1,
              inits = inits.list,
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
save(out, file = paste0(out.dir, 'stage-1-spatial-', n.samples, '-samples-', n.factors,
                        '-factors-', Sys.Date(), '.rda'))

# Save a small subset -----------------------------------------------------
beta.samples <- out$beta.samples
theta.samples <- out$theta.samples
lambda.samples <- out$lambda.samples
beta.comm.samples <- out$beta.comm.samples
tau.sq.beta.samples <- out$tau.sq.beta.samples
psi.means <- apply(out$psi.samples, c(2, 3), mean)
psi.sds <- apply(out$psi.samples, c(2, 3), sd)
w.means <- apply(out$w.samples, c(2, 3), mean)
w.sds <- apply(out$w.samples, c(2, 3), sd)
save(beta.samples, theta.samples, lambda.samples, beta.comm.samples,
     tau.sq.beta.samples, psi.means, w.means, w.sds, psi.sds,
     file = paste0(out.dir, 'small-stage-1-spatial-', n.samples, '-samples-',
                   n.factors, '-factors-', Sys.Date(), '.rda'))
