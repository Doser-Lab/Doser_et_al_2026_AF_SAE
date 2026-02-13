# 2a-main-sfMsAbund.R: script to fit the multivariate spatial JSDM for 
#                      estimating diameter distributions for the 10 species
#                      in the data set 
rm(list = ls())
library(spAbundance)

# Determine current species -----------------------------------------------
# This code is to extract the current species id from the command line
# to easily run the script for different species
args <- commandArgs(trailingOnly = TRUE)
# Current species
sp.indx <- as.numeric(args[1])
# Alternatively, if not running the script from the command line, you can
# specify the individual species manually below:
# TODO:
# sp.indx <- 1
if(length(args) == 0) base::stop('Need to tell spAbundance the current species')
print(sp.indx)

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
sp.names <- rownames(data.list$y)
curr.sp <- sp.names[sp.indx]
# Subset to just grab the current species
data.list$y <- data.list$y[sp.indx, , ]


# Specify model inputs ----------------------------------------------------
# Priors ------------------------------
dist.mat <- dist(data.list$coords)
high.dist <- quantile(dist.mat, 0.7)
low.dist <- quantile(dist.mat, 0.2)
prior.list <- list(beta.comm.normal = list(mean = 0, var = 10),
                   tau.sq.beta.ig = list(a = 0.1, b = 0.1), 
                   phi.unif = c(3 / high.dist, 3 / low.dist), 
                   kappa.unif = c(0, 10),
                   sigma.sq.ig = c(2, 1),
                   sigma.sq.mu.ig = list(a = 0.1, b = 0.1))

# Starting values
inits.list <- list(beta.comm = 0, beta = 0, tau.sq.beta = 1,
                   phi = 3 / median(dist.mat))
# Tuning
tuning.list <- list(beta = 0.1, beta.star = 0.1, lambda = 1, phi = 0.75, w = 0.5, 
                    kappa = 0.5)

# MCMC criteria
n.batch <- 4000
batch.length <- 25
n.burn <- 40000
n.thin <- 20
n.chains <- 1

# n.batch <- 2000
# batch.length <- 25
# n.burn <- 25000
# n.thin <- 5
# n.chains <- 1

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

# Fit the model -----------------------------------------------------------
out <- spAbund(formula = ~ scale(Elevation) + I(scale(Elevation)^2) +
                           scale(forest_cov) + I(scale(forest_cov)^2) + 
                           scale(ppt) + I(scale(ppt)^2) + 
                           (1 | d_class),
               data = data.list,
               n.batch = n.batch,
               batch.length = batch.length,
               inits = inits.list,
               family = 'Poisson',
               n.neighbors = 10,
               tuning = tuning.list,
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
save(out, file = paste0(out.dir, 'spAbund-', curr.sp, '-', n.samples, '-samples-',
                        Sys.Date(), '.rda'))
