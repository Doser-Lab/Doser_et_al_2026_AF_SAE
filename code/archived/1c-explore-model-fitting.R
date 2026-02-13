# 02-explore-model-fitting.R: just a script to explore fitting some models 
#                             with the new spAbundance functionality
rm(list = ls())
# TODO: change accordingly. 
# library(devtools)
# load_all("~/Dropbox/DFKZ23/spAbundance")
library(spAbundance)
library(coda)

# Read in the data set ----------------------------------------------------
load('data/spAbundance_data.rda')

# Subset to single species for now
sp.names <- dimnames(data.list$y)[[1]]
data.one.sp <- data.list
data.one.sp$y <- data.list$y[which(sp.names == 'Quercus_baloot'), , ]

# Subset to a single district for testing
# keep.indx <- which(data.one.sp$covs$District_num == 9)
# data.one.sp$y <- data.one.sp$y[keep.indx, ]
# data.one.sp$covs <- data.one.sp$covs[keep.indx, ]
# data.one.sp$coords <- data.one.sp$coords[keep.indx, ]
# 
# # Also remove points that are pretty far away
# dist.mat <- as.matrix(dist(data.one.sp$coords))
# keep.indx <- which(dist.mat[1, ] < 9000)
# data.one.sp$y <- data.one.sp$y[keep.indx, ]
# data.one.sp$covs <- data.one.sp$covs[keep.indx, ]
# data.one.sp$coords <- data.one.sp$coords[keep.indx, ]

# Specify model inputs ----------------------------------------------------
tvc.cols <- c('(Intercept)', 'scale(Elevation)')
p <- length(tvc.cols)

# Priors
dist.mat <- dist(data.one.sp$coords)
high.dist <- quantile(dist.mat, 0.9)
low.dist <- quantile(dist.mat, 0.1)
prior.list <- list(beta.0.normal = list(mean = 0, var = 10), 
                   gamma.normal = list(mean = 0, var = 10),
                   sigma.eta.iw = list(p, diag(p)),
                   kappa.unif = c(0, 100), 
                   phi.unif = list(3 / high.dist, 3 / low.dist), 
                   sigma.sq.ig = list(2, 1))

# Starting values
inits.list <- list(beta = 0, gamma = 0, beta.0 = 0, kappa = 1,
                   Sigma.eta = diag(p), 
                   w = matrix(0, nrow(data.one.sp$y), ncol(data.one.sp$y)), 
                   phi = 3 / median(dist.mat), sigma.sq = 1)
# Tuning
tuning.list <- list(beta = 0.1, gamma = 0.5, beta.0 = 0.5, 
                    beta.star = 0.1, kappa = 1, phi = 0.75, 
                    sigma.sq = 0.5, 
                    w = 0.5) 

# MCMC criteria
n.batch <- 3000
batch.length <- 25
n.burn <- 30000
n.thin <- 15
n.chains <- 1
# n.batch <- 1200
# batch.length <- 25
# n.burn <- 10000
# n.thin <- 10
# n.chains <- 1
# n.batch <- 400
# batch.length <- 25
# n.burn <- 5000
# n.thin <- 1
# n.chains <- 1
# Small
# n.batch <- 100
# batch.length <- 25
# n.burn <- 500
# n.thin <- 2
# n.chains <- 1

# Run the model -----------------------------------------------------------
# out <- dynAbund(formula = ~ scale(Elevation) + scale(forest_cov), 
#                 data = data.one.sp,
#                 n.batch = n.batch, 
#                 batch.length = batch.length, 
#                 tvc.cols = tvc.cols, 
#                 inits = inits.list, 
#                 family = 'Poisson', 
#                 tuning = tuning.list, 
#                 priors = prior.list, 
#                 accept.rate = 0.43, 
#                 n.omp.threads = 1, 
#                 verbose = TRUE, 
#                 n.report = 50, 
#                 n.burn = n.burn, 
#                 n.thin = n.thin, 
#                 n.chains = n.chains)
# # 
# # summary(out$beta.samples)
# # beta.means <- apply(out$beta.samples, 2, mean)
# # par(mfrow = c(1, 2))
# # plot(exp(beta.means[1:20]), pch = 19)
# # plot(apply(out$y, 2, mean), pch = 19)
# # par(mfrow = c(1, 1))
# # plot(beta.means[21:40], pch = 19)
# # plot(out$beta.samples, density = FALSE)
# # plot(out$gamma.samples, density = FALSE)
# # 
# # 
# # y.rep.means <- apply(out$y.rep.samples, c(2, 3), mean)
# # plot(y.rep.means, out$y, pch = 19, xlab = 'Fitted', ylab = 'True')
# # abline(0, 1)
# 
# # Spatial model -----------------------------------------------------------
# out.sp <- spDynAbund(formula = ~ scale(Elevation) + scale(forest_cov), 
#                      data = data.one.sp,
#                      n.batch = n.batch, 
#                      batch.length = batch.length, 
#                      tvc.cols = tvc.cols, 
#                      inits = inits.list, 
#                      family = 'Poisson', 
#                      tuning = tuning.list, 
#                      n.neighbors = 5, 
#                      cov.model = 'exponential',
#                      priors = prior.list, 
#                      accept.rate = 0.43, 
#                      n.omp.threads = 1, 
#                      verbose = TRUE, 
#                      n.report = 10, 
#                      n.burn = n.burn, 
#                      n.thin = n.thin, 
#                      n.chains = n.chains)
# 
# 
# summary(out.sp$beta.samples)
# beta.means <- apply(out.sp$beta.samples, 2, mean)
# par(mfrow = c(1, 2))
# plot(exp(beta.means[1:20]), pch = 19)
# plot(apply(out.sp$y, 2, mean), pch = 19)
# par(mfrow = c(1, 1))
# plot(beta.means[21:40], pch = 19)
# plot(out.sp$beta.samples, density = FALSE)
# plot(out.sp$gamma.samples, density = FALSE)
# plot(out.sp$theta.samples, density = FALSE)
# 
# plot(mcmc(out.sp$epsilon.samples[, , 20]), density = FALSE)
# 
# epsilon.means <- apply(out.sp$epsilon.samples, c(2, 3), mean)
# 
# 
# y.rep.means <- apply(out.sp$y.rep.samples, c(2, 3), mean)
# plot(y.rep.means, out.sp$y, pch = 19, xlab = 'Fitted', ylab = 'True')
# abline(0, 1)


# sfMsAbund() model -------------------------------------------------------
out <- abund(formula = ~ scale(Elevation) + I(scale(Elevation)^2) + 
                                 scale(forest_cov) + I(scale(forest_cov)^2) + 
                                 scale(ppt) + I(scale(ppt)^2) + (1 | d_class) + 
                                 (1 | Province_num), 
                       data = data.one.sp,
                       n.batch = n.batch, 
                       batch.length = batch.length, 
                       # inits = inits.list, 
                       family = 'Poisson', 
                       tuning = tuning.list, 
                       # priors = prior, 
                       accept.rate = 0.43, 
                       n.omp.threads = 1, 
                       verbose = TRUE, 
                       n.report = 10, 
                       n.burn = n.burn, 
                       n.thin = n.thin, 
                       n.chains = n.chains)

y.rep.means <- apply(out$y.rep.samples, c(2, 3), mean)
plot(out$y, y.rep.means, pch = 19, xlab = 'True', ylab = 'Fitted')
abline(0, 1)
