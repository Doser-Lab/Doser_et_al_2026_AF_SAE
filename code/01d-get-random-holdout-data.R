# 1d-get-random-holdout-data.R: script to split up the data into a hold out data set 
#                               for model prediction. Here the plots that are held out
#                               are a random sample of 25% of the plot locations in the 
#                               data set. 
# Author: Jeffrey W. Doser
rm(list = ls())

# Set seed for reproducibility
set.seed(88372201)
# Load the full data sets -------------------------------------------------
load('data/spOccupancy_data.rda')
load('data/spAbundance_data.rda')

# Generate the subsets ----------------------------------------------------
n <- nrow(data.list.1$coords)
n.fit <- round(n * .75)
fit.indx <- sample(1:n, n.fit, replace = FALSE)
hold.indx <- (1:n)[-fit.indx]

# Generate the necessary data sets ----------------------------------------
# Fit data ----------------------------
data.fit.1 <- data.list.1
data.fit.1$y <- data.list.1$y[, fit.indx]
data.fit.1$coords <- data.list.1$coords[fit.indx, ]
data.fit.1$covs <- data.list.1$covs[fit.indx, ]
data.fit.2 <- data.list.2
data.fit.2$y <- data.list.2$y[, fit.indx]
data.fit.2$coords <- data.list.2$coords[fit.indx, ]
data.fit.2$covs <- data.list.2$covs[fit.indx, ]
data.fit.2$z <- data.list.2$z[, fit.indx]
save(data.fit.1, file = 'data/spOccupancy_random_holdout_fit.rda')
save(data.fit.2, file = 'data/spAbundance_random_holdout_fit.rda')
# Hold out data -----------------------
data.hold.1 <- data.list.1
data.hold.1$y <- data.list.1$y[, hold.indx]
data.hold.1$coords <- data.list.1$coords[hold.indx, ]
data.hold.1$covs <- data.list.1$covs[hold.indx, ]
data.hold.2 <- data.list.2
data.hold.2$y <- data.list.2$y[, hold.indx]
data.hold.2$coords <- data.list.2$coords[hold.indx, ]
data.hold.2$covs <- data.list.2$covs[hold.indx, ]
data.hold.2$z <- data.list.2$z[, hold.indx]
save(data.hold.1, file = 'data/spOccupancy_random_holdout_hold.rda')
save(data.hold.2, file = 'data/spAbundance_random_holdout_hold.rda')

