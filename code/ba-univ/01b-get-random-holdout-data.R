# 1d-get-random-holdout-data.R: script to split up the data into a hold out data set 
#                               for model prediction. Here the plots that are held out
#                               are a random sample of 25% of the plot locations in the 
#                               data set. 
# Author: Jeffrey W. Doser
rm(list = ls())

# Set seed for reproducibility
set.seed(88372201)
# Load the full data sets -------------------------------------------------
load('data/ba_spAbundance_data.rda')

# Generate the subsets ----------------------------------------------------
n <- nrow(data.list$coords)
n.fit <- round(n * .75)
fit.indx <- sample(1:n, n.fit, replace = FALSE)
hold.indx <- (1:n)[-fit.indx]

# Generate the necessary data sets ----------------------------------------
# Fit data ----------------------------
data.fit <- data.list
data.fit$y <- data.list$y[fit.indx]
data.fit$coords <- data.list$coords[fit.indx, ]
data.fit$covs <- data.list$covs[fit.indx, ]
save(data.fit, file = 'data/ba_random_holdout_fit.rda')
# Hold out data -----------------------
data.hold <- data.list
data.hold$y <- data.list$y[hold.indx]
data.hold$coords <- data.list$coords[hold.indx, ]
data.hold$covs <- data.list$covs[hold.indx, ]
save(data.hold, file = 'data/ba_random_holdout_hold.rda')

