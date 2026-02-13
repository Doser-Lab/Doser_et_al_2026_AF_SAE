# 1e-get-district-holdout-data.R: script to split up the data into a hold out data set 
#                                 for model prediction. Here the plots that are held out
#                                 are chosen based on the districts that they come from, 
#                                 such that 25% of the districts are held out from the 
#                                 model fit, and the remaining data are used to fit the 
#                                 model.
# Author: Jeffrey W. Doser
rm(list = ls())

# Set seed for reproducibility
set.seed(3382)
# Load the full data sets -------------------------------------------------
load('data/spOccupancy_data.rda')
load('data/spAbundance_data.rda')

# Generate the subsets ----------------------------------------------------
n <- nrow(data.list.1$coords)

districts <- sort(unique(data.list.1$covs$District))
n.districts <- length(unique((data.list.1$covs$District)))
# Split into 75% and 25%
n.fit.districts <- round(n.districts * .75)
n.hold.districts <- n.districts - n.fit.districts
# Determine the districts that you're going to keep
fit.districts.indx <- sample(1:n.districts, n.fit.districts, replace = FALSE) 
hold.districts.indx <- (1:n.districts)[-fit.districts.indx]
fit.indx <- which(data.list.1$covs$District %in% districts[fit.districts.indx])
hold.indx <- which(data.list.1$covs$District %in% districts[hold.districts.indx])

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
save(data.fit.1, file = 'data/spOccupancy_district_holdout_fit.rda')
save(data.fit.2, file = 'data/spAbundance_district_holdout_fit.rda')
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
save(data.hold.1, file = 'data/spOccupancy_district_holdout_hold.rda')
save(data.hold.2, file = 'data/spAbundance_district_holdout_hold.rda')
