rm(list = ls())
library(tidyverse)
library(sf)

# Load the data 
load('data/spAbundance_data.rda')

sp.names <- dimnames(data.list$y)[[1]]
data.one.sp <- data.list
data.one.sp$y <- data.list$y[which(sp.names == 'Picea_smithiana'), , ]

plot.df <- data.frame(counts = apply(data.one.sp$y, 1, sum), 
                      X = data.list$coords[, 1], 
                      Y = data.list$coords[, 2], 
                      ppt = data.one.sp$covs$ppt, 
                      tmin = data.one.sp$covs$tmin, 
                      forest = data.one.sp$covs$forest_cov, 
                      wind = data.one.sp$covs$wind,
                      tmax = data.one.sp$covs$tmax,
                      elev = data.one.sp$covs$Elevation)

plot.sf <- st_as_sf(plot.df, coords = c('X', 'Y'))

plot(plot.df$ppt, plot.df$counts, pch = 19)
plot(plot.df$tmin, plot.df$counts, pch = 19)
plot(plot.df$elev, plot.df$counts, pch = 19)
plot(plot.df$forest, plot.df$counts, pch = 19)

# Covariates to include: Elevation, precipitation, forest cover. 
# Include quadratic effects for all of them? Allowing them to peak. 
cor(plot.df)
