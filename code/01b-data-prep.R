# 01-data-prep.R: script to format the data into the necessary formats for 
#                 fitting the SAE models with spOccupancy and spAbundance.
rm(list = ls())
library(tidyverse)
library(sf)
library(stars)
# For extracting WorldClim data
library(geodata)
library(elevatr)

# Read in the raw data ----------------------------------------------------
# Individual tree data. All trees collected on 12.6m radius plots (500m2).
tree.dat <- read.csv("data/individual_tree_data.csv")

# Only get trees with a measured DBH and that have DBH at least equal to 5, 
# round DBH to the nearest integer
tree.dat <- tree.dat %>%
  filter(!is.na(DBH), DBH >= 5) %>%
  mutate(DBH = round(DBH)) %>%
  select(-c(Understory, Logged_trees, Regeneration, Height))

# Get some site-level covariates ------------------------------------------
site.dat <- tree.dat %>%
  select(-c(Overstory, DBH, BA)) %>%
  unique() %>%
  arrange(Plot)

# Remove a few sites that have duplicate coordinates. It appears that these are
# distinct sites, but the latitude/longitude were recorded as exactly the same, 
# which leads to problems. Those, the second of the duplicate will be thrown out.
bad.indx <- which(duplicated(site.dat[, c('Lat', 'Long')]))

site.dat <- site.dat[-bad.indx, ]

coords.lat.long <- site.dat %>%
  select(Long, Lat)

coords.sf.lat.long <- st_as_sf(coords.lat.long, 
                               coords = c('Long', 'Lat'), 
                               crs = 4326)
coords.sf.utm <- coords.sf.lat.long %>%
  st_transform(crs = 32642)
coords <- coords.sf.utm %>%
  st_coordinates()

# coords.sf.lat.long$Province <- site.dat$Province
# plot(coords.sf.lat.long, pch = 19)

covs <- site.dat %>%
  mutate(Province_num = as.numeric(factor(Province)), 
         District_num = as.numeric(factor(District))) %>%
  select(-c(Lat, Long, Village))

# Format data for spAbundance ---------------------------------------------
# Number of plots
n <- n_distinct(site.dat$Plot)
# Unique plot IDs
unique.plots <- unique(site.dat$Plot)
# Number of species
J <- n_distinct(tree.dat$Overstory)
# Unique species
unique.species <- sort(unique(tree.dat$Overstory))
# Basal area data
y <- array(0, dim = c(J, n))
for (i in 1:n) {
  print(paste0("Currently on site ", i, " out of ", n))
  for (j in 1:J) {
    plot.dat <- tree.dat %>%
      filter(Plot == unique.plots[i], Overstory == unique.species[j]) %>%
      select(Overstory, BA) %>%
      # Multiply by BA by tree factor to get BA in m2 / ha
      mutate(BA = BA * 10000 / 500)
    if (nrow(plot.dat) > 0) {
      y[j, i] <- sum(plot.dat$BA)
    }
  }
}
# Remove species groups
bad.sp <- which(str_detect(unique.species, 'spp'))
y <- y[-bad.sp, ]
sp.names <- unique.species[-bad.sp]
sp.names <- str_replace_all(sp.names, ' ', '_')
dimnames(y)[[1]] <- sp.names

# Get landcover data ------------------------------------------------------
# 30 x 30 m resolution
landuse.dat <- read_stars('data/land-use-data/data/LANDCOVER_2018.tif')
# Only using the landcover data to predict determine the plots that are forested, 
# which is what determines whether a plot is included in the model or not. 

# Extract climate data from WorldClim ------------------------------------- 
# Download WorldClim data for Afghanistan
afghan.tmin <- worldclim_country(country = 'Afghanistan', var = 'tmin', 
                                 path = '~/Dropbox/data/worldclim', res = 0.5)
afghan.tmax <- worldclim_country(country = 'Afghanistan', var = 'tmax', 
                                 path = '~/Dropbox/data/worldclim', res = 0.5)
afghan.prec <- worldclim_country(country = 'Afghanistan', var = 'prec', 
                                 path = '~/Dropbox/data/worldclim', res = 0.5)
afghan.wind <- worldclim_country(country = 'Afghanistan', var = 'wind', 
                                 path = '~/Dropbox/data/worldclim', res = 0.5)
# afghan.vapr <- worldclim_country(country = 'Afghanistan', var = 'vapr', 
#                                  path = '~/Dropbox/data/worldclim', res = 0.5)
# Convert coordinates to worldclim crs
coords.sf.wc <- coords.sf.utm %>%
  st_transform(crs = st_crs(afghan.tmin))
# Extract climate variables at 
tmp <- terra::extract(afghan.tmax, coords.sf.wc)
tmax.cov <- apply(tmp[, -1], 1, max)
tmp <- terra::extract(afghan.tmin, coords.sf.wc)
tmin.cov <- apply(tmp[, -1], 1, min)
tmp <- terra::extract(afghan.prec, coords.sf.wc)
ppt.cov <- apply(tmp[, -1], 1, sum)
tmp <- terra::extract(afghan.wind, coords.sf.wc)
wind.cov <- apply(tmp[, -1], 1, mean)

# Extract elevation data --------------------------------------------------
# Source: Amazon Web Services (AWS) Terrain Tiles (https://registry.opendata.aws/terrain-tiles/)
# Citation: Terrain Tiles was accessed on Oct 10, 2025 from https://registry.opendata.aws/terrain-tiles.
elev <- get_elev_point(coords.sf.utm, src = "aws")

# Format data for spAbundance ---------------------------------------------
# Convert covs to a list and add in d_class
covs.list <- data.frame(Plot = covs$Plot, 
                        Province = covs$Province, 
                        District = covs$District, 
                        Aspect = covs$Aspect, 
                        Elevation = elev$elevation,
                        Province_num = covs$Province_num, 
                        District_num = covs$District_num, 
                        tmin = tmin.cov, 
                        ppt = ppt.cov, 
                        tmax = tmax.cov, 
                        wind = wind.cov)
data.list.2 <- list(y = y, 
                    covs = covs.list, 
                    coords = coords, 
                    z = ifelse(y > 0, 1, y))
# Reorder the species to help with factor model convergence
# Reformat species ordering to add in mixing and convergence --------------
# Using 4 factors
sp.names <- dimnames(data.list.2$y)[[1]]
start.sp <- c('Cedrus_deodara', 'Abies_spectabilis', 'Juniperus_semiglobosa', 
              'Pinus_gerardiana')
# Other species codes
indices <- rep(NA, length(start.sp))
for (i in 1:length(start.sp)) {
  indices[i] <- which(sp.names == start.sp[i])
}
indices.other <- 1:nrow(data.list.2$y)
indices.other <- indices.other[-indices]
# Create the ordered y data frame
data.list.2$y <- data.list.2$y[c(indices, indices.other), ]
data.list.2$z <- data.list.2$z[c(indices, indices.other), ]

# Format data for first stage in spOccupancy ------------------------------
data.list.1 <- data.list.2
data.list.1$y <- data.list.2$z
data.list.1$z <- NULL

# Explore transformations for y -------------------------------------------
# Here I'm looking at a couple of transformations to explore which ones
# will likely provide the best assumptions in model estimation.
sp.names <- dimnames(data.list.2$y)[[1]]
par(mfrow = c(1, 3))
for (i in 1:length(sp.names)) {
  keep.indx <- which(data.list.2$y[i, ] != 0)
  hist(log(data.list.2$y[i, keep.indx]), main = paste0(sp.names[i], ' log'))
  hist(data.list.2$y[i, keep.indx]^.25, main = paste0(sp.names[i], ' fourth root'))
  hist(data.list.2$y[i, keep.indx]^.5, main = paste0(sp.names[i], ' square root'))
  Sys.sleep(2) 
}

# While fourth-root seemingly provides a closer approx to a normal distribution, 
# the center of the distribution is fairly close to 0, and thus there will be a
# non-negligible chunk of the probability distribution that goes below 0. This 
# can be problematic, since the fourth-root is not a one-to-one function. Thus, 
# we will use the log-transformation, despite it resulting in a bit of an extreme
# negative skew. 

# Remove rare species -----------------------------------------------------
bad.sp <- which(apply(data.list.2$z, 1, sum) < 10)
data.list.1$y <- data.list.1$y[-bad.sp, ]
data.list.2$y <- data.list.2$y[-bad.sp, ]
data.list.2$z <- data.list.2$z[-bad.sp, ]

# Take the log to model with a multivariate log-normal hurdle model. 
data.list.2$y <- ifelse(data.list.2$y > 0, log(data.list.2$y), 0)

# Determine plots that fall in forest cover -------------------------------
lc.vals <- st_extract(landuse.dat, at = coords.sf.utm)
# Figure out plots that fall within land classified  
forest.indx <- which(lc.vals$LANDCOVER_2018.tif == 4)

# Filter out to only use forestland plots
data.list.1$y <- data.list.1$y[, forest.indx]
data.list.1$covs <- data.list.1$covs[forest.indx, ]
data.list.1$coords <- data.list.1$coords[forest.indx, ]
data.list.2$y <- data.list.2$y[, forest.indx]
data.list.2$covs <- data.list.2$covs[forest.indx, ]
data.list.2$coords <- data.list.2$coords[forest.indx, ]
data.list.2$z <- data.list.2$z[, forest.indx]

# Save to hard drive ------------------------------------------------------
save(data.list.1, file = 'data/spOccupancy_data.rda')
save(data.list.2, file = 'data/spAbundance_data.rda')
