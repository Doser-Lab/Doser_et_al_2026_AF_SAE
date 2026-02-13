# 01a-data-prep.R: script to format the data into the formats necessary 
#                  for estimating basal area. 
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

covs <- site.dat %>%
  mutate(Province_num = as.numeric(factor(Province)), 
         District_num = as.numeric(factor(District))) %>%
  select(-c(Lat, Long, Village))

# Format data for spAbundance ---------------------------------------------
# Number of plots
n <- n_distinct(site.dat$Plot)
# Unique plot IDs
unique.plots <- unique(site.dat$Plot)
# Basal area data
y <- rep(0, n)
# Very inefficient, but using this approach to keep same order as 
# the multi-species model. 
for (i in 1:n) {
  plot.dat <- tree.dat %>%
    filter(Plot == unique.plots[i]) %>%
    select(Overstory, BA) %>%
    # Multiply by BA by tree factor to get BA in m2 / ha
    mutate(BA = BA * 10000 / 500)
  y[i] <- sum(plot.dat$BA)
}

# Get landcover data ------------------------------------------------------
landuse.dat <- read_stars('data/land-use-data/data/LANDCOVER_2018.tif')
# Snow = 1
# Build-up area = 2
# Water body = 3
# Forest = 4
# Irrigated agriculture = 5
# Rainfed agriculture = 6
# Fruit trees = 7
# Vineyards = 8
# Marshland = 9
# Bareland = 10
# Rangeland = 11
# Sand cover = 12
# Streams = 13
# 1000 m radius. NOTE: need to think about this a bit more. 
coords.sf.buff <- coords.sf.utm %>%
  st_buffer(dist = 1000)
percent.for <- function(a) {
  mean(a == 4, na.rm = TRUE)
}
percent.ag <- function(a) {
  mean(a %in% c(5, 6), na.rm = TRUE)
}
percent.range <- function(a) {
  mean(a == 11, na.rm = TRUE)
}
percent.bare <- function(a) {
  mean(a == 10, na.rm = TRUE)
}
# Calculate percent forest cover as a first initial covariate.
forest.cov <- rep(NA, n)
ag.cov <- rep(NA, n)
range.cov <- rep(NA, n)
bare.cov <- rep(NA, n)
vals <- split(1:n, ceiling(seq_along(1:n)/100))
for (i in 1:length(vals)) {
  print(paste0("Currently on piece ", i, " out of ", length(vals)))
  forest.cov[vals[[i]]] <- aggregate(landuse.dat, by = coords.sf.buff[vals[[i]], ], 
                                     FUN = percent.for)[[1]]
  ag.cov[vals[[i]]] <- aggregate(landuse.dat, by = coords.sf.buff[vals[[i]], ], 
                                 FUN = percent.ag)[[1]]
  range.cov[vals[[i]]] <- aggregate(landuse.dat, by = coords.sf.buff[vals[[i]], ], 
                                    FUN = percent.range)[[1]]
  bare.cov[vals[[i]]] <- aggregate(landuse.dat, by = coords.sf.buff[vals[[i]], ], 
                                    FUN = percent.bare)[[1]]
}
# Add to covariate data frame
covs$forest_cov <- forest.cov
covs$ag_cov <- ag.cov
covs$range_cov <- range.cov
covs$bare_cov <- bare.cov

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
# Citation: Terrain Tiles was accessed on Jan 2, 2026 from https://registry.opendata.aws/terrain-tiles.
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
                        forest_cov = covs$forest_cov,
                        ag_cov = covs$ag_cov, 
                        range_cov = covs$range_cov,
                        bareland_cov = covs$bare_cov,
                        tmin = tmin.cov, 
                        ppt = ppt.cov, 
                        tmax = tmax.cov, 
                        wind = wind.cov)
# Using a 1/4 root transformation as this leads to an adequately 
# normal distribution.                         
data.list <- list(y = y^.25, 
                  covs = covs.list, 
                  coords = coords)

# Determine plots that fall in forest cover -------------------------------
lc.vals <- st_extract(landuse.dat, at = coords.sf.utm)
# Figure out plots that fall within land classified as forest. 
forest.indx <- which(lc.vals$LANDCOVER_2018.tif == 4)

# Filter out to only use forestland plots
data.list$y <- data.list$y[forest.indx]
data.list$covs <- data.list$covs[forest.indx, ]
data.list$coords <- data.list$coords[forest.indx, ]

# Save to hard drive ------------------------------------------------------
save(data.list, file = 'data/ba_spAbundance_data.rda')
