# 01-data-prep.R: script to format the diameter distribution data into the 
#                 necessary format for fitting diameter distribution models
#                 with spAbundance.
rm(list = ls())
library(tidyverse)
library(sf)
library(stars)
# For extracting WorldClim data
library(geodata)

# Read in the raw data ----------------------------------------------------
# Individual tree data. All trees collected on 12.6m radius plots (500m2).
tree.dat <- read.csv("data/individual_tree_data.csv")

# Only get trees with a measured DBH and that have DBH at least equal to 5, 
# round DBH to the nearest integer
tree.dat <- tree.dat %>%
  filter(!is.na(DBH), DBH >= 5) %>%
  mutate(DBH = round(DBH)) %>%
  select(-c(Understory, Logged_trees, Regeneration, Height, BA)) %>%
  # Create DBH classes, where trees with DBH >= 100cm are put in the final class
  mutate(DBH = ifelse(DBH >= 100, 100, DBH), 
         DBH_5cm = cut_width(DBH, width = 5))

# Get some site-level covariates ------------------------------------------
site.dat <- tree.dat %>%
  select(-c(Overstory, DBH, DBH_5cm)) %>%
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
# Number of diameter classes
n.d.class <- n_distinct(tree.dat$DBH_5cm)
# Unique diameter classes
unique.d.class <- sort(unique(tree.dat$DBH_5cm)) 
# Diameter distribution data. Note can fill in with 0s since there's no missing data.
y <- array(0, dim = c(J, n, n.d.class))
for (i in 1:n) {
  print(paste0("Currently on site ", i, " out of ", n))
  for (j in 1:J) {
    plot.dat <- tree.dat %>%
      filter(Plot == unique.plots[i], Overstory == unique.species[j]) %>%
      select(Overstory, DBH_5cm) %>%
      group_by(Overstory, DBH_5cm) %>%
      summarize(n_vals = n(), .groups = 'drop')
    y[j, i, as.numeric(plot.dat$DBH_5cm)] <- plot.dat$n_vals
  }
}
# Remove species groups
bad.sp <- which(str_detect(unique.species, 'spp'))
y <- y[-bad.sp, , ]
sp.names <- unique.species[-bad.sp]
sp.names <- str_replace_all(sp.names, ' ', '_')
dimnames(y)[[1]] <- sp.names

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
# 1000 km radius. NOTE: need to think about this a bit more. 
coords.sf.buff <- coords.sf.utm %>%
  st_buffer(dist = 1000)
percent.for <- function(a) {
  mean(a == 4, na.rm = TRUE)
}
# Calculate percent forest cover as a first initial covariate.
forest.cov <- rep(NA, n)
vals <- split(1:n, ceiling(seq_along(1:n)/100))
for (i in 1:length(vals)) {
  print(paste0("Currently on piece ", i, " out of ", length(vals)))
  forest.cov[vals[[i]]] <- aggregate(landuse.dat, by = coords.sf.buff[vals[[i]], ], 
                                     FUN = percent.for)[[1]]
}
# Add to covariate data frame
covs$forest_cov <- forest.cov

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

# Format data for spAbundance ---------------------------------------------
# Convert covs to a list and add in d_class
covs.list <- list(Plot = covs$Plot, 
                  Province = covs$Province, 
                  District = covs$District, 
                  Aspect = covs$Aspect, 
                  Slope = covs$Slope, 
                  Elevation = covs$Elevation,
                  Province_num = covs$Province_num, 
                  District_num = covs$District_num, 
                  forest_cov = covs$forest_cov,
                  tmin = tmin.cov, 
                  ppt = ppt.cov, 
                  tmax = tmax.cov, 
                  wind = wind.cov,
                  d_class = matrix(1:n.d.class, nrow = n, ncol = n.d.class, byrow = TRUE))
data.list <- list(y = y, 
                  covs = covs.list, 
                  coords = coords)
# Reorder the species to help with factor model convergence
# Reformat species ordering to add in mixing and convergence --------------
# Using 4 factors
sp.names <- dimnames(data.list$y)[[1]]
start.sp <- c('Cedrus_deodara', 'Abies_spectabilis', 'Juniperus_semiglobosa', 
              'Pinus_gerardiana')
# Other species codes
indices <- rep(NA, length(start.sp))
for (i in 1:length(start.sp)) {
  indices[i] <- which(sp.names == start.sp[i])
}
indices.other <- 1:nrow(data.list$y)
indices.other <- indices.other[-indices]
# Create the ordered y data frame
data.list$y <- data.list$y[c(indices, indices.other), , ]

# Save to hard drive
save(data.list, file = 'data/spAbundance_data.rda')
