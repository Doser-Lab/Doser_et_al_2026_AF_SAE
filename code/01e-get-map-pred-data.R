# 1c-get-pred-data.R: extract data for a prediction grid across the 
#                     study region for use in prediction and map generation.
rm(list = ls())
library(tidyverse)
library(sf)
library(stars)
# For extracting WorldClim data
library(geodata)
library(AOI)
library(elevatr)


# Load data ---------------------------------------------------------------
# Afghanistan province data
af.prov <- st_read(dsn = 'data/afghanistan_geospatial/', layer = 'gadm41_AFG_1')
# Inventory data with the plot locations
load('data/spAbundance_data.rda')
my.crs <- 32642
coords.sf <- st_as_sf(data.frame(data.list.2$coords),
                      coords = c('X', 'Y'),
                      crs = my.crs)
af.prov <- af.prov %>%
  st_transform(crs = my.crs)
# Filter for only the relevant provinces
keep.prov <- c('Nuristan', 'Kapisa', 'Kabul', 'Laghman', 'Kunar', 'Nangarhar', 
               'Logar', 'Paktya', 'Khost', 'Paktika')
af.prov <- af.prov %>%
  filter(NAME_1 %in% keep.prov)


# Join the province with the district level data
af.district <- st_read(dsn = 'data/afghanistan_geospatial/', layer = 'gadm41_AFG_2')
af.district <- af.district %>%
  st_transform(crs = my.crs)


# Extract the districts for only the provinces of interest
af.district <- st_filter(af.district, af.prov)


# Generate a grid for prediction ------------------------------------------
# Note this is in units of m. 
curr.res <- 1000
grid.pred <- st_as_stars(st_bbox(af.prov), dx = curr.res, dy = curr.res)
# Convert to data frame
coords.pred <- as.data.frame(grid.pred, center = TRUE)
# Convert coordinates to an sf object
coords.pred.sf <- st_as_sf(coords.pred,
                           coords = c('x', 'y'),
                           crs = my.crs)
# Intersect with region of interest
coords.pred.sf <- st_intersection(coords.pred.sf, st_make_valid(af.prov))
coords.0 <- as.data.frame(st_coordinates(coords.pred.sf))
coords.lat.long <- coords.pred.sf %>%
  st_transform(crs = '+proj=longlat +datum=WGS84')

# Extract precipitation data ----------------------------------------------
# Extract climate data from WorldClim -------------------------------------
# Download WorldClim data for Afghanistan
afghan.prec <- worldclim_country(country = 'Afghanistan', var = 'prec',
                                 path = '~/Dropbox/data/worldclim', res = 0.5)
# Convert coordinates to worldclim crs
coords.sf.wc <- coords.pred.sf %>%
  st_transform(crs = st_crs(afghan.prec))
# Extract precipitation at each of the locations
tmp <- terra::extract(afghan.prec, coords.sf.wc)
ppt.cov <- apply(tmp[, -1], 1, sum)


# Extract elevation data --------------------------------------------------
# Download elevation data -------------------------------------------------
## Source: Amazon Web Services (AWS) Terrain Tiles (https://registry.opendata.aws/terrain-tiles/)
## Citation: Terrain Tiles was accessed on 23 June 2025 from https://registry.opendata.aws/terrain-tiles.
elev <- get_elev_point(coords.pred.sf, src = "aws")
elev <- elev$elevation

# Combine together and save to results ------------------------------------
pred.covs.df <- data.frame(ppt = ppt.cov, 
                           elev = elev)
coords.pred.mat <- st_coordinates(coords.pred.sf)


# Determine province and district associated with each point --------------
# Districts
indx.by.district <- st_contains(af.district, coords.pred.sf)
district <- vector(mode = 'character', length = nrow(pred.covs.df))
for (i in 1:n_distinct(af.district$NAME_2)) {
  district[indx.by.district[[i]]] <- af.district$NAME_2[i]
}
# Provinces
indx.by.province <- st_contains(af.prov, coords.pred.sf)
province <- vector(mode = 'character', length = nrow(pred.covs.df))
for (i in 1:n_distinct(af.prov$NAME_1)) {
  province[indx.by.province[[i]]] <- af.prov$NAME_1[i]
}
pred.covs.df$Province <- province
pred.covs.df$District <- district
# Manually create numeric version of Province to make sure it lines up 
# with what was used to fit the model. 
province_num <- rep(NA, length(province))
province_num <- case_when(province == 'Kapisa' ~ 1, 
                          province == 'Khost' ~ 2, 
                          province == 'Kunar' ~ 3, 
                          province == 'Laghman' ~ 4, 
                          province == 'Nangarhar' ~ 5, 
                          province == 'Nuristan' ~ 6, 
                          province == 'Paktya' ~ 7, 
                          province == 'Paktika' ~ 8, 
                          province == 'Kabul' ~ 9, 
                          province == 'Logar' ~ 10, 
                          TRUE ~ NA)
pred.covs.df$Province_num <- province_num


# Remove rows with any NA values ------------------------------------------
# bad.indx <- which(apply(pred.covs.df, 1, function(a) sum(is.na(a))) > 0)
# pred.covs.df <- pred.covs.df[-bad.indx, ]
# coords.pred.mat <- coords.pred.mat[-bad.indx, ]
# coords.pred.sf <- coords.pred.sf[-bad.indx, ]

# Save to hard drive ------------------------------------------------------
save(pred.covs.df, coords.pred.mat, coords.pred.sf, 
     file = "data/map_prediction_data_1000m.rda")

