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
# Inventory data with the plot locations
load('data/spAbundance_data.rda')
my.crs <- 32642
coords.sf <- st_as_sf(as.data.frame(data.list.1$coords),
                      coords = c('X', 'Y'),
                      crs = my.crs)
# Afghanistan province data
af.prov <- st_read(dsn = 'data/afghanistan_geospatial/', layer = 'gadm41_AFG_1')
af.prov <- af.prov %>%
  st_transform(crs = my.crs)
# Filter for only the relevant provinces
keep.prov <- c('Nuristan', 'Kapisa', 'Kabul', 'Laghman', 'Kunar', 'Nangarhar',
               'Logar', 'Paktya', 'Khost', 'Paktika')
af.prov.model <- af.prov %>%
  filter(NAME_1 %in% keep.prov)

# Join the province with the district level data
af.district <- st_read(dsn = 'data/afghanistan_geospatial/', layer = 'gadm41_AFG_2')
af.district <- af.district %>%
  st_transform(crs = my.crs)
af.district <- af.district %>%
  filter(NAME_1 %in% keep.prov)
# Climate data
# Download WorldClim data for Afghanistan
afghan.prec <- worldclim_country(country = 'Afghanistan', var = 'prec',
                                 path = '~/Dropbox/data/worldclim', res = 0.5)

# Generate a grid for prediction one district at a time -------------------
# Read in land use data for determining sites that fall in forest. 
landuse.dat <- read_stars('data/land-use-data/data/LANDCOVER_2018.tif')
# NOTE: this approach is for generating the prediction data for the SAEs. For the 
#       prediction maps, there is a separate script. 
curr.res <- 100
n.districts <- nrow(af.district)
for (l in 1:n.districts) {
  print(paste0("Currently on district ", l, " out of ", n.districts))
  curr.district <- af.district[l, ] 
  grid.pred <- st_as_stars(st_bbox(curr.district), dx = curr.res, dy = curr.res)
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
  # Determine the prediction pixels that are classified as forest cover. 
  lc.vals <- st_extract(landuse.dat, at = coords.pred.sf)
  # Figure out pixels that fall within land classified  
  forest.indx <- which(lc.vals$LANDCOVER_2018.tif == 4)
  coords.pred.sf <- coords.pred.sf[forest.indx, ]
  coords.0 <- coords.0[forest.indx, ]
  print(nrow(coords.0))
  # Extract climate data from WorldClim -------------------------------------
  if (nrow(coords.0) > 0) {
    # Convert coordinates to worldclim crs
    coords.sf.wc <- coords.pred.sf %>%
      st_transform(crs = st_crs(afghan.prec))
    # Extract precipitation at each of the locations
    tmp <- terra::extract(afghan.prec, coords.sf.wc)
    ppt.cov <- apply(tmp[, -1], 1, sum)
    # Download elevation data -------------------------------------------------
    # Source: Amazon Web Services (AWS) Terrain Tiles (https://registry.opendata.aws/terrain-tiles/)
    # Citation: Terrain Tiles was accessed on 13 Jan 2026 from https://registry.opendata.aws/terrain-tiles.
    elev <- get_elev_point(coords.pred.sf, src = "aws")
    elev <- elev$elevation
    # Combine together and save to results ------------------------------------
    pred.covs.df <- data.frame(ppt = ppt.cov, 
                              elev = elev)
    coords.pred.mat <- st_coordinates(coords.pred.sf)
    # Determine province and district associated with the point. 
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
    bad.indx <- which(apply(pred.covs.df, 1, function(a) sum(is.na(a))) > 0)
    if (length(bad.indx) > 0) {
      pred.covs.df <- pred.covs.df[-bad.indx, ]
      coords.pred.mat <- coords.pred.mat[-bad.indx, ]
      coords.pred.sf <- coords.pred.sf[-bad.indx, ]
    }
  } else {
    pred.covs.df <- NULL
    coords.pred.mat <- st_coordinates(coords.pred.sf)
  }
  # Save to hard drive 
  save(pred.covs.df, coords.pred.mat, coords.pred.sf, 
       file = paste0('data/pred_pieces/prediction_data_', 
                     str_replace_all(curr.district$NAME_2, ' ', '_'), '_',  
                     curr.res, 'm.rda'))
  rm(grid.pred, coords.pred.sf, coords.pred, coords.lat.long)
  gc()
}
