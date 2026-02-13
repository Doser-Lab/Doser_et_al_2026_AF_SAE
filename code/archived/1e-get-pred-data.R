# 1e-get-pred-data.R: extract data for a prediction grid across the 
#                     study region for use in prediction and map generation.
rm(list = ls())
library(tidyverse)
library(sf)
library(stars)
# For extracting WorldClim data
library(geodata)

# Load data ---------------------------------------------------------------
# Afghanistan province data
af.prov <- st_read(dsn = 'data/afghanistan_geospatial/', layer = 'gadm41_AFG_1')
# Inventory data with the plot locations
load('data/spAbundance_data.rda')
my.crs <- 32642
coords.sf <- st_as_sf(data.frame(data.list$coords),
                      coords = c('X', 'Y'),
                      crs = my.crs)
af.prov <- af.prov %>%
  st_transform(crs = my.crs)
# Filter for only the relevant provinces
keep.prov <- c('Nuristan', 'Kapisa', 'Kabul', 'Laghman', 'Kunar', 'Nangarhar', 
               'Logar', 'Paktya', 'Khost', 'Paktika')
af.prov <- af.prov %>%
  filter(NAME_1 %in% keep.prov)

# Generate a grid for prediction ------------------------------------------
# Note this is in units of m
grid.pred <- st_as_stars(st_bbox(af.prov), dx = 1000, dy = 1000)
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

# Extract forest cover data -----------------------------------------------
landuse.dat <- read_stars('data/land-use-data/data/LANDCOVER_2018.tif')
# Forest = 4
# 1000 m radius. NOTE: need to think about this a bit more.
coords.sf.buff <- coords.pred.sf %>%
  st_buffer(dist = 1000)
percent.for <- function(a) {
  mean(a == 4, na.rm = TRUE)
}
# Calculate percent forest cover as a first initial covariate.
n.0 <- nrow(coords.0)
forest.0.cov <- rep(NA, n.0)
vals <- split(1:n.0, ceiling(seq_along(1:n.0)/100))
# NOTE: this takes a few hours
for (i in 1:length(vals)) {
  print(paste0("Currently on piece ", i, " out of ", length(vals)))
  forest.0.cov[vals[[i]]] <- aggregate(landuse.dat, by = coords.sf.buff[vals[[i]], ],
                                       FUN = percent.for)[[1]]
}
save(forest.0.cov, file = 'data/forest_pred_data.rda')
