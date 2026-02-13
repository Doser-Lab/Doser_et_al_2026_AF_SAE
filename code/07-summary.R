# 6-summary.R: script to summarize analysis results and generate all figures
#              shown in the manuscript.
# Author: Jeffrey W. Doser
rm(list = ls())
library(spOccupancy)
library(spAbundance)
library(tidyverse)
library(pals)
library(sf)
library(stars)
library(patchwork)
# For the simpleCap function
library(clinUtils)
# For scale_color_colorblind
library(ggthemes)
library(ggspatial)

# Load in data and prediction data ----------------------------------------
load('data/spOccupancy_data.rda')
load('data/spAbundance_data.rda')
load('data/prediction_data.rda')
my.crs <- 32642
coords.sf <- st_as_sf(as.data.frame(data.list.1$coords), 
                      coords = c('X', 'Y'), 
                      crs = my.crs)
# Afghanistan province data
af.prov <- st_read(dsn = 'data/afghanistan_geospatial/', layer = 'gadm41_AFG_1')
af.prov <- af.prov %>%
  st_transform(crs = my.crs)
# Filter for only the relevant provinces
keep.prov <- c('Nuristan', 'Kapisa', 'Laghman', 'Kunar', 'Nangarhar', 
               'Paktya', 'Khost', 'Paktika')
af.prov.model <- af.prov %>%
  filter(NAME_1 %in% keep.prov)

# Join the province with the district level data
af.district <- st_read(dsn = 'data/afghanistan_geospatial/', layer = 'gadm41_AFG_2')
af.district <- af.district %>%
  st_transform(crs = my.crs)
af.district <- af.district %>%
  filter(NAME_1 %in% keep.prov)

af.country <- st_union(af.prov.model)
af.country.full <- st_union(af.prov)

# Get species names
sp.names <- dimnames(data.list.1$y)[[1]]

# Get average distance between centroids of provinces. This is the value that 
# is used in the determination of the upper bound on the prior distribution for phi.
# prov.dists <- af.prov.model %>%
#   st_centroid() %>%
#   st_coordinates() %>%
#   dist()
# mean(prov.dists)

# Calculate basic summary statistics --------------------------------------
true.ba <- ifelse(data.list.2$y == 0, 0, exp(data.list.2$y))
apply(true.ba, 1, mean)
sort(sp.names)
raw.tree.dat <- read.csv("data/individual_tree_data.csv")
# Filter raw tree data to only use the data from the plots included in the model.
raw.tree.dat <- raw.tree.dat %>%
  filter(Plot %in% unique(data.list.2$covs$Plot))

apply(true.ba, 1, mean)
table(raw.tree.dat$Overstory)


# EDA histogram of BA values ----------------------------------------------
sp_names_plot <- str_replace_all(sp.names, "_", " ")
n <- ncol(true.ba)

ba_df <- data.frame(ba = c(true.ba), 
                    species = rep(sp_names_plot, times = n))
ggplot(data = ba_df, aes(x = ba)) +
  geom_histogram(bins = 15, col = "black", fill = "gray", linewidth = 0.25) +
  labs(x = "Basal area (square meters per hectare)", y = "Frequency") +
  facet_wrap(vars(species), scales = "free") + 
  theme_bw(base_size = 14) +
  theme(text = element_text(family="LM Roman 10"))
ggsave(file = 'figures/Figure_S2.png', height = 6, width = 8, units = 'in')

# Build Figure 1 ----------------------------------------------------------

# Plot locations
location.plot <- ggplot(coords.sf) +
  geom_sf(data = af.district, fill = 'cornsilk', color = 'grey', lwd = 0.4) +
  geom_sf(data = af.prov.model, fill = NA, color = 'black', lwd = 0.4, alpha = 0) +
  geom_sf(size = 1, col = 'black') +
  theme_bw(base_size = 18) +
  labs(x = 'Longitude', y = 'Latitude', title = '(a)') +
  theme(legend.position = 'inside',
        legend.position.inside = c(0.83, 0.25),
        legend.background = element_rect(fill = NA),
        text = element_text(family = 'LM Roman 10')) + 
  annotation_scale(style = 'ticks', text_family = 'LM Roman 10')


inset.plot <- ggplot() + 
  geom_sf(data = af.country.full, fill = 'white') + 
  geom_sf(data = st_union(af.district), fill = 'cornsilk') +
  theme_bw() + 
  theme(legend.position = 'inside',
        legend.position.inside = c(0.83, 0.25),
        legend.background = element_rect(fill = NA),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        text = element_text(family = 'LM Roman 10'))

# Simple landcover map ----------------
# Need to load in the prediction data here. 
landuse.dat <- read_stars('data/land-use-data/data/LANDCOVER_2018.tif')
landuse.efc <- st_crop(landuse.dat, af.country)

#   prop.forest <- function(a) {
#     mean(as.numeric(a$Class) %in% 8:10)
#   }

prop.forest <- function(a) {
  mean(a == 4, na.rm = TRUE)
}

# NOTE: this takes a few minutes to run.  
# prop.forest.dist <- rep(NA, nrow(af.district))
# for (i in 1:nrow(af.district)) {
#   print(paste0("Currently on district ", i, " out of ", nrow(af.district)))
#   prop.forest.dist[i] <- st_extract(landuse.dat, af.district[i, ], FUN = prop.forest)
# }
# prop.forest.dist <- unlist(prop.forest.dist)
# Save this as output just so you don't have to run it each time
# save(prop.forest.dist, file = "data/prop_forest_district.rda")
load("data/prop_forest_district.rda")
af.district$prop.forest.dist <- prop.forest.dist

vals <- st_extract(landuse.dat, coords.sf)
# Confirm that you're only using plots classified as forest cover in the 
# analysis.
table(vals$LANDCOVER_2018.tif)

load('data/prediction_data_500m.rda')
keep.indx <- which(pred.covs.df$Province %in% keep.prov)
plot.vals <- st_extract(landuse.dat, coords.pred.sf[keep.indx, ])
plot.df <- data.frame(numeric.val = plot.vals$LANDCOVER_2018.tif, 
                      x = coords.pred.mat[keep.indx, 1], 
                      y = coords.pred.mat[keep.indx, 2])
plot.df <- plot.df %>%
  filter(!is.na(numeric.val)) %>%
  mutate(land_cover_class = case_when(numeric.val == 1 ~ 'Snow', 
                                      numeric.val == 2 ~ 'Built-up area', 
                                      numeric.val == 4 ~ 'Forest', 
                                      numeric.val == 5 ~ 'Irrigated agriculture', 
                                      numeric.val == 10 ~ 'Bare land', 
                                      numeric.val == 11 ~ 'Rangeland', 
                                      numeric.val == 13 ~ 'Streams', 
                                      TRUE ~ 'Other'))

# TODO: need to make better colors for this. 
my.cols <- c('black', '#D55E00', '#009E73', '#E69F00', '#F0E442', 
             '#CC79A7', '#56B4E9', '#0072B2')
plot.df <- st_as_stars(plot.df, dims = c('x', 'y'))
landcover.map <- ggplot() +
  geom_stars(data = plot.df, aes(fill = land_cover_class)) +
  geom_sf(data = af.country, col = 'black', alpha = 0) +
  scale_fill_manual(values = my.cols, na.value = NA, na.translate = FALSE) + 
  theme_bw(base_size = 18) + 
  labs(x = 'Longitude', y = 'Latitude', title = '(b)', 
       fill = 'Type') + 
  theme(legend.position.inside = c(0.75, 0.18), 
        legend.position = 'inside',
        legend.background = element_rect(fill = NA), 
        text = element_text(family = 'LM Roman 10'),
        legend.title = element_text(size = 12),
        plot.title = element_text(size = 18),
        legend.text = element_text(size = 12)) +
  annotation_scale(style = 'ticks', text_family = 'LM Roman 10')

fig.1 <- location.plot + inset_element(inset.plot, 0.54, 0, 0.95, .45, align_to = 'full') + 
         landcover.map

ggsave(fig.1, file = 'figures/Figure_1.png', device = 'png', units = 'in', 
       height = 8, width = 12)

# EDA plot of basal area distributions by species -------------------------
# TODO: left off here, just make some EDA plots. 
# plot.df <- data.frame(ba = 

# Plotting the SAEs -------------------------------------------------------
# Determine number of prediction cells in each SAE
pred.files <- list.files("data/pred_pieces")
res.files <- sapply(pred.files, function(a) substr(a, nchar(a) - 7, nchar(a)))
pred.files <- pred.files[which(res.files == '100m.rda')]
# This is the order of the districts used in the SAE generation
districts <- sort(unique(pred.covs.df$District))
# Number of forest cells used for prediction for each small area
n.pred.district <- rep(0, length(districts))
for (i in 1:length(districts)) {
  load(paste0("data/pred_pieces/", pred.files[i]))
  if (!is.null(pred.covs.df)) {
    n.pred.district[i] <-  nrow(pred.covs.df)
  }
}
# The number of prediction cells within each SAE relates to the stability of the 
# estimate, and also the relevance of the district-wide SAE. Since models were only 
# fit with forested locations, we will only report SAE estimates for counties with minimal
# forest cover. 

load("results/top_model_SAE_100m.rda")
N <- length(sp.names)
ba.district.meds <- apply(basal.area.mean.samples, c(2, 3), median)
ba.district.low <- apply(basal.area.mean.samples, c(2, 3), quantile, 0.025, na.rm = TRUE)
ba.district.high <- apply(basal.area.mean.samples, c(2, 3), quantile, 0.975, na.rm = TRUE)
ba.district.sd <- apply(basal.area.mean.samples, c(2, 3), sd, na.rm = TRUE)
n.districts <- length(districts)
sae.df <- data.frame(med = c(ba.district.meds), 
                     low = c(ba.district.low),
                     high = c(ba.district.high), 
                     sd = c(ba.district.sd),
                     species = rep(sp.names, times = n.districts), 
                     district = rep(districts, each = N))
sae.df$ci.width <- sae.df$high - sae.df$low
for (i in 1:N) {
  plot.df <- sae.df %>% 
    filter(species == sp.names[i]) %>% 
    mutate(n.cells = n.pred.district)
  plot.sf <- inner_join(af.district, plot.df, by = join_by(NAME_2 == district))
  # TODO: don't map estimates for districts with only a small number of forested locations.
  #       Could change the proportion you use if you want. 
  plot.sf <- plot.sf %>%
    filter(prop.forest.dist >= .03)
  mean.plot <- ggplot() +
    geom_sf(data = af.district, fill = 'white') + 
    geom_sf(data = plot.sf, aes(fill = med)) +
    scale_fill_gradientn(colors = ocean.tempo(100),  na.value = NA) +
    labs(x = 'Longitude', y = 'Latitude', fill = expression(paste('BA(', m^2, '/ha)')),
         title = paste0('(a) ', simpleCap(str_replace_all(sp.names[i], '_', ' ')), 
                        ' median')) +
    theme_bw(base_size = 10) +
    theme(text = element_text(family="LM Roman 10"),
          axis.title.x=element_blank(), axis.title.y=element_blank(),
          axis.text.x = element_text(angle = 45, hjust = 1),
          # axis.text.y = element_text(size = 18),
          legend.key.size = unit(0.5, 'cm'),
          legend.position = 'inside',
          legend.position.inside=c(0.82, 0.28),
          legend.background = element_rect(fill = NA))
  ci.plot <- ggplot() +
    geom_sf(data = af.district, fill = 'white') + 
    geom_sf(data = plot.sf, aes(fill = ci.width)) +
    scale_fill_gradientn(colors = ocean.tempo(100),  na.value = NA) +
    labs(x = 'Longitude', y = 'Latitude', fill = expression(paste('BA(', m^2, '/ha)')),
         title = paste0('(b) ', simpleCap(str_replace_all(sp.names[i], '_', ' ')), 
                        ' 95% CI width')) +
    theme_bw(base_size = 10) +
    theme(text = element_text(family="LM Roman 10"),
          axis.title.x=element_blank(), axis.title.y=element_blank(),
          axis.text.x = element_text(angle = 45, hjust = 1),
          # axis.text.y = element_text(size = 18),
          legend.key.size = unit(0.5, 'cm'),
          legend.position = 'inside',
          legend.position.inside=c(0.82, 0.28),
          legend.background = element_rect(fill = NA))
  full.plot <- mean.plot + ci.plot
  ggsave(plot = full.plot, file = paste('figures/species_maps/', sp.names[i],
	      '-sae-map.png', sep = ''), height = 6, width = 8, units = 'in'  )
}
mean.plots <- list()
for (i in 1:N) {
  plot.df <- sae.df %>% 
    filter(species == sp.names[i]) %>% 
    mutate(n.cells = n.pred.district)
  plot.sf <- inner_join(af.district, plot.df, by = join_by(NAME_2 == district))
  # TODO: don't map estimates for districts with only a small number of forested locations.
  #       Could change the proportion you use if you want. 
  plot.sf <- plot.sf %>%
    filter(prop.forest.dist >= .03)
  mean.plots[[i]] <- ggplot() +
    geom_sf(data = af.district, fill = 'white') + 
    geom_sf(data = plot.sf, aes(fill = med)) +
    scale_fill_gradientn(colors = ocean.tempo(100),  na.value = NA) +
    labs(x = 'Longitude', y = 'Latitude', fill = expression(paste('BA(', m^2, '/ha)')),
         title = paste0(simpleCap(str_replace_all(sp.names[i], '_', ' ')))) +
    theme_bw(base_size = 10) +
    theme(text = element_text(family="LM Roman 10"),
          axis.title.x=element_blank(), axis.title.y=element_blank(),
          axis.text.x = element_text(angle = 45, hjust = 1),
          # axis.text.y = element_text(size = 18),
          legend.key.size = unit(0.5, 'cm'),
          legend.position = 'inside',
          legend.position.inside=c(0.82, 0.28),
          legend.background = element_rect(fill = NA))
}
# NOTE: order of the plots below is hardcoded based on the order of species. 
order(sp.names)
full.plot <- (mean.plots[[2]] | mean.plots[[1]] | mean.plots[[3]] | mean.plots[[5]]) / 
             (mean.plots[[4]] | mean.plots[[6]] | mean.plots[[7]] | mean.plots[[8]])
ggsave(plot = full.plot, file = 'figures/Figure_4.png', 
       height = 7, width = 10, units = 'in'  )


# Look at the sum of species-specific estimates, just as a comparison. 
plot.df <- data.frame(med = apply(ba.district.meds, 2, sum), 
                     district = districts)
plot.sf <- inner_join(af.district, plot.df, by = join_by(NAME_2 == district))
plot.sf <- plot.sf %>%
  filter(prop.forest.dist > .03)
ggplot() +
  geom_sf(data = plot.sf, aes(fill = med)) +
  scale_fill_gradientn(colors = ocean.tempo(100),  na.value = NA) +
  labs(x = 'Longitude', y = 'Latitude', fill = expression(paste('BA(', m^2, '/ha)'))) +
  theme_bw(base_size = 10) +
  theme(text = element_text(family="LM Roman 10"),
        axis.title.x=element_blank(), axis.title.y=element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1),
        # axis.text.y = element_text(size = 18),
        legend.key.size = unit(0.5, 'cm'),
        legend.position = 'inside',
        legend.position.inside=c(0.82, 0.28),
        legend.background = element_rect(fill = NA))

# Species distribution maps -----------------------------------------------
# Load prediction results
# TODO: change the resoultion as needed. Note that this is just for making maps, 
#       the SAE prediction is done separately at a higher resolution.
curr.res <- 1000
load("results/top_model_prediction_1000m.rda")
load("data/map_prediction_data_1000m.rda")
dimnames(psi.quants)[[2]] <- sp.names
dimnames(ba.quants)[[2]] <- sp.names

dist.plots <- list()
dist.ci.plots <- list()
ba.plots <- list()
for (i in 1:N) {
  print(paste0("Currently on species ", i, " out of ", N))
  curr.sp <- sp.names[i]
  curr.df <- data.frame(psi.med = psi.quants[1, i,], 
                        psi.ci.width = psi.quants[3, i, ] - psi.quants[2, i, ], 
                        ba.med = ba.quants[1, i, ], 
                        ba.ci.width = ba.quants[3, i, ] - ba.quants[2, i, ],
                        x = coords.pred.mat[, 1], 
                        y = coords.pred.mat[, 2], 
                        district = pred.covs.df$District)
  curr.df <- curr.df %>%
    filter(district %in% af.district$NAME_2)

  plot.df <- st_as_stars(curr.df, dims = c('x', 'y'))
  dist.plots[[i]] <- ggplot() +
    geom_stars(data = plot.df, aes(fill = psi.med), interpolate = TRUE) +
    # scale_fill_viridis_c(na.value = 'grey') +
    scale_fill_gradient(low = '#F7FBFF', high = '#08306B', na.value = NA, 
                        limits = c(0, 1)) + 
    # scale_fill_gradientn(colors = ocean.tempo(100),  na.value = NA) +
    geom_sf(data = af.country, col = 'black', alpha = 0) +
    labs(x = 'Longitude', y = 'Latitude', fill = 'Occurrence\nprobability',
         title = paste0('(A) ', simpleCap(str_replace_all(sp.names[i], '_', ' ')), ' median')) +
    theme_bw(base_size = 10) +
    theme(text = element_text(family="LM Roman 10"),
          axis.title.x=element_blank(), axis.title.y=element_blank(),
          axis.text.x = element_text(angle = 45, hjust = 1),
          # axis.text.y = element_text(size = 18),
          legend.key.size = unit(0.5, 'cm'),
          legend.position = 'inside',
          legend.title = element_text(size = 8), 
          legend.text = element_text(size = 8),
          legend.position.inside=c(0.82, 0.28),
          legend.background = element_rect(fill = NA))
  dist.ci.plots[[i]] <- ggplot() +
    geom_stars(data = plot.df, aes(fill = psi.ci.width), interpolate = TRUE) +
    # scale_fill_viridis_c(na.value = 'grey') +
    scale_fill_gradient(low = '#F7FBFF', high = '#08306B', na.value = NA, 
                        limits = c(0, 1)) + 
    # scale_fill_gradientn(colors = ocean.tempo(100),  na.value = NA) +
    geom_sf(data = af.country, col = 'black', alpha = 0) +
    labs(x = 'Longitude', y = 'Latitude', fill = 'Occurrence\nprobability',
         title = paste0('(B) ', simpleCap(str_replace_all(sp.names[i], '_', ' ')), ' 95% CI Width')) +
    theme_bw(base_size = 10) +
    theme(text = element_text(family="LM Roman 10"),
          axis.title.x=element_blank(), axis.title.y=element_blank(),
          axis.text.x = element_text(angle = 45, hjust = 1),
          # axis.text.y = element_text(size = 18),
          legend.key.size = unit(0.5, 'cm'),
          legend.position = 'inside',
          legend.title = element_text(size = 8), 
          legend.text = element_text(size = 8),
          legend.position.inside=c(0.82, 0.28),
          legend.background = element_rect(fill = NA))
  full.plot <- dist.plots[[i]] + dist.ci.plots[[i]]
  # Save distribution map
  ggsave(plot = full.plot, file = paste('figures/species_maps/', sp.names[i],
	      '-distribution-map.png', sep = ''), height = 6, width = 9, units = 'in'  )
  # Map of basal area  
  ba.plots[[i]] <- ggplot() +
    geom_stars(data = plot.df, aes(fill = ba.med), interpolate = TRUE) +
    # scale_fill_viridis_c(na.value = NA) +
    scale_fill_gradientn(colors = ocean.tempo(100),  na.value = NA) +
    # scale_fill_stepsn(colors = ocean.tempo(100), na.value = NA) + 
    geom_sf(data = af.country, col = 'black', alpha = 0) +
    labs(x = 'Longitude', y = 'Latitude', fill = expression(paste('BA (', m^2, '/ha)')),
         title = paste0('(A) ', simpleCap(str_replace_all(sp.names[i], '_', ' ')), ' median')) +
    theme_bw(base_size = 10) +
    theme(text = element_text(family="LM Roman 10"),
          axis.title.x=element_blank(), axis.title.y=element_blank(),
          axis.text.x = element_text(angle = 45, hjust = 1),
          # axis.text.y = element_text(size = 18),
          legend.key.size = unit(0.5, 'cm'),
          legend.position = 'inside',
          legend.position.inside=c(0.82, 0.28),
          legend.background = element_rect(fill = NA))
  ci.plots <- ggplot() +
    geom_stars(data = plot.df, aes(fill = ba.ci.width), interpolate = TRUE) +
    # scale_fill_viridis_c(na.value = NA) +
    scale_fill_gradientn(colors = ocean.tempo(100),  na.value = NA) +
    # scale_fill_stepsn(colors = ocean.tempo(100),  na.value = NA) +
    geom_sf(data = af.country, col = 'black', alpha = 0) +
    labs(x = 'Longitude', y = 'Latitude', fill = expression(paste('BA (', m^2, '/ha)')),
         title = paste0('(B) ', simpleCap(str_replace_all(sp.names[i], '_', ' ')), ' 95% CI Width')) +
    theme_bw(base_size = 10) +
    theme(text = element_text(family="LM Roman 10"),
          axis.title.x=element_blank(), axis.title.y=element_blank(),
          axis.text.x = element_text(angle = 45, hjust = 1),
          # axis.text.y = element_text(size = 18),
          legend.key.size = unit(0.5, 'cm'),
          legend.position = 'inside',
          legend.position.inside=c(0.82, 0.28),
          legend.background = element_rect(fill = NA))
  full.plot <- ba.plots[[i]] + ci.plots 
  print(summary(curr.df$ba.ci.width))
  # Save BA maps 
  ggsave(plot = full.plot, file = paste('figures/species_maps/', sp.names[i],
	      '-ba-map.png', sep = ''), height = 6, width = 9, units = 'in'  )
}

for (i in 1:N) {
  print(paste0("Currently on species ", i, " out of ", N))
  curr.sp <- sp.names[i]
  curr.df <- data.frame(psi.med = psi.quants[1, i,],
                        psi.ci.width = psi.quants[3, i, ] - psi.quants[2, i, ],
                        ba.med = ba.quants[1, i, ],
                        ba.ci.width = ba.quants[3, i, ] - ba.quants[2, i, ],
                        x = coords.pred.mat[, 1],
                        y = coords.pred.mat[, 2],
                        district = pred.covs.df$District)
  curr.df <- curr.df %>%
    filter(district %in% af.district$NAME_2)
  plot.df <- st_as_stars(curr.df, dims = c('x', 'y'))
  dist.plots[[i]] <- ggplot() +
    geom_stars(data = plot.df, aes(fill = psi.med), interpolate = TRUE) +
    # scale_fill_viridis_c(na.value = 'grey') +
    scale_fill_gradient(low = '#F7FBFF', high = '#08306B', na.value = NA,
                        limits = c(0, 1)) +
    # scale_fill_gradientn(colors = ocean.tempo(100),  na.value = NA) +
    geom_sf(data = af.country, col = 'black', alpha = 0) +
    labs(x = 'Longitude', y = 'Latitude', fill = 'Occurrence\nprobability',
         title = simpleCap(str_replace_all(sp.names[i], '_', ' '))) +
    theme_bw(base_size = 10) +
    theme(text = element_text(family="LM Roman 10"),
          axis.title.x=element_blank(), axis.title.y=element_blank(),
          axis.text.x = element_text(angle = 45, hjust = 1),
          # axis.text.y = element_text(size = 18),
          legend.key.size = unit(0.5, 'cm'),
          legend.position = 'inside',
          legend.title = element_text(size = 8),
          legend.text = element_text(size = 8),
          legend.position.inside=c(0.82, 0.28),
          legend.background = element_rect(fill = NA))
}
full.psi.plot <- (dist.plots[[2]] | dist.plots[[1]] | dist.plots[[3]] | dist.plots[[5]]) / 
                 (dist.plots[[4]] | dist.plots[[6]] | dist.plots[[7]] | dist.plots[[8]])
ggsave(plot = full.psi.plot, file = 'figures/Figure_3.png', 
       height = 7, width = 10, units = 'in')


# Model predictive performance --------------------------------------------
# Model order: Nonspatial only, nonspatial noRE, nonspatial, 
#              spatial only, spatial noRE, full spatial
load('results/ho-random-auc-rmspe.rda')
rownames(auc.ests) <- sp.names
rownames(rmspe.ests) <- sp.names
apply(auc.ests, 2, mean)
apply(rmspe.ests, 2, mean)


# Plots of covariate effects ----------------------------------------------
# Stage 1 -----------------------------
load("results/stage-1-spatial-2e+05-samples-4-factors-2026-01-09.rda")
out.stage.1 <- out

beta.quants <- apply(out.stage.1$beta.samples, c(2), quantile, c(0.025, 0.5, 0.975))
beta.prob.pos <- apply(out.stage.1$beta.samples, c(2), function(a) mean(a > 0))
# Manually remove the intercept. NOTE: hardcoded
beta.quants <- beta.quants[, -(1:N)]
beta.prob.pos <- beta.prob.pos[-(1:N)]
n.param <- length(beta.prob.pos) / N
# NOTE: hardcoded

beta.df <- data.frame(med = c(beta.quants[2, ]),
                      low = c(beta.quants[1, ]),
                      high = c(beta.quants[3, ]),
                      prob.pos = beta.prob.pos,
                      param = factor(rep(c('Elevation (linear)', 'Elevation (quadratic)',
                                           'Precipitation (linear)', 'Precipitation (quadratic)'),
                                    each = N)),
                      sp = rep(str_replace_all(sp.names, '_', ' '), times = n.param))

# Generate Figure S21
beta.df %>%
  ggplot(aes(x = med, y = param, fill = prob.pos)) +
  facet_wrap(vars(sp), ncol = 3) +
  geom_vline(xintercept = 0, lty = 2) +
  geom_segment(aes(x = low, y = param, xend = high, yend = param),
                   lineend = 'butt', linewidth = 1, col = 'gray') +
  geom_point(size = 3, pch = 21) +
  scale_fill_gradient2(midpoint = 0.5, high = '#2166AC', mid = 'white', low = '#B2182B',
                       na.value = NA, limits = c(0, 1)) +
  theme_bw(base_size = 15) +
  labs(x = 'Median effect (logit scale)',
       y = 'Covariate', fill = 'P(effect > 0)') +
  theme(text = element_text(family="LM Roman 10"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.y = element_text(size = 12))
ggsave(file = 'figures/Figure_S21.png', units = 'in', device = 'png',
       height = 8, width = 10)




# Stage 2 -----------------------------
load("results/stage-2-spatial-2e+05-samples-4-factors-2026-01-09.rda")
out.stage.2 <- out

beta.quants <- apply(out.stage.2$beta.samples, c(2), quantile, c(0.025, 0.5, 0.975))
beta.prob.pos <- apply(out.stage.2$beta.samples, c(2), function(a) mean(a > 0))
# Manually remove the intercept. NOTE: hardcoded
beta.quants <- beta.quants[, -(1:N)]
beta.prob.pos <- beta.prob.pos[-(1:N)]
n.param <- length(beta.prob.pos) / N
# NOTE: hardcoded

beta.df <- data.frame(med = c(beta.quants[2, ]),
                      low = c(beta.quants[1, ]),
                      high = c(beta.quants[3, ]),
                      prob.pos = beta.prob.pos,
                      param = factor(rep(c('Elevation (linear)', 'Elevation (quadratic)',
                                           'Precipitation (linear)', 'Precipitation (quadratic)'),
                                    each = N)),
                      sp = rep(str_replace_all(sp.names, '_', ' '), times = n.param))

# Generate Figure S22
beta.df %>%
  ggplot(aes(x = med, y = param, fill = prob.pos)) +
  facet_wrap(vars(sp), ncol = 3) +
  geom_vline(xintercept = 0, lty = 2) +
  geom_segment(aes(x = low, y = param, xend = high, yend = param),
                   lineend = 'butt', linewidth = 1, col = 'gray') +
  geom_point(size = 3, pch = 21) +
  scale_fill_gradient2(midpoint = 0.5, high = '#2166AC', mid = 'white', low = '#B2182B',
                       na.value = NA, limits = c(0, 1)) +
  theme_bw(base_size = 15) +
  labs(x = 'Median effect (log scale)',
       y = 'Covariate', fill = 'P(effect > 0)') +
  theme(text = element_text(family="LM Roman 10"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.y = element_text(size = 12))
ggsave(file = 'figures/Figure_S22.png', units = 'in', device = 'png',
       height = 8, width = 10)


# Model assessment --------------------------------------------------------
# TODO: change the model you're using here as needed
load("results/stage-2-spatial-1e+05-samples-4-factors-2026-01-08.rda")
y.rep.quants <- apply(out$y.rep.samples, c(2, 3), quantile, c(0.025, 0.5, 0.975))
N <- length(sp.names)

par(mfrow = c(1, 2))
for (i in 1:N) {
  keep.indx <- which(data.list.2$y[i, ] != 0)
  max.val <- max(c(data.list.2$y[i, keep.indx],
                   y.rep.quants[2, i, keep.indx]))
  min.val <- min(c(data.list.2$y[i, keep.indx],
                   y.rep.quants[2, i, keep.indx]))
  plot(data.list.2$y[i, keep.indx], y.rep.quants[2, i, keep.indx], pch = 19,
       xlab = 'True', ylab = 'Fitted', main = sp.names[i],
       ylim = c(min.val, max.val), xlim = c(min.val, max.val))
  abline(0, 1)
  hist(data.list.2$y[i, keep.indx], main = sp.names[i])
  hist(y.rep.quants[2, i, keep.indx], add = TRUE, col = 'red')
  print(cor(data.list.2$y[i, keep.indx], y.rep.quants[2, i, keep.indx]))
  Sys.sleep(1)
}

# A bit of funkiness here, but things generally seem alright. Correlation
# coefficients from above suggest general good pattern, but that the model
# tends to underpredict at high values and overpredict at low values.
resids.2 <- data.list.2$y - y.rep.quants[2, , ]
par(mfrow = c(1, 3))
for (i in 1:N) {
  keep.indx <- which(data.list.2$y[i, ] != 0)
  plot(y.rep.quants[2, i, keep.indx], resids.2[i, keep.indx], pch = 19,
       xlab = 'Fitted', ylab = 'Residuals', main = sp.names[i])
  abline(h = 0, lty = 2, col = 'grey')
  hist(resids.2[i, keep.indx], main = sp.names[i])
  qqnorm(resids.2[i, keep.indx], main = sp.names[i])
  qqline(resids.2[i, keep.indx], main = sp.names[i])
  Sys.sleep(2)
}
par(mfrow = c(1, 1))

