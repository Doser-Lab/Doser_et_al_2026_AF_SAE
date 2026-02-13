# 6-summary.R: script to summarize analysis results and generate all figures
#              shown in the manuscript.
# Author: Jeffrey W. Doser
rm(list = ls())
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

# Load in data and prediction data ----------------------------------------
load('data/spOccupancy_data.rda')
load('data/ba_spAbundance_data.rda')
my.crs <- 32642
coords.sf <- st_as_sf(as.data.frame(data.list$coords), 
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
# Load in the propportion each district is forested for use in generating SAE map. 
load("data/prop_forest_district.rda")
af.district$prop.forest.dist <- prop.forest.dist

af.country <- st_union(af.prov.model)
af.country.full <- st_union(af.prov)

# EDA histogram of basal area values --------------------------------------
ba <- data.list$y^4
ba_df <- data.frame(ba = ba)
ggplot(data = ba_df, aes(x = ba)) + 
  geom_histogram(bins = 15, col = "black", fill = "gray", linewidth = 0.25) + 
  labs(x = "Basal area (square meters per hectare)", y = "Frequency") + 
  theme_bw(base_size = 14) +
  theme(text = element_text(family="LM Roman 10"))
ggsave(file = 'figures/Figure_S1.png', height = 4, width = 6, units = 'in')

# Basal area map and small area estimates ---------------------------------
# Load prediction results
load("results/ba_univ_top_model_prediction_1000m.rda")
load("data/map_prediction_data_1000m.rda")

curr.df <- data.frame(ba.med = ba.quants[1, ], 
                      ba.ci.width = ba.quants[3, ] - ba.quants[2, ],
                      district = pred.covs.df$District, 
                      x = coords.pred.mat[, 1], 
                      y = coords.pred.mat[, 2])
curr.df <- curr.df %>%
  filter(district %in% af.district$NAME_2)
plot.df <- st_as_stars(curr.df, dims = c('x', 'y'))
# Map of basal area  
ba.plot <- ggplot() +
  geom_stars(data = plot.df, aes(fill = ba.med), interpolate = TRUE) +
  # scale_fill_viridis_c(na.value = NA) +
  scale_fill_gradientn(colors = ocean.tempo(100),  na.value = NA) +
  geom_sf(data = af.country, col = 'black', alpha = 0) +
  labs(x = 'Longitude', y = 'Latitude', fill = expression(paste('BA (', m^2, '/ha)')),
       title = '(a) Posterior median') +
  theme_bw(base_size = 10) +
  theme(text = element_text(family="LM Roman 10"),
        axis.title.x=element_blank(), axis.title.y=element_blank(),
        # axis.text.x = element_text(angle = 45, hjust = 1),
        # axis.text.y = element_text(size = 18),
        legend.key.size = unit(0.5, 'cm'),
        legend.position = 'inside',
        legend.position.inside=c(0.82, 0.28),
        legend.background = element_rect(fill = NA))
ci.plot <- ggplot() +
  geom_stars(data = plot.df, aes(fill = ba.ci.width), interpolate = TRUE) +
  # scale_fill_viridis_c(na.value = NA) +
  scale_fill_gradientn(colors = ocean.tempo(100),  na.value = NA) +
  # scale_fill_stepsn(colors = ocean.tempo(100),  na.value = NA) +
  geom_sf(data = af.country, col = 'black', alpha = 0) +
  labs(x = 'Longitude', y = 'Latitude', fill = expression(paste('BA (', m^2, '/ha)')),
       title = '(b) 95% CI Width') +
  theme_bw(base_size = 10) +
  theme(text = element_text(family="LM Roman 10"),
        axis.title.x=element_blank(), axis.title.y=element_blank(),
        # axis.text.x = element_text(angle = 45, hjust = 1),
        # axis.text.y = element_text(size = 18),
        legend.key.size = unit(0.5, 'cm'),
        legend.position = 'inside',
        legend.position.inside=c(0.82, 0.28),
        legend.background = element_rect(fill = NA))
full.maps <- ba.plot + ci.plot 
# SAE Maps ----------------------------------------------------------------
# Plot the SAEs
load("results/ba_univ_top_model_SAE_100m.rda")
ba.district.meds <- apply(basal.area.mean.samples, c(2), median)
ba.district.low <- apply(basal.area.mean.samples, c(2), quantile, 0.025, na.rm = TRUE)
ba.district.high <- apply(basal.area.mean.samples, c(2), quantile, 0.975, na.rm = TRUE)
ba.district.sd <- apply(basal.area.mean.samples, c(2), sd, na.rm = TRUE)
# This is the order of the districts used in the SAE generation (alphabetical)
districts <- sort(unique(pred.covs.df$District))
n.districts <- length(districts)
plot.df <- data.frame(med = ba.district.meds, 
                     low = ba.district.low,
                     high = ba.district.high, 
                     sd = ba.district.sd,
                     district = districts)
plot.sf <- inner_join(af.district, plot.df, by = join_by(NAME_2 == district))
# Only generate SAE map for plots with >3% forest cover
plot.sf <- plot.sf %>%
  filter(prop.forest.dist > 0.03)
sae.mean.plot <- ggplot() +
  geom_sf(data = af.district, fill = 'white') + 
  geom_sf(data = plot.sf, aes(fill = med)) +
  scale_fill_gradientn(colors = ocean.tempo(100),  na.value = NA) +
  labs(x = 'Longitude', y = 'Latitude', fill = expression(paste('BA(', m^2, '/ha)')), 
       title = '(c) District-level posterior median') +
  theme_bw(base_size = 10) +
  theme(text = element_text(family="LM Roman 10"),
        axis.title.x=element_blank(), axis.title.y=element_blank(),
        # axis.text.x = element_text(angle = 45, hjust = 1),
        # axis.text.y = element_text(size = 18),
        legend.key.size = unit(0.5, 'cm'),
        legend.position = 'inside',
        legend.position.inside=c(0.82, 0.28),
        legend.background = element_rect(fill = NA))
sae.sd.plot <- ggplot() +
  geom_sf(data = af.district, fill = 'white') + 
  geom_sf(data = plot.sf, aes(fill = sd)) +
  scale_fill_gradientn(colors = ocean.tempo(100),  na.value = NA) +
  labs(x = 'Longitude', y = 'Latitude', fill = expression(paste('BA(', m^2, '/ha)')), 
       title = '(d) District-level 95% CI Width') +
  theme_bw(base_size = 10) +
  theme(text = element_text(family="LM Roman 10"),
        axis.title.x=element_blank(), axis.title.y=element_blank(),
        # axis.text.x = element_text(angle = 45, hjust = 1),
        # axis.text.y = element_text(size = 18),
        legend.key.size = unit(0.5, 'cm'),
        legend.position = 'inside',
        legend.position.inside=c(0.82, 0.28),
        legend.background = element_rect(fill = NA))
sae.mean.plot + sae.sd.plot

full.plot <- ba.plot + ci.plot + sae.mean.plot + sae.sd.plot
ggsave(plot = full.plot, file = 'figures/Figure_2.png', 
       height = 8, width = 6, units = 'in')

# Get some insight on forests with the highest BA -------------------------
plot.sf %>% 
  arrange(desc(med)) %>%
  st_drop_geometry() %>%
  select(Province = NAME_1, District = NAME_2, med)

plot.sf %>% 
  arrange(desc(med)) %>%
  st_drop_geometry() %>%
  select(Province = NAME_1, District = NAME_2, med) %>%
  group_by(Province) %>%
  summarize(avg_val = mean(med))

# Plots showing the covariate effects -------------------------------------
load("results/ba-univ-spatial-noRE-1e+05-samples-2026-01-13.rda")

beta.quants <- apply(out$beta.samples, c(2), quantile, c(0.025, 0.5, 0.975))
beta.prob.pos <- apply(out$beta.samples, c(2), function(a) mean(a > 0))
# Manually remove the intercept. NOTE: hardcoded
beta.quants <- beta.quants[, -1]
beta.prob.pos <- beta.prob.pos[-1]
n.param <- length(beta.prob.pos)
# NOTE: hardcoded

beta.df <- data.frame(med = c(beta.quants[2, ]),
                      low = c(beta.quants[1, ]),
                      high = c(beta.quants[3, ]),
                      prob.pos = beta.prob.pos,
                      param = factor(c('Elevation (linear)', 'Elevation (quadratic)',
                                           'Precipitation (linear)', 'Precipitation (quadratic)')))

# Generate Figure S20
beta.df %>%
  ggplot(aes(x = med, y = param, fill = prob.pos)) +
  geom_vline(xintercept = 0, lty = 2) +
  geom_segment(aes(x = low, y = param, xend = high, yend = param),
                   lineend = 'butt', linewidth = 1, col = 'gray') +
  geom_point(size = 3, pch = 21) +
  scale_fill_gradient2(midpoint = 0.5, high = '#2166AC', mid = 'white', low = '#B2182B',
                       na.value = NA, limits = c(0, 1)) +
  theme_bw(base_size = 15) +
  labs(x = 'Median effect (fourth-root scale)',
       y = 'Covariate', fill = 'P(effect > 0)') +
  theme(text = element_text(family="LM Roman 10"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.y = element_text(size = 12))
ggsave(file = 'figures/Figure_S20.png', units = 'in', device = 'png',
       height = 4, width = 7)

