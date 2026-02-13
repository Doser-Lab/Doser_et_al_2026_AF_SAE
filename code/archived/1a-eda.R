# 00-eda.R: exploratory data analysis script.
rm(list = ls())
library(plyr)
library(tidyverse)

dat <- read.csv('data/individual_tree_data.csv')

# Number of locations
n_distinct(dat$Plot)

dat %>%
  filter(Plot == 1)

# OVERALL GOAL: characterize forest structure and composition. Thus, models 
#               will certainly need to be at the individual species-level.

# Data description --------------------------------------------------------
# Nasir said that the trees with DBH > 4cm were measured. 
# Each row corresponds to an overstory tree. 
# Plot: plot ID, unique for each individual plot.
# Province: 8 distinct provinces that are unique spatial locations in the 
#           Eastern Forest Complex. These are the polygons on the map. 
# District: 15 districts, seem to be smaller than provinces. These are the colors
#           on the map. 
# Village: smaller than districts, 45 of them. 
# Lat, Long: spatial coordinates. There's clear clustering here. 
# Aspect
# Slope
# Elevation
# Understory
# Logged_trees
# Regeneration
# Overstory
# DBH
# Height
# BA

# Number of trees per plot
dat %>% 
  group_by(Plot) %>%
  summarize(n_plots = n()) %>%
  pull(n_plots) %>%
  summary()
# Number of regen trees per plot
dat %>% 
  group_by(Plot) %>%
  summarize(regen = sum(Regeneration, na.rm = TRUE)) %>%
  pull(regen) %>%
  summary()


dat %>% 
  filter(District == 'Manogai') %>%
  ggplot() + 
  geom_histogram(aes(x = DBH))

dat <- dat %>%
  mutate(DBH_round = round_any(DBH, accuracy = 0.5, f = round))

dat %>%
  group_by(District) %>%
  summarize(med.dbh = median(DBH_round, na.rm = TRUE))
