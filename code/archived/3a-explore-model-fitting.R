rm(list = ls())
library(coda)
library(tidyverse)
library(sf)

# Read in data
load('data/spAbundance_data.rda')
# Read in results
load('results/small-sfMsAbund-50000-samples-4-factors-2025-02-17.rda')

sp.names <- dimnames(data.list$y)[[1]]
N <- nrow(data.list$y)
for (i in 1:N) {
  plot(data.list$y[i, , ], y.rep.quants[2, i, , ], pch = 19,
       xlab = 'True', ylab = 'Fitted', main = sp.names[i])
  abline(0, 1, col = 'grey', lty = 2)
  Sys.sleep(1)
}

apply(data.list$y, 1, sum)

coords.sf <- st_as_sf(data.frame(data.list$coords),
                      coords = c('X', 'Y'),
                      crs = 32642)

coords.sf$factor.1 <- w.means[1, ]
coords.sf$factor.2 <- w.means[2, ]
coords.sf$factor.3 <- w.means[3, ]
coords.sf$factor.4 <- w.means[4, ]

# Make a long table for plotting in ggplot
plot.df.long <- coords.sf %>%
  pivot_longer(cols = factor.1:factor.4, names_to = 'parameter',
               values_to = 'estimate')
ggplot() +
  geom_sf(data = plot.df.long, aes(col = estimate)) +
  theme_bw() +
  scale_color_gradient2(midpoint = 0, low = '#B2182B', mid = 'white', high = '#2166AC',
                        na.value = NA) +
  facet_wrap(vars(parameter), nrow = 2)

