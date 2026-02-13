# 02c-get-factors.R: script to visualize and summarize how many factors are 
#                    optimal for use in both Stage 1 and Stage 2 of the multivariate
#                    hurdle model. 
# Author: Jeffrey W. Doser
rm(list = ls())
library(dplyr)
library(ggplot2)

# Number of factors
# NOTE: hardcoded
n.factors <- 8
waic.df <- data.frame(stage = rep(c('Stage 1', 'Stage 2'), each = n.factors), 
                      waic = NA, 
                      n.factors = rep(1:n.factors, times = 2))
for (i in 1:n.factors) {
  load(paste0('results/prelim-stage-1-waic-', i, '-factors-2026-01-07.rda'))
  waic.df$waic[i] <- sum(waic.out$WAIC)
  load(paste0('results/prelim-stage-2-waic-', i, '-factors-2026-01-08.rda'))
  waic.df$waic[i + n.factors] <- sum(waic.out$WAIC)
}

# Generate figure for supplemental info
ggplot(data = waic.df, aes(x = n.factors, y = waic)) + 
  geom_point(size = 2) + 
  geom_line(linewidth = .8) + 
  facet_wrap(vars(stage), scales = 'free_y') + 
  theme_bw(base_size = 16) +
  theme(text = element_text(family="LM Roman 10")) + 
  labs(x = 'Number of spatial factors', y = 'WAIC')
ggsave(file = 'figures/Figure_S3.png', 
       device = 'png', height = 5, width = 11, units = 'in'  )

# Based on this plot and the number of species, decided to go with 4 spatial factors for 
# Stage 1 and 4 factors for Stage 2. 
