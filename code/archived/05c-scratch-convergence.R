rm(list = ls())
library(spOccupancy)
library(spAbundance)


load("data/spOccupancy_data.rda")
load("data/spAbundance_data.rda")

load("results/stage-1-spatial-1e+05-samples-4-factors-2026-01-08.rda")

plot(out$lambda.samples, density = FALSE)

psi.fit.quants <- apply(out$psi.samples, c(2, 3), quantile, c(0.025, 0.975))
psi.ci.width <- psi.fit.quants[2, , ] - psi.fit.quants[1, , ]

summary(t(psi.ci.width))

load("results/stage-1-spatial-only-1e+05-samples-4-factors-2026-01-08.rda")


load("results/top_model_prediction_results_2000m.rda")
psi.0.ci.width <- psi.quants[5, , ] - psi.quants[2, , ]
summary(t(psi.0.ci.width))
# Seems like uncertainty is highest in the predictions for the species that 
# occure more often... That's an odd pattern. Basically just predicting
# massive uncertainty everywhere. 

load("results/test_top_model_prediction_results_2000m.rda")
psi.0.ci.width.test <- psi.quants[5, , ] - psi.quants[2, , ]
summary(t(psi.0.ci.width.test))

# The massive uncertainty is not tied to the covariates having 
# pretty extreme values.  
