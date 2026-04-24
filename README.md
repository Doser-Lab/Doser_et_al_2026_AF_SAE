# Small area estimation of basal area in a highly inaccessible forest region

### [Jeffrey W. Doser](https://doserlab.com/), Safiullah Khurram, Mujeeburahman Ariez, Mohammad Nasir Shalizi, John W. Groninger

### In review

### Code DOI: 

### Please contact the first author for questions: Jeffrey W. Doser (jwdoser@ncsu.edu)

---------------------------------

## Abstract

Estimates of forest structure at management-relevant spatial scales are essential for the successful implementation of sustainable forestry practices. In remote and/or dangerous terrain, it is difficult to collect forest inventory data following a strict probability sampling design, which inhibits the use of classical design-based estimators to obtain statistically defensible estimates with associated uncertainty. As a result, model-based approaches represent a viable solution when forest inventory data collection does not adhere to strict probability sampling rules. Here we evaluate the feasibility of a hierarchical Bayesian spatial modeling framework for providing the first statistically rigorous estimates of basal area across eastern Afghanistan. We focus on generating small area estimates of overall basal area and basal area by species within administrative districts that represent the scale at which many local communities make natural resource management decisions. We leverage an extensive forest inventory data set of 467 plots, in which the plot locations were determined based primarily with regards to safety and accessibility. Univariate spatial models were able to successfully provide estimates of overall basal area at the district level. Uncertainty of overall basal area was low across most of the study region even at fine-spatial scales. Multivariate spatial models provided reliable estimates of eight individual species distributions across the region, while estimates of species-specific basal area were reliable for five out of eight species. Model comparison revealed the importance of accounting for spatial autocorrelation in the model-based estimators to account for spatial patterns in species distributions and basal area across the region. However, spatial autocorrelation in basal area was often extremely fine scale, which limited the predictive capability of the models when generating estimates of basal area at non-sampled locations. Future efforts should seek to incorporate wall-to-wall remote sensing data to improve the ability to estimate fine-scale variation in basal area across eastern Afghanistan. The produced maps of overall and species-specific basal area by district are of direct relevance for local communities to help inform community-based forest management initiatives.  

## Repository Directory

The forest inventory data are not included on GitHub due to privacy concerns, and thus the code below cannot be successfully run. However, if you are interested in exploring the data used to fit the model, please contact the first author (Jeff Doser, jwdoser@ncsu.edu).  

### [code](./code/)

+ `01a-eda.R`: a simple exploratory script for exploring the original unformatted data set. This was not used for part of the analysis, but was provided for transparency. 
+ `01b-data-prep.R`: script to format the data into the necessary formats for fitting the SAE models with spOccupancy and spAbundance
+ `01c-get-pred-data.R`: script to extract data for a prediction grid across the study region for use in prediction.
+ `01d-get-random-holdout-data.R`: script to split up the data into a hold out data set for model prediction.  
+ `01e-get-map-pred-data.R`: script to extract data for a prediction grid across the study region for use in map generation.
+ `02a-determine-factors-stage-1.R`: script to do some preliminary runs of the Stage 1 model to determine how many spatial factors are needed to accurately represent the species distribution across the area. 
+ `02b-determine-factors-stage-2.R`: script to do some preliminary runs of the Stage 2 model to determine how many spatial factors are needed to accurately represent the species basal area across the area. 
+ `02c-get-factors.R`: script to visualize and summarize how many factors are optimal for use in both the Stage 1 and Stage 2 of the multivariate hurdel model. 
+ `03a-main-spatial-stage-1.R`: script to fit Stage 1 of the multivariate SAE model with spatial random effects and with unstructured province level effects. 
+ `03b-main-nonspatial-stage-1.R`: script to fit Stage 1 of the multivariate SAE model without spatial random effects and with unstructured province level effects. 
+ `03c-main-spatial-noRE-stage-1.R`: script to fit Stage 1 of the multivariate SAE model with spatial random effects and without unstructured province level effects. 
+ `03d-main-nonspatial-noRE-stage-1.R`: script to fit Stage 1 of the multivariate SAE model without spatial random effects and without unstructured province level effects. 
+ `03e-main-spatial-only-stage-1.R`: script to fit Stage 1 of the multivariate SAE model with spatial random effects, and with unstructured province level effects, without including any of the covariate data. 
+ `03f-main-nonspatial-only-stage-1.R`: script to fit Stage 1 of the multivariate SAE model without spatial random effects and with unstructured province level effects, and without any covariates. 
+ `04a-main-spatial-stage-2.R`: script to fit Stage 2 of the multivariate SAE model with spatial random effects and with unstructured province level effects. 
+ `04b-main-nonspatial-stage-2.R`: script to fit Stage 2 of the multivariate SAE model without spatial random effects and with unstructured province level effects. 
+ `04c-main-spatial-noRE-stage-2.R`: script to fit Stage 2 of the multivariate SAE model with spatial random effects and without unstructured province level effects. 
+ `04d-main-nonspatial-noRE-stage-2.R`: script to fit Stage 2 of the multivariate SAE model without spatial random effects and without unstructured province level effects. 
+ `04e-main-spatial-only-stage-2.R`: script to fit Stage 2 of the multivariate SAE model with spatial random effects, and with unstructured province level effects, without including any of the covariate data. 
+ `04f-main-nonspatial-only-stage-2.R`: script to fit Stage 2 of the multivariate SAE model without spatial random effects and with unstructured province level effects, and without any covariates. 
+ `05a-submit.sh`: script to submit all the model fitting files to the NCSU HPC. 
+ `05b-convergence-assessment.R`: script to do some basic exploration of model fit results, including convergence assessment and residual diagnostics. 
+ `06a-predict.R`: script to generate small area estimates across districts in Eastern Afghanistan. 
+ `06b-submit.sh`: bash script to submit the prediction R script to the NCSU HPC. 
+ `06c-predict.R`: script to generate maps of species distributions and basal area across eastern Afghanistan. 
+ `07-summary.R`: script to summarize analysis results and generate all figures shown in the manuscript. 
+ [ba-univ/](./code/ba-univ/): directory containing all files from the univariate portion of the analysis. 
     + `01a-data-prep.R`: script to format the data into the necessary format to fit the univariate models. 
     + `01b-get-random-holdout-data.R`: script to split up the data into a hold out data set for model prediction. 
     + `02a-main-spatial.R`: script to fit the main spatial linear mixed model for small area estimation of biomass across eastern Afghanistan. 
     + `02b-main-nonspatial.R`: script to fit the main nonspatial model for estimating basal area across eastern Afghanistan. 
     + `02c-main-spatial-noRE.R`: script to fit the spatial linear mixed model without the province-level random effects. 
     + `02d-main-nonspatial-noRE.R`: script to fit a basic linear model for small area estimation of basal area across eastern Afghanistan. 
     + `02e-main-spatial-only.R`: script to fit the main spatial linear mixed model for small area estimation of biomass across eastern Afghanistan. 
     + `02f-main-nonspatial-only.R`: script to fit the constant linear model for small area estimation of basal area across eastern Afghanistan. 
     + `03-convergence-model-comp.R`: script to assess model convergence and determine which model performs the best relative to all other models. 
     + `04a-sae-predict.R`: script to generate small area estimates across provinces in Eastern Afghanistan. 
     + `04b-map-predict.R`: script to generate predictions of basal area across eastern Afghanistan for use in generating maps. 
     + `05-summary.R`: script to summarize analysis results and generate all figures shown in the manuscript. 
     + [hold-out](./code/ba-univ/hold-out/): directory containing files for the out-of-sample validation for the univariate model. 
          + `01a-main-spatial.R`: script to fit the main spatial linear mixed model for small area estimation of biomass across eastern Afghanistan for cross-validation. 
          + `01b-main-nonspatial.R`: script to fit the non-spatial model for estimating basal area across eastern Afghanistan for cross-validation. 
          + `01c-main-spatial-noRE.R`: script to fit the spatial linear mixed model without the province level random effects with cross validation. 
          + `01d-main-nonspatial-noRE.R`: script to fit a basic linear model for small area estimation of basal area across eastern Afghanistan with cross-validation. 
          + `01e-main-spatial-only.R`: script to fit the main spatial linear mixed model for small area estimation of biomass across eastern Afghanistan for cross-validation. 
          + `01f-main-nonspatial-only.R`: script to fit the constant linear model for small area estimation of basla area across eastern Afghanistan for cross-validation. 
          + `02-ho-random-predict.R`: script to predict presence/absence and biomass at hold-out locations used in the analysis. 
+ [hold-out](./code/hold-out): directory containing files for the out-of-sample validation for the multivariate model. 
    + `01a-ho-random-spatial-stage-1.R`: script to fit Stage 1 of the multivariate SAE model with spatial random effects and with unstructured province level effects. 
    + `01b-ho-random-nonspatial-stage-1.R`: script to fit Stage 1 of the multivariate SAE model without spatial random effects and with unstructured province level effects. 
    + `01c-ho-random-spatial-noRE-stage-1.R`: script to fit Stage 1 of the multivariate SAE model with spatial random effects and without unstructured province level effects. 
    + `01d-ho-random-nonspatial-noRE-stage-1.R`: script to fit Stage 1 of the multivariate SAE model without spatial random effects and without unstructured province level effects. 
    + `01e-ho-random-spatial-only-stage-1.R`: script to fit Stage 1 of the multivariate SAE model with spatial random effects, and with unstructured province level effects, without including any of the covariate data. 
    + `01f-ho-random-nonspatial-only-stage-1.R`: script to fit Stage 1 of the multivariate SAE model without spatial random effects and with unstructured province level effects, and without any covariates. 
    + `02a-ho-random-spatial-stage-2.R`: script to fit Stage 2 of the multivariate SAE model with spatial random effects and with unstructured province level effects. 
    + `02b-ho-random-nonspatial-stage-2.R`: script to fit Stage 2 of the multivariate SAE model without spatial random effects and with unstructured province level effects. 
    + `02c-ho-random-spatial-noRE-stage-2.R`: script to fit Stage 2 of the multivariate SAE model with spatial random effects and without unstructured province level effects. 
    + `02d-ho-random-nonspatial-noRE-stage-2.R`: script to fit Stage 2 of the multivariate SAE model without spatial random effects and without unstructured province level effects. 
    + `02e-ho-random-spatial-only-stage-2.R`: script to fit Stage 2 of the multivariate SAE model with spatial random effects, and with unstructured province level effects, without including any of the covariate data. 
    + `02f-ho-random-nonspatial-only-stage-2.R`: script to fit Stage 2 of the multivariate SAE model without spatial random effects and with unstructured province level effects, and without any covariates. 
    + `03-submit.sh`: script to submit all the model running files to the HPC. 
    + `04-ho-random-predict.R`: script to predict presence/absence and biomass at hold-out locations used in the analysis. 

### [results](./results)

Note that some of the large results files cannot be included on GitHub. If there is a file referenced in the code above that you would like and is not available on GitHub, contact the first author (jwdoser@ncsu.edu). 

+ `ba-ho-random-rmspe.rda`: RMSPE and correlation values from the hold-out assessment for the multivariate model. 
+ `ba-univ-nonspatial-1e+05-samples-2026-01-13.rda`: Results from Model C for the univariate model. 
+ `ba-univ-nonspatial-noRE-1e+05-samples-2026-01-13.rda`: Results from Model B for the univariate model.
+ `ba-univ-nonspatial-only-1e+05-samples-2026-01-13.rda`: Results from Model A for the univariate model. 
+ `ba-univ-spatial-1e+05-samples-2026-01-13.rda`: Results from Model F for the univariate model.  
+ `ba-univ-spatial-noRE-1e+05-samples-2026-01-13.rda`: Results from Model E for the univariate model. 
+ `ba-univ-spatial-only-1e+05-samples-2026-01-13.rda`: Results from Model D for the univariate model. 
+ `ba_univ_top_model_prediction_1000m.rda`: Prediction results for the univariate model at 1000m resolution (used for maps). 
+ `ba_univ_top_model_SAE_100m.rda`: Prediction results for the univariate model at 100m resolution (used for small area estimates).
+ `ho-ba-univ-nonspatial-1e+05-samples-2026-01-30.rda`Hold-out analysis results from Model C for the univariate model. 
+ `ho-ba-univ-nonspatial-noRE-1e+05-samples-2026-01-30.rda`: Hold-out analysis results from Model B for the univariate model.
+ `ho-ba-univ-nonspatial-only-1e+05-samples-2026-01-30.rda`: Hold-out analysis results from Model A for the univariate model. 
+ `ho-ba-univ-spatial-1e+05-samples-2026-01-30.rda`: Hold-out analysis results from Model F for the univariate model. 
+ `ho-ba-univ-spatial-noRE-1e+05-samples-2026-01-30.rda`: Hold-out analysis results from Model E for the univariate model. 
+ `ho-ba-univ-spatial-only-1e+05-samples-2026-01-30.rda`: Hold-out analysis results from Model D for the univariate model. 
+ `ho-random-auc-rmspe.rda`: Estimates of AUC from the hold-out analysis for the Stage 1 model of the multivariate model. 
+ `prelim-stage-1-waic-1-factors-2026-01-07.rda`: WAIC results from preliminary model run with 1 factor to determine the optimal number of factors for Stage 1.
+ `prelim-stage-1-waic-2-factors-2026-01-07.rda`: WAIC results from preliminary model run with 2 factors to determine the optimal number of factors for Stage 1.
+ `prelim-stage-1-waic-3-factors-2026-01-07.rda`: WAIC results from preliminary model run with 3 factors to determine the optimal number of factors for Stage 1.
+ `prelim-stage-1-waic-4-factors-2026-01-07.rda`: WAIC results from preliminary model run with 4 factors to determine the optimal number of factors for Stage 1.
+ `prelim-stage-1-waic-5-factors-2026-01-07.rda`: WAIC results from preliminary model run with 5 factors to determine the optimal number of factors for Stage 1.
+ `prelim-stage-1-waic-6-factors-2026-01-07.rda`: WAIC results from preliminary model run with 6 factors to determine the optimal number of factors for Stage 1.
+ `prelim-stage-1-waic-7-factors-2026-01-07.rda`: WAIC results from preliminary model run with 7 factors to determine the optimal number of factors for Stage 1.
+ `prelim-stage-1-waic-8-factors-2026-01-07.rda`: WAIC results from preliminary model run with 8 factors to determine the optimal number of factors for Stage 1.
+ `prelim-stage-2-waic-1-factors-2026-01-08.rda`: WAIC results from preliminary model run with 1 factor to determine the optimal number of factors for Stage 2.
+ `prelim-stage-2-waic-2-factors-2026-01-08.rda`: WAIC results from preliminary model run with 2 factors to determine the optimal number of factors for Stage 2.
+ `prelim-stage-2-waic-3-factors-2026-01-08.rda`: WAIC results from preliminary model run with 3 factors to determine the optimal number of factors for Stage 2.
+ `prelim-stage-2-waic-4-factors-2026-01-08.rda`: WAIC results from preliminary model run with 4 factors to determine the optimal number of factors for Stage 2.
+ `prelim-stage-2-waic-5-factors-2026-01-08.rda`: WAIC results from preliminary model run with 5 factors to determine the optimal number of factors for Stage 2.
+ `prelim-stage-2-waic-6-factors-2026-01-08.rda`: WAIC results from preliminary model run with 6 factors to determine the optimal number of factors for Stage 2.
+ `prelim-stage-2-waic-7-factors-2026-01-08.rda`: WAIC results from preliminary model run with 7 factors to determine the optimal number of factors for Stage 2.
+ `prelim-stage-2-waic-8-factors-2026-01-08.rda`: WAIC results from preliminary model run with 8 factors to determine the optimal number of factors for Stage 2.
+ `small-stage-1-nonspatial-2e+05-samples-4-factors-2026-01-09.rda`: results from Model C for the Stage 1 multivariate model. 
+ `small-stage-1-nonspatial-noRE-2e+05-samples-4-factors-2026-01-09.rda`: results from Model B for the Stage 1 multivariate model. 
+ `small-stage-1-nonspatial-only-2e+05-samples-4-factors-2026-01-09.rda`: results from Model A for the Stage 1 multivariate model.
+ `small-stage-1-spatial-2e+05-samples-4-factors-2026-01-09.rda`: results from Model F for the Stage 1 multivariate model. 
+ `small-stage-1-spatial-noRE-2e+05-samples-4-factors-2026-01-09.rda`: results from Model E for the Stage 1 multivariate model. 
+ `small-stage-1-spatial-only-2e+05-samples-4-factors-2026-01-09.rda`: results from Model D for the Stage 1 multivariate model. 
+ `small-stage-2-nonspatial-2e+05-samples-4-factors-2026-01-09.rda`: results from Model C for the Stage 2 multivariate model.
+ `small-stage-2-nonspatial-noRE-2e+05-samples-4-factors-2026-01-09.rda`: results from Model B for the Stage 2 multivariate model. 
+ `small-stage-2-nonspatial-only-2e+05-samples-4-factors-2026-01-09.rda`: results from Model A for the Stage 2 multivariate model.
+ `small-stage-2-spatial-2e+05-samples-4-factors-2026-01-09.rda`: results from Model F for the Stage 2 multivariate model. 
+ `small-stage-2-spatial-noRE-2e+05-samples-4-factors-2026-01-09.rda`: results from Model E for the Stage 2 multivariate model.
+ `small-stage-2-spatial-only-2e+05-samples-4-factors-2026-01-09.rda`: results from Model D for the Stage 2 multivariate model.
+ `top_model_prediction_1000m.rda`: results from the top multivariate model for prediction at a 1000m resolution (used for maps).
+ `top_model_prediction_results_100m.rda`: results from the top multivariate model for prediction at a 100m resolution (used for generating small area estimates).
+ `top_model_SAE_100m.rda`: small area estimates generated from the top multivariate model. 

### [figures](./figures/)

Contains all figures produced in the main text and the supplement. The file names indicate either the figure number from the paper, or the purpose of the figure. 


