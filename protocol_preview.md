---
runtime: shiny
output: html_document
---



<h4><center> - ODMAP protocol - </center></h4><h2><center></center></h2><p><b>Authors: </b>Damaris Zurell, Niklaus E. Zimmermann, Helge Gross, Andri Baltensweiler, Thomas Sattler, Rafael O. Wüest</p><p><b>Contact: </b></p><p><b>Date: </b>2020-02-20</p>
______________


## Overview 


#### Authorship 

Contact : damaris@zurell.de


#### Model objective 

Model objective: Mapping and interpolation

Target output: continuous occurrence probabilities and binary maps of potential presence


#### Focal Taxon 

Focal Taxon: (i) forest bird species, and (ii) bush and tree species


#### Location 

Location: Switzerland


#### Scale of Analysis 

Spatial extent: 5.76° W, 8.12° W, 45.70° N , 47.93° N (xmin, xmax, ymin, ymax)

Spatial resolution: (i) 1x1 km for forest bird species, and (ii) 100x100 m for bush and tree species

Temporal extent: (i)1993-1996 for birds, and (ii) 2004-2006 for trees

Boundary: political


#### Biodiversity data 

Observation type: standardised monitoring data

Response data type: presence/absence


#### Predictors 

Predictor types: climatic, topographic, vegetation structure


#### Hyptheses 

Hypotheses: Based on previous studies, we tested climate, topography and vegetation structure as important environmental predictor variables for bird and tree species in Switzerland in an exploratory way.


#### Assumptions 

Model assumptions: We assumed that species are at pseudo-equilibrium with the environment. Also, we assumed that detection errors were negligible. Previous analyses of the Swiss breeding bird data have shown that the sampling approach ensures high species detectability of approximately 90% (Kéry & Schmid, 2006). In the forest inventory, tree individuals below a certain diameter at breast height are not recorded and we assume that this procedure does not bias the species identification.


#### Algorithms 

Modelling techniques: glm, gam, brt, randomForest, JSDM

Model complexity: Model settings were chosen to yield intermediately complex response surfaces. We allowed quadratic relationships in GLMs and JSDMs, and restricted GAMs, BRTs and RFs such that these would not overfit too much.

Model averaging: We combined the four SDMs to ensemble SDM predictions.


#### Workflow 

Model workflow: Prior to model building, all predictor variables were standardised. In each model, we only included the five most important and weakly correlated variables. Univariate variable importance for each predictor was assessed in a 5-fold spatial block cross-validation design. Ensemble predictions from SDMs and richness models were derived using un-weighted ensemble means. Predictive model performance was assessed using a 5-fold spatial block cross-validation.


#### Software 

Software: All analyses were conducted using R version 3.3.2 (R Core Team, 2016) with packages sperrorest (Brenning, 2012), mgcv (Wood, 2011), gbm (Ridgeway, 2013), dismo (Hijmans et al., 2017), randomForest (Liaw & Wiener, 2002), boral (Hui, 2016), ncf (Bjornstad, 2016), PresenceAbsence (Freeman & Moisen, 2008), ecospat (Broennimann et al., 2016) and lme4 (Bates et al., 2015). 

Code availability: All codes were provided in the Online Supplementary Information and on github (https://github.com/damariszurell/SSDM-JSDM). 

Data availability: Data are available from Dryad (https://doi.org/10.5061/dryad.k88v330). 


## Data 


#### Biodiversity data 

Taxon names: All species are listed in Table S1 and S2 of the original publication.

Taxonomic reference system: We follow the taxonomy of the Swiss breeding bird atlas and the Swiss National Forest Inventory.

Ecological level: populations

Data sources: Bird presence-absence data at a 1x1 km spatial resolution were obtained from the Swiss breeding bird atlas (Schmid et al., 1998). Tree species presence-absence data were obtained from the Swiss National Forest Inventory (NFI). 

Sampling design: Bird atlas data were recorded over a four-year period (1993-1996) in usually three visits per year (two above the treeline) using a simplified territory mapping approach. The NFI samples Switzerland on a regular grid (spacing 1.4 km), and in case the sample falls into forest it records forest characteristics in a maximal area of 50x50 m (Brassel & Lischke, 2001). 

Sample size: The bird data set contained 2535 1x1 km cells with a total number of 56 forest bird species and prevalence ranging 0.03-0.73.  The tree data set contained 6946 100x100 m cells with a total number of 63 tree and shrub species and prevalence ranging 0.01-0.79.

Clipping: We clipped all data to the political boundary of Switzerland.

Scaling: We aggregated the NFI presence-absence data to 100x100 m plot size to match the minimum spatial grain of available environmental data. 

Cleaning: We only considered species with at least 50 presences. 

Absence data: The Swiss breeding bird atlas and the NFI contains presence and absence data. In the Swiss breeding bird data, species are listed as absent in a site if they were not encountered within the 2-3 visits per breeding season in the four successive years of recording. Previous analyses have shown that this sampling approach ensures high species detectability of approximately 90% (Kéry & Schmid, 2006). In the NFI plots, all tree and bush individuals above a certain diameter at breast height are recorded. We have no information about potential species biases in unrecorded small trees and bushes.


 <span style='color:#DC3522'>\<Background data\> </span>


#### Data partitioning 

Training data: We randomly selected 70% of data (1774 cells for birds and 4862 for tree species) for model building . 

Validation data: Predictive model performance for single species was assessed using a 5-fold spatial block cross-validation design. Therefore, for each dataset we split the study region into five rectangular tiles (R package “sperrorest”). The resulting sample sizes per tile ranged 222-568 cells for forest birds and 570-1440 cells for tree species.

Test data: We left 30% of the data (761 cells for birds and 2084 cells for tree species) for independent validation of the community predictions. 


#### Predictor variables 

Predictor variables: - Topography: slope, aspect, topographic position index (TPI), topographic wetness index (TWI), potential monthly solar radiation
- Climate: 19 bioclimatic predictors, degree days, potential evapotranspiration (PET), moisture balance (MBAL), moisture index (MIND)
- Vertical vegetation structure: LiDAR-derived average height, height standard deviation, height coefficient of variation, as well as 10th, 25th and 95th height percentiles, canopy cover (COV), canopy density (DNS), foliage height diversity (FHD), understory height diversity (UHD)
- Horizontal vegetation structure (for bird data only): edge length between two height classes, clumpiness for different height classes

Data sources: We prepared the environmental predictor variables climate, topography and LiDAR-derived vegetation structure. Monthly average climate data were obtained from the Federal Office of Meteorology and Climatology MeteoSwiss (www.meteosuisse.ch). Topography and LiDAR data were obtained from the Swiss Federal Office of Topography.


 <span style='color:#DC3522'>\<Spatial extent\> </span>

Spatial resolution: The raw resolution of topography and climate was 100 m. The raw LIDAR point clouds had a nominal footprint of 0.3 m. Data were prepared at the same resolution as the species data, meaning at 1x1 km for analyses of forest birds and at 100x100 m for analyses of tree species.

Projection: Swiss coordinate system (Swiss grid)


 <span style='color:#DC3522'>\<Temporal extent\> </span>

Data processing: - Topography: Slope, aspect, TPI and TWI (Wilson & Gallant, 2000) were based on a 100 m digital elevation model. TPI in a cell corresponds to the difference of the focal cell to the mean of its eight surrounding cells, with negative values indicating a depression,  positive values a rise. Potential monthly solar radiation was calculated following Hofierka et al. (2002). 
- Climate: Monthly average climate data for the period 1981-1990 were generated by interpolating ca. 300 MeteoSwiss station data to a resolution of 100x100m using the Daymet software (Thornton et al., 1997). From these, we derived 19 bioclimatic predictors (http://worldclim.org/bioclim) as well as so-called degree days. Degree days constitute the sum of all monthly temperature values greater than a given threshold temperature multiplied by the total number of days (with thresholds 0°C and 5°C for DDEG0 and DDEG5). We calculated PET using radiation as proposed by Makkink (1957), as this method was shown to best approximate PET in Switzerland (Xu & Singh, 2002). For precipitation and PET we calculated summer (April to September) and winter (October to March) averages as well as their ratio. We calculated the MBAL as the difference between precipitation and PET, and MIND as the ratio between PET and precipitation. For the analyses of forest bird species, the topographic and climatic data were aggregated to 1x1 km grids using the mean as aggregate-function such that the grid cell-size matched the bird survey data.
- Vertical vegetation structure: LiDAR-derived data was processed with the LAStools software (Isenburg, 2015). The Swiss-wide LiDAR dataset consists of discrete first and last pulse returns with a nominal footprint of 0.3 m and a point density of 0.5 points/m2 (Artuso et al., 2003). LiDAR variables were generated separately as raster datasets for both the 100x100 m grid that matches the tree species NFI data, as well as the 1x1 km grid that matches the bird survey data. From the terrain corrected and classified LiDAR point cloud (heights of classified vegetation LiDAR returns minus interpolated DTM heights), we calculated the average height, their standard deviation and coefficient of variation as well as the corresponding 10th, 25th and 95th percentiles per 100 m and 1 km pixel, respectively. In addition, we also derived the canopy cover (percentage of first returns above 1m; COV) and canopy density (ratio of all returns above 1m divided by all returns; DNS). We further calculated both standard deviation and coefficient of variation in order to characterize vertical variation in LiDAR returns. In addition, we estimated the so-called foliage height diversity (FHD), which is the Shannon diversity index based on 5m vertical bins as H = pi ln(pi), where pi is the proportion of LiDAR returns in the 5m bin i. Additionally, we calculated understory height diversity (UHD) for the 1x1 km grid as we considered these important predictors for bird distributions in Swiss forests. UHD was derived analogously to FHD but using 1m bins restricted to below 12m (Zellweger et al., 2016) as well as the ratio between the 95th and 25th percentile.
- Horizontal vegetation structure: For the bird data at 1x1 km resolution, we derived LiDAR variables accounting for edge effects and fragmentation in order to describe the horizontal structural heterogeneity of the vegetation (Zellweger et al., 2013). Based on the terrain corrected LiDAR point cloud we generated a gridded Canopy Height Model (CHM) with a grid size of 20 m. The CHM was classified into three classes, which are non-forest (vegetation height < 1m), understory/midstory (1 – 12m) and canopy (> 12m). The length of edges between two height classes (e.g. non-forest/canopy) was calculated for each grid cell and summed up for the 1x1 km grid of the bird survey data. To measure the spatial dispersion or aggregation of the vegetation height classes understory/midstory and canopy, a clumpiness index was calculated for the 1x1 km grid using the software FRAGSTATS (McGarigal et al., 2012).
- Because the forest bird data were recorded 1993-1996 and the LiDAR data were recorded after 2000, there is a temporal mismatch between species data and vegetation data. Generally, the forest laws in Switzerland are very strict and we can, thus, rule out any major changes in the vegetation structure between these two periods.  The main exception is storm damage due to the cyclone “Lothar” in 1999. In storm-damaged sites we can expect differences in vegetation structure between the recording periods of the bird survey data (before the cyclone) and the LiDAR data (after the cyclone). Hence, we removed all storm-damaged sites (n=10) from the analyses to avoid mismatches in vegetation structure.


#### Transfer data 


 <span style='color:#DC3522'>\<Data sources\> </span>


 <span style='color:#DC3522'>\<Spatial extent\> </span>


 <span style='color:#DC3522'>\<Spatial resolution\> </span>


 <span style='color:#DC3522'>\<Temporal extent\> </span>


 <span style='color:#DC3522'>\<Models and scenarios\> </span>


 <span style='color:#DC3522'>\<Quantification of Novelty\> </span>


## Model 


#### Variable pre-selection 

Variable pre-selection: In each model, we only included the five most important and weakly correlated variables obtained from the cross-validated univariate variable importance.


#### Multicollinearity 

Multicollinearity: We reduced the predictor set to variables with bivariate Spearman correlations |r|<0.7, retaining those variables from highly correlated pairs with higher cross-validated univariate importance (Dormann et al., 2013).  Univariate variable importance for each predictor was assessed in a 5-fold spatial block cross-validation design by estimating univariate GAMs on 4 of 5 folds (with logit link for species occurrences and log link for species richness) and cross-predicting these to the left-out fold with 5 repetitions. From the cross-predictions, we calculated the percentage of explained deviance. We thus obtained a predictor ranking for each single species and removed the less important variables from pairs of highly correlated variables. However, JSDMs require a global set of predictor variables and we thus selected those five variables with highest mean cross-validated univariate importance among all species. SDMs and richness models were run with the same set of global predictors as in JSDMs.  In a sensitivity analysis we additionally estimated SDMs and richness models using the five most important variables selected individually for each species and species richness, respectively. This did not change the overall results.


#### Model settings 

glm: family (binomial for SDMs and Poisson for richness models), formula (linear and quadratic)

gam: family (binomial for SDMs and Poisson for richness models), formula (cubic smoothing splines with four degrees of freedom)

brt: distribution (binomial for SDMs and Poisson for richness models), nTrees (1000-5000), interactionDepth (2), shrinkage (variable learning rate such that 1000-5000 trees were fitted (Elith et al., 2008)), bagFraction (0.75)

randomForest: ntree (1000), nodesize (20)

JSDM: family (binomial), formular (linear and quadratic terms), number of latent variables (5), iterations (50000 for birds, 100000 for trees), burnin (20000 for birds, 50000 for trees), thinning rate (50), convergence diagnostic (Geweke)


 <span style='color:#DC3522'>\<Model settings (extrapolation)\> </span>


#### Model estimates 

Coefficients: We did not analyse model coefficients in depths, but compared for each species how often each predictor was selected among the five most important variables.


#### Model selection - model averaging - ensembles 

Model ensembles: Consensus predictions from SDMs and richness models, respectively, were generated using un-weighted ensemble means.


#### Analysis and Correction of non-independence 

Spatial autocorrelation: Spatial autocorrelation in model residuals was assessed using spline correlograms in the R package “ncf” (Bjornstad, 2016).


#### Threshold selection 

Threshold selection: Binary predictions were derived by using the TSS (true skill statistic)-maximisation threshold. Maximising TSS is equivalent to maximising the sum of sensitivity and specificity.


## Assessment 


#### Performance statistics 


 <span style='color:#DC3522'>\<Performance on training data\> </span>

Performance on validation data: AUC, TSS, True positive rate, True negative rate

Performance on test data: assemblage sensitivity (rate of true species presence), assemblage specificity (rate of true species absence), assemblage TSS, assemblage prediction success, species richness error


#### Plausibility check 

Response shapes: In our pre-analyses, we used inflated response curves to understand model behaviour for different parameter settings, and based on these checks decided for intermediate model complexity.


 <span style='color:#DC3522'>\<Expert judgement\> </span>


## Prediction 


#### Prediction output 

Prediction unit: For further analyses, we used continuous predictions of occurrence probability per species and site as well as predicted presence per species and site that was obtained by binarising the predicted occurrence probabilities using the TSS-maximisation threshold.


#### Uncertainty quantification 

Algorithmic uncertainty: In SDMs, we accounted for algorithmic uncertainty by applying an ensemble approach averaging over four different SDM algorithms. In a sensitivity analysis, we also compared predictions from GLMs only against JSDMs.


 <span style='color:#DC3522'>\<Scenario uncertainty\> </span>


 <span style='color:#DC3522'>\<Novel environments\> </span>
