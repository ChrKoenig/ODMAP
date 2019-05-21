library(shiny)
library(shinyjs)
library(shinyWidgets)
library(tidyverse)

# DEFINE UI
ui <- navbarPage(
  id = "navbar",
  title = "ODMAP",
  position = "fixed-top",
  header =  tags$head(tags$style(
    HTML('h3 { color: #48ca3b; padding-top: 45px}'),
    HTML('shiny-text-output { padding-top: 45px}'),
    HTML(".shiny-input-container:not(.shiny-input-container-inline) { width: 80%;}")
  )),
  
  # HOME TAB
  tabPanel("What is ODMAP?", value = "tab_home", fluidPage(
    useShinyjs(),
    h3("What is ODMAP?"),
    p("Species distribution models (SDMs) constitute the most common class of biodiversity models. The advent of ready-to-use software packages and
             increasing availability of digital geo-information have considerably assisted the application of SDMs in recent years  enabling their use in
             informing conservation and management, and quantifying impacts from global change. However, models must be fit for purpose, with all important
             aspects of their development and applications properly considered. . Despite the widespread use of SDMs, the standardisation and documentation
             of model protocols remains limited. To address these issues, we propose a standard protocol for reporting SDMs. We call this the
             ODMAP (Overview, Data, Model, Assessment and Prediction) protocol as each of its components reflectsthe main steps involved in building SDMs
             and other empirically-based biodiversity models. The ODMAP protocol serves two main purposes. First, it provides a checklist for authors
             detailing key steps for model building and analyses. Second, it introduces a standard approach todocumentation that ensures transparency and
             reproducibility, facilitatingpeer review and expert evaluation of model quality as well as meta-analyses."),
    img(src = "workflow.jpg", width = "400", align = "center")
  )),
  
  # OVERVIEW TAB
  tabPanel("1. Overview", value = "tab_1",  fluidPage(
    useShinyjs(),
    width = 10,
    h3("Give a brief overview on your SDM study"),
    
    h4("Study Objective"),
    selectInput("o_objective_1", selected = NULL, multiple = F, choices = c("", "Inference and explanation", "Prediction and mapping", "Projection and transfer"), label = NULL),
    textAreaInput("o_objective_2", placeholder = "Target output: e.g. suitable vs. unsuitable habitat, continuous habitat suitability index, abundance", resize = "vertical", label = NULL),
    
    h4("Taxon & ecological scale"),
    textAreaInput("o_tax_eco_1", placeholder = "Taxon names: e.g., names of subspecies, species, genus, families", resize = "vertical", label = NULL),
    textAreaInput("o_tax_eco_2", placeholder = "Ecological level: e.g., operational taxonomic units, individuals, populations, species, communities", resize = "vertical", label = NULL),
    
    h4("Location"),
    textAreaInput("o_location_1", placeholder = "Location and extent of study area", resize = "vertical", label = NULL, rows = 1),
    
    h4("Species data overivew"),
    textAreaInput("o_spec_dat_1", placeholder = "Data source: e.g., own field data or data from external provider", resize = "vertical", label = NULL),
    textAreaInput("o_spec_dat_2", placeholder = "Observation type: e.g., standardised monitoring data, expert knowledge, citizen science, heterogenous types", resize = "vertical", label = NULL),
    textAreaInput("o_spec_dat_3", placeholder = "Data type: e.g., presence-only, presence/absence, counts, GPS locations (from individualtracking data)", resize = "vertical", label = NULL),
    textAreaInput("o_spec_dat_4", placeholder = "Sampling design, if applicable: e.g., random, uniform, environmentally stratified, opportunistic", resize = "vertical", label = NULL),
    textAreaInput("o_spec_dat_5_optional", placeholder = "Time period of data collection (optional)", resize = "vertical", label = NULL),
    textAreaInput("o_spec_dat_6", placeholder = "Sample size (incl. prevalence)", resize = "vertical", label = NULL),
    
    h4("Spatial and temporal scale"),
    textAreaInput("o_spat_temp_1", placeholder = "Spatial resolution and extent, type of extent boundary (e.g., natural or political)", resize = "vertical", label = NULL),
    textAreaInput("o_spat_temp_2", placeholder = "Temporal resolution and extent", resize = "vertical", label = NULL),
    
    h4("Conceptual model"),
    textAreaInput("o_concept_1", placeholder = "Hypotheses about species-environment relationships", resize = "vertical", label = NULL),
    textAreaInput("o_concept_2", placeholder = "Response variable: e.g. presence/absence, abundance, species richness", resize = "vertical", label = NULL),
    textAreaInput("o_concept_3", placeholder = "Justification of considered predictor variables and their scales", resize = "vertical", label = NULL),
    
    h4("Assumptions"),
    textAreaInput("o_assumpt_1", placeholder = "Critical model assumptions", resize = "vertical", label = NULL),
    
    h4("SDM algorithms"),
    textAreaInput("o_assumpt_1", placeholder = "Critical model assumptions", resize = "vertical", label = NULL),
    textAreaInput("o_assumpt_1", placeholder = "Model complexity", resize = "vertical", label = NULL),
    
    h4("Modeling workflow"),
    textAreaInput("o_workflow_1", placeholder = "Conceptual description of modelling steps including model fitting, assessment and prediction", resize = "vertical", label = NULL),
    
    h4("Software"),
    textAreaInput("o_software_1", placeholder = "Specify modelling platform incl. version, key packages used, availability of source codes and data", resize = "vertical", label = NULL)
  )),
  
  # DATA TAB
  tabPanel("2. Data", value = "tab_2", fluidPage(
    useShinyjs(),
    h3("Describe your data"),
    
    h4("Species data"),
    textAreaInput("d_spec_dat_1_optional", placeholder = "Details on external species data source: e.g., URL/DOI, accession date, database version", resize = "vertical", label = NULL),
    textAreaInput("d_spec_dat_2", placeholder = "Details on taxonomic reference system", resize = "vertical", label = NULL),
    textAreaInput("d_spec_dat_3_optional", placeholder = "Details on observation type, if applicable: e.g., standardised monitoring data, expert knowledge, citizen science, heterogenous types", resize = "vertical", label = NULL),
    textAreaInput("d_spec_dat_4_optional", placeholder = "Details on spatial and temporal sampling design, temporal replications, nestedness", resize = "vertical", label = NULL),
    textAreaInput("d_spec_dat_5", placeholder = "Details on sample size per taxon: e.g., number of observations/counts, prevalence", resize = "vertical", label = NULL),
    textAreaInput("d_spec_dat_6_optional", placeholder = "Details on potential errors and biases in data, if applicable: e.g., detection probability, misidentification potential, geo-referencing errors, sampling bias", resize = "vertical", label = NULL),
    textAreaInput("d_spec_dat_7_optional", placeholder = "Details on data cleaning/filtering steps, if applicable: e.g., taxonomically, spatially, temporally, outlier presence/treatment", resize = "vertical", label = NULL),
    textAreaInput("d_spec_dat_8_optional", placeholder = "Details on scaling, if applicable: e.g., rasterisation of polygon maps, spatial and temporal thinning, measures to address spatial uncertainties", resize = "vertical", label = NULL),
    
    h4("Absence/background data"),
    textAreaInput("d_absence_1", placeholder = "Details on absence data collection, if applicable", resize = "vertical", label = NULL),
    textAreaInput("d_absence_2", placeholder = "Details on background data derivation, if applicable: e.g., spatial and temporal extent, spatial and temporal buffer, bias correction (e.g. target group sampling)", resize = "vertical", label = NULL),
    
    h4("Data partitioning", id = "d_partit"),
    textAreaInput("d_partit_1", placeholder = "Selection of training data (for model fitting)", resize = "vertical", label = NULL),
    textAreaInput("d_partit_2", placeholder = "Selection of validation data (withheld from model fitting, used for estimating prediction error for model selection, model averaging or ensemble): e.g., cross-validation method", resize = "vertical", label = NULL),
    textAreaInput("d_partit_3_optional", placeholder = "Selection of test (truly independent) data , sensu Hastie, et al. (2009)", resize = "vertical", label = NULL),
    
    h4("Environmental data / Predictor variables"),
    textAreaInput("d_env_data_1", placeholder = "Details on data sources: e.g., URL/DOI, accession date, database version", resize = "vertical", label = NULL),
    textAreaInput("d_env_data_2_optional", placeholder = "Details on measurements errors and bias, when known", resize = "vertical", label = NULL),
    textAreaInput("d_env_data_3", placeholder = "Spatial and temporal resolution and extent", resize = "vertical", label = NULL),
    textAreaInput("d_env_data_4_optional", placeholder = "Details on data processing and on spatial, temporal and thematic scaling: e.g. upscaling/downscaling, transformations, normalisations, thematic aggregations (e.g. of land cover classes), measures to address spatial uncertainties", resize = "vertical", label = NULL),
    textAreaInput("d_env_data_5_optional", placeholder = "Details on dimension reduction of variable set, if applicable – if model-based, this should be contained in Model section (element: Details on pre-selection of variables)", resize = "vertical", label = NULL),
    
    h4("Transfer data for projection", id = "d_proj"),
    textAreaInput("d_proj_data_1", placeholder = "Details on data sources: e.g., URL/DOI, accession date, database version", resize = "vertical", label = NULL),
    textAreaInput("d_proj_data_2", placeholder = "Models and scenarios used", resize = "vertical", label = NULL),
    textAreaInput("d_proj_data_3", placeholder = "Spatial and temporal resolution and extent", resize = "vertical", label = NULL),
    textAreaInput("d_proj_data_4_optional", placeholder = "Details on data processing and scaling (see above)", resize = "vertical", label = NULL),
    textAreaInput("d_proj_data_5", placeholder = "Quantification of novel environmental conditions and novel environmental combinations: e.g., distance to training data", resize = "vertical", label = NULL)
  )),
  
  # MODEL TAB
  tabPanel("3. Model", value = "tab_3", fluidPage(
    useShinyjs(),
    h3("Describe your modeling framework"),
    
    h4("Multicollinearity"),
    textAreaInput("m_multicoll_1", placeholder = "Methods for identifying and dealing with multicollinearity (Dormann, et al. 2013) or justification if multicollinearity is not explicitly dealt with", resize = "vertical", label = NULL),
    
    h4("Variable pre-selection"),
    textAreaInput("m_pre_select_1_optional", placeholder = "Details on pre-selection of variables, if applicable", resize = "vertical", label = NULL),
    
    h4("Parameter settings / model complexity"),
    textAreaInput("m_complexity_1", placeholder = "Name selected model techniques", resize = "vertical", label = NULL),
    textAreaInput("m_complexity_2", placeholder = "Details on model complexity and parameter settings for all selected algorithms (including default settings for platforms such as biomod and Maxent)", resize = "vertical", label = NULL),
    textAreaInput("m_complexity_3", placeholder = "Weighting of data", resize = "vertical", label = NULL),
    textAreaInput("m_complexity_4_optional", placeholder = "Details on relevant parameter settings for extrapolation beyond sample range, if applicable: e.g., clamping", resize = "vertical", label = NULL),
    
    h4("Model selection / Model averaging / Ensembles", id = "m_select_optional"),
    textAreaInput("m_select_1_optional", placeholder = "Details on model selection strategy: e.g. information-theoretic approach for variable selection, shrinkage and regularization", resize = "vertical", label = NULL),
    textAreaInput("m_select_2_optional", placeholder = "Details on model averaging: e.g. derivation of weights", resize = "vertical", label = NULL),
    textAreaInput("m_select_3_optional", placeholder = "Details on ensemble method: e.g. initial conditions (input data)", resize = "vertical", label = NULL),
    
    h4("Non-indepencence correction / analysis", id = "m_select_optional"),
    textAreaInput("m_autocorr_1", placeholder = "Method for addressing spatial autocorrelation in residuals", resize = "vertical", label = NULL),
    textAreaInput("m_autocorr_2_optional", placeholder = "Method for addressing temporal autocorrelation in residuals", resize = "vertical", label = NULL),
    textAreaInput("m_autocorr_3_optional", placeholder = "Method to account for nested data: e.g., fixed and random effects", resize = "vertical", label = NULL),
    
    h4("Threshold selection", id = "m_threshold"),
    textAreaInput("m_threshold_1", placeholder = "Details on threshold selection, if applicable: transforming continuous predictions into binary predictions", resize = "vertical", label = NULL)
  )),
  
  # ASSESSMENT TAB
  tabPanel("4. Assessment", value = "tab_4", fluidPage(
    useShinyjs(),
    h3("Describe your statistical assessment framework"),
    
    h4("Preformance statistics"),
    textAreaInput("a_perform_1", placeholder = "Performance statistics estimated on training data", resize = "vertical", label = NULL),
    textAreaInput("a_perform_2", placeholder = "Performance statistics estimated on validation data (from data partitioning)", resize = "vertical", label = NULL),
    textAreaInput("a_perform_3", placeholder = "Performance statistics estimated on test (truly independent) data, if applicable", resize = "vertical", label = NULL),
    
    h4("Model estimates", id = "a_estimates"),
    textAreaInput("a_estimates_1", placeholder = "Assessment of model coefficients, variable importance", resize = "vertical", label = NULL),
    
    h4("Response shapes", id = "a_resp_shapes"),
    textAreaInput("a_resp_shapes_1", placeholder = "Plausibility check: e.g., partial response plots, evaluation strips, inflated response plots", resize = "vertical", label = NULL)
    
  )),
  
  # PREDICTION TAB
  tabPanel("5. Prediction", value = "tab_5", fluidPage(
    h3("Describe your prediction framework"),
    
    h4("Uncertainty quantification"),
    textAreaInput("a_uncertainty_1_optional", placeholder = "Algorithmic uncertainity, if applicable", resize = "vertical", label = NULL),
    textAreaInput("a_uncertainty_2_optional", placeholder = "Uncertainty in input data, if applicable", resize = "vertical", label = NULL),
    textAreaInput("a_uncertainty_3_optional", placeholder = "Error propagation in Hierarchical/Bayesian models, if applicable", resize = "vertical", label = NULL),
    textAreaInput("a_uncertainty_4", placeholder = "Prediction unit", resize = "vertical", label = NULL),
    textAreaInput("a_uncertainty_5", placeholder = "Uncertainty in scenarios (e.g. climate models, land use models, storylines)", resize = "vertical", label = NULL),
    textAreaInput("a_uncertainty_6", placeholder = "Treatment of novel environments: e.g., masking", resize = "vertical", label = NULL)
  )),
  
  # EXPORT TAB
  tabPanel("Export", value = "tab_export", fluidPage(
    h3("Export ODMAP protocol")
  )),
  
  # CONTROL FOR OPTIONAL FIELDS
  fixedPanel(
    top = "13%",
    right = "0%",
    checkboxInput("toggle_optional", "Show optional fields", value = T, width = "50%")
  )
)

# DEFINE SERVER
server <- function(input, output, session) {
  elem_hide_obj1 = c("#o_tax_eco_1", "#d_partit", "#d_partit_1", "#d_partit_2", "#d_partit_3_optional", "#d_proj", "#d_proj_data_1", # ODMAP elements not needed for objective "Inference and explanation"
                     "#d_proj_data_2", "#d_proj_data_3", "#d_proj_data_4_optional", "#d_proj_data_5", "m_complexity_4_optional",
                     "#m_threshold", "#m_threshold_1", "#a_perform_2", "#a_perform_3")
  elem_hide_obj2 = c("d_proj", "#d_proj_data_1", "#d_proj_data_2", "#d_proj_data_3", "#d_proj_data_4_optional", "#d_proj_data_5", # ODMAP elements not needed for objective "Prediction and mapping"
                     "#m_complexity_4_optional", "#a_estimates", "#a_estimates_1", "#a_resp_shapes", "#a_resp_shapes_1", "#a_uncertainty_5",
                     "#a_uncertainty_6")
  elem_hidden = c()
  
  observeEvent(input$o_objective_1, {
    obj = input$o_objective_1
    if(obj == "Inference and explanation"){
      elem_show = setdiff(elem_hidden, elem_hide_obj1)
      shinyjs::show(selector = elem_show)
      shinyjs::hide(selector = elem_hide_obj1)
      hideTab("navbar", "tab_5")
      elem_hidden <<- elem_hide_obj1
    }
    if(obj== "Prediction and mapping"){
      elem_show = setdiff(elem_hidden, elem_hide_obj2)
      shinyjs::show(selector = elem_show)
      showTab("navbar", "tab_5")
      shinyjs::hide(selector = elem_hide_obj2)
      elem_hidden <<- elem_hide_obj2
    }
    if(obj== "Projection and transfer"){
      shinyjs::show(selector = elem_hidden)
      showTab("navbar", "tab_5")
      elem_hidden <<- c()
    }
  })
  
}

# Run the application
shinyApp(ui, server)

