library(shiny)
library(shinyjs)
library(shinyWidgets)
library(shinythemes)
library(shinydashboard)
library(DT)

ui <- navbarPage(
  id = "navbar",
  title = "ODMAP v1.0",
  position = "fixed-top",
  theme = shinytheme("cosmo"), # general theme
  includeCSS("www/odmap.css"), # some css tweaks
  selected = "tab_1",
  useShinyjs(),
  
  # HOME TAB
  tabPanel("What is ODMAP?", value = "tab_1", fluidPage(
    fluidRow(
      column(width = 2),
      column(width = 8, tabsetPanel(
        tabPanel("About", value = "about", fluidPage(
          strong(p("What is ODMAP?", style = "padding-top: 10px")),
          p("Species distribution models (SDMs) constitute the most common class of biodiversity models. The advent of ready-to-use software packages and
             increasing availability of digital geo-information have considerably assisted the application of SDMs in recent years  enabling their use in
             informing conservation and management, and quantifying impacts from global change."),
          p("However, models must be fit for purpose, with all important aspects of their development and applications properly considered. 
             Despite the widespread use of SDMs, the standardisation and documentation of model protocols remains limited. To address these issues, 
             we propose a standard protocol for reporting SDMs. We call this the ODMAP (Overview, Data, Model, Assessment and Prediction) protocol
             as each of its components reflectsthe main steps involved in building SDMs and other empirically-based biodiversity models."), 
          img(src = "workflow.jpg", width = "70%", style="display: block; margin-left: auto; margin-right: auto; min-width: 600px;"), br(),
          p("The ODMAP protocol serves two main purposes. First, it provides a checklist for authors detailing key steps for model building and analyses. 
             Second, it introduces a standard approach to documentation that ensures transparency and reproducibility, facilitating peer review and 
             expert evaluation of model quality as well as meta-analyses."),
          p("This application helps to implement the ODMAP approach and produces well formatted protocols that can be exported for further usage. For further explanation please refer to the original publication (Zurell et al., under review)."),
          em(p("Please cite as follows:")),
          p("Zurell D, Franklin J, König C, Bouchet PJ, Serra-Diaz JM, Dormann CF, Elith J, Fandos Guzman G, Feng X, Guillera-Arroita G, Guisan A, Leitão PJ, Lahoz-Monfort JJ, Park DS, Peterson AT,  Raacciuolo G, Schmatz D, Schröder B, Thuiller W, Yates KL, Zimmermann NE, Merow C (under review) A standard protocol for describing species distribution models.")
        )),
        tabPanel("How to use this app", value = "howto", fluidPage(
          strong(p("How to create an ODMAP protocol", style = "padding-top: 10px")),
          p("Enter all relevant information into the fields provided under 'Create a protocol'. Your progress in individual ODMAP sections is displayed in the 
             side bar at the left. The 'hide optional' switch on the left allows you to only display mandatory fields, which depend on the model objective chosen.
             A preview of your current protocol as available in the 'Protocol Viewer'."),
          p("You can always save your progress by clicking the download button on the left. After downloading your protocol, it is safe to close the Shiny app. 
             You will be able to resume working on your protocol by choosing the Upload tab above and uploading your previously saved ODMAP protocol. 
             In addition, you can import objects generated with", 
             a(href = 'https://cran.r-project.org/web/packages/rangeModelMetadata/index.html', 'rangeModelsMetaData', .noWS = "outside"),
            "R-package to autofill your ODMAP protocol.")
        ))
      )),
      column(width = 2)
      )
    )),
    
    tabPanel("Create a protocol", value = "tab_2", sidebarLayout(
      sidebarPanel(
        style = "position:fixed; width: 16%;",
        width = 2,
      
        h5("Progress", style = "font-weight: bold"),
        uiOutput("progress_bars"),
        
        h5("Hide optional fields", style = "font-weight: bold"),
        materialSwitch("hide_optional", label = NULL, status = "danger"),
        
        h5("Download protocol", style = "font-weight: bold"),
        radioButtons("document_format", label = NULL, choices = c("csv", "docx")),
        downloadButton("protocol_download")
      ),
      
      mainPanel(
        tabsetPanel(
          id = "tabset",
          tabPanel("1. Overview", value = "Overview",  fluidPage(
            em(p("Give a brief overview of all important parts of your study.", style = "padding-top: 10px; font-weight: 300")),
            uiOutput("Overview_UI")
          )),
          
          tabPanel("2. Data", value = "Data", fluidPage(
            em(p("Describe your your data in detail.", style = "padding-top: 10px; font-weight: 300")),
            uiOutput("Data_UI")
          )),
          
          tabPanel("3. Model", value = "Model", fluidPage(
            em(p("Describe your modeling approach in detail.", style = "padding-top: 10px; font-weight: 300")),
            uiOutput("Model_UI")
          )),
          
          tabPanel("4. Assessment", value = "Assessment", fluidPage(
            em(p("Describe how you assessed your model results.", style = "padding-top: 10px; font-weight: 300")),
            uiOutput("Assessment_UI")
          )),
          
          tabPanel("5. Prediction", value = "Prediction", fluidPage(
            em(p("Describe your model predictions in detail.", style = "padding-top: 10px; font-weight: 300")),
            uiOutput("Prediction_UI")
            # ADD next previous buttons
          )) 
        ))
    )),
    
    # PREVIEW PROTOCOL
    tabPanel("Protocol viewer", value = "tab_3", fluidPage(
      fluidRow(
        column(width = 2),
        column(width = 8, htmlOutput("markdown")),
        column(width = 2)
      )
    )),
    
    tabPanel("Upload / Import", value = "tab_4", fluidPage(
      fluidRow(
        column(width = 2),
        column(width = 8, 
               p("There are two options for importing data into your ODMAP protocol"),
               p(tags$b("(1) Upload an ODMAP protocol (.csv)"), br(), "This option is convenient if you want to edit or resume working on a previously saved ODMAP protocol."),
               p(tags$b("(2) Upload an RMMS file (.RDS or .csv)"), br(), "The rangeModelMetaData package of Merow et al. (2019) allows exporting standardized metadata 
                         for SDMs directly from R. Note that the objectives of ODMAP and RMMS differ and not all fields can be mapped between both approaches. 
                         This option is therefore not a replacement for filling out ODMAP, but a helpful for e.g. documenting model settings or references. 
                         If RMMS values have been imported, the corresponding field and entity is indicated in parentheses as Field1($Field2)($Field3)-Entity)"),
               fileInput("upload", "Choose file",  accept = c(".csv")),
               uiOutput("Upload_UI")),
        column(width = 2)
      )
    ))
  )