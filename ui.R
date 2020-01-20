source("./global.R")

ui <- tagList(
  tags$head(tags$style(
    HTML('.row {padding-top: 65px}'), # prevent overlap between navbar and content
    HTML('.shiny-input-container:not(.shiny-input-container-inline) {width: 100%;}'), # fix bug in textAreaInput
    HTML('.shiny-notification {position: fixed; top: 30%; left: 0%; right: 84%}') # position of popup notifications
  )),
  
  navbarPage(
    id = "navbar",
    title = "ODMAP v1.0",
    position = "fixed-top",
    theme = shinytheme("cosmo"),
    selected = "tab_1",
    useShinyjs(),
    
    # HOME TAB
    tabPanel("What is ODMAP?", value = "tab_1", fluidPage(
      fluidRow(
        column(width = 2),
        column(width = 8,
               h3("What is ODMAP?"),
               p("Species distribution models (SDMs) constitute the most common class of biodiversity models. The advent of ready-to-use software packages and
             increasing availability of digital geo-information have considerably assisted the application of SDMs in recent years  enabling their use in
             informing conservation and management, and quantifying impacts from global change."),
               p("However, models must be fit for purpose, with all important aspects of their development and applications properly considered. 
             Despite the widespread use of SDMs, the standardisation and documentation of model protocols remains limited. To address these issues, 
             we propose a standard protocol for reporting SDMs. We call this the ODMAP (Overview, Data, Model, Assessment and Prediction) protocol
             as each of its components reflectsthe main steps involved in building SDMs and other empirically-based biodiversity models."), 
               img(src = "workflow.jpg", width = 400, align = "center"),
               p("The ODMAP protocol serves two main purposes. First, it provides a checklist for authors detailing key steps for model building and analyses. 
             Second, it introduces a standard approach todocumentation that ensures transparency and reproducibility, facilitating peer review and 
             expert evaluation of model quality as well as meta-analyses."),
               p("This application helps to implement the ODMAP approach and produces well formatted protocols that can be exported for further usage. For further explanation please refer to the original publication (Zurell et al., under review)."),
               em(p("Please cite as follows:")),
               p("Zurell D,  Franklin J,  Bouchet PJ, Serra-Diaz JM, Dormann CF, Elith J, Fandos Guzman G, Feng X, Guillera-Arroita G, Guisan A, König C, Leitão PJ, Lahoz-Monfort JJ, Park DS, Peterson AT,  Raacciuolo G, Schmatz D, Schröder B, Thuiller W, Yates KL, Zimmermann NE, Merow C (under review) A standard protocol for describing species distribution models.")
        ),
        column(width = 2)
      )
    )),
    
    tabPanel("Create a protocol", value = "tab_2", sidebarLayout(
      sidebarPanel(
        width = 2,
        materialSwitch("toggle_optional", label = "Hide optional fields", status = "danger"),
        radioButtons("document_format", label = "Download protocol", choices = c("doc", "txt")),
        downloadButton("protocol_download")
      ),
      
      mainPanel(
        tabsetPanel(
          id = "tabset",
          tabPanel("0. General information", value = "General", fluidPage(
            strong(p("How to create an ODMAP protocol", style = "padding-top: 10px")),
            p("Enter all relevant information into the fields provided in steps 0-5. The switch on the left allows you to hide optional fields and show only the mandatory fields. These will differ according to the model objective, which you can choose below."),
            p("For viewing your progress, please go to the Protocol Viewer (see tabs above)."),
            p("You can always save your progress by clicking the download button on the left. After downloading your protocol, it is safe to close the Shiny app. You will be able to resume working on your protocol by choosing the Upload tab above and uploading your previously saved ODMAP protocol (.csv-files only)."),
            hr(),
            h5("Study title", style = "font-weight: bold"),
            textInput("study_title", label = NULL),
            h5("DOI", style = "font-weight: bold"),
            textInput("DOI", label = NULL),
            h5("Author(s)", style = "font-weight: bold"),
            dataTableOutput("authors_table"),
            actionButton("add_author", label = NULL, icon = icon("plus"))
          )),
          
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
    
    tabPanel("Upload a Protocol", value = "tab_4", fluidPage(
      fluidRow(
        column(width = 2),
        column(width = 8, uiOutput("Upload_UI")),
        column(width = 2)
      )
    ))
  )
)