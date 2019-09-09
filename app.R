library(shiny)
library(shinyjs)
library(shinyWidgets)
library(shinythemes)
library(tidyverse)
library(plyr)

#                                                                                                        #
# ------------------------------------------------------------------------------------------------------ #
#                                             DEFINE FRONT-END                                           #
ui <- tagList(
  tags$head(tags$style(
    HTML('.row {padding-top: 65px}'), # prevent overlap between navbar and content
    HTML('.shiny-input-container:not(.shiny-input-container-inline) {width: 100%;}') # fix bug in textAreaInput
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
               img(src = "workflow.jpg", width = "400", align = "center"),
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
        radioButtons("document_format", label = "Download protocol", choices = c("doc", "csv")),
        downloadButton("protocol_download")
      ),
      
      mainPanel(
        tabsetPanel(
          id = "tabset",
          tabPanel("0. General information", value = "General", fluidPage(
            p("---------------------------"),
            strong(p("How to create an ODMAP protocol:")),
            p("Enter all relevant information into the fields provided in steps 0-5. The switch on the left allows you to hide optional fields and show only the mandatory fields. These will differ according to the model objective, which you can choose below."),
            p("For viewing your progress, please go to the Protocol Viewer (see tabs above)."),
            p("You can always save your progress by clicking the download button on the left. After downloading your protocol, it is safe to close the Shiny app. You will be able to resume working on your protocol by choosing the Upload tab above and uploading your previously saved ODMAP protocol (.csv-files only)."),
            p("---------------------------"),
            em(p("Provide some general information about your SDM study.", style = "padding-top: 10px; font-weight: 300")),
            h5("Study title", style = "font-weight: bold"),
            textInput("title", label = NULL),
            h5("Author(s)", style = "font-weight: bold"),
            textInput("authors", label = NULL),
            h5("Email adress of corresponding author", style = "font-weight: bold"),
            textInput("email", label = NULL),
            h5("Model objective", style = "font-weight: bold"),
            selectInput("study_objective", label = NULL, selected = NULL, multiple = F, choices = list("", "Inference and explanation", "Mapping and interpolation", "Forecast and transfer"))
          )),
          
          tabPanel("1. Overview", value = "Overview",  fluidPage(
            em(p("Give a brief overview of all important parts of your study.", 
                 style = "padding-top: 10px; font-weight: 300")),
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

#                                                                                                        #
# ------------------------------------------------------------------------------------------------------ #
#                                              DEFINE SERVER                                             #

server <- function(input, output, session) {
  ####################
  ### Prepare data ###
  ####################
  elem_input = read_csv("www/elements_input.csv", skip_empty_rows = T, 
                        col_types = c(section = col_character(), section_id = col_character(), subsection = col_character(), subsection_id = col_character(),  
                                      paragraph = col_character(),  paragraph_id = col_character(), paragraph_placeholder = col_character(),  
                                      optional = col_double(),  inference = col_double(),  prediction = col_double(), projection = col_double()))
  
  elem_hide = list("Inference and explanation" = c(pull(elem_input %>% filter(inference == 0), paragraph_id), # unused paragraphs
                                                   unique(pull(elem_input %>% group_by(subsection_id) %>% filter(all(inference  == 0)), subsection_id)), # unused subsections
                                                   "p"),
                   "Prediction and mapping" =  c(pull(elem_input %>% filter(prediction == 0), paragraph_id), 
                                                 unique(pull(elem_input %>% group_by(subsection_id) %>% filter(all(prediction == 0)), subsection_id))),
                   "Projection and transfer" =  c(pull(elem_input %>% filter(projection == 0), paragraph_id),
                                                  unique(pull(elem_input %>% group_by(subsection_id) %>% filter(all(projection  == 0)), subsection_id))))
  
  elem_optional = c(pull(elem_input %>% filter(optional == 1), paragraph_id), # optional paragraphs
                    unique(pull(elem_input %>% group_by(subsection_id) %>% filter(all(optional == 1)), subsection_id))) # optional subsections)
  
  elem_hidden = "" # temporary variables to keep track of hidden elements
  
  
  ######################
  ### EVENT HANDLERS ###
  ######################
  # Study objective
  observeEvent(input$study_objective, {
    # Dynamically show/hide corresponding input fields
    shinyjs::show(selector = paste0("#", setdiff(elem_hidden, elem_hide[[input$study_objective]])))
    shinyjs::hide(selector = paste0("#", elem_hide[[input$study_objective]]))
    elem_hidden <<- elem_hide[[input$study_objective]]
    
    # Show/hide Prediction tab when study objective is inference
    if(input$study_objective == "Inference and explanation"){
      hideTab("tabset", "Prediction")
    } else {
      showTab("tabset", "Prediction")
    }
  })
  
  # Toggle optional fields
  observeEvent(input$toggle_optional,{
    shinyjs::toggle(selector = paste0("#", setdiff(elem_optional, elem_hide[[input$study_objective]])), condition = !input$toggle_optional)
  })
  
  
  #######################
  ### MARKDOWN OUTPUT ###
  #######################
  # Functions for dynamically knitting text elements
  knit_section= function(section_id){
    section = unique(elem_input$section[which(elem_input$section_id == section_id)])
    cat("\n\n##", section, "\n")
  }
  
  knit_subsection= function(subsection_id){
    # Get all paragraphs
    paragraph_ids = elem_input$paragraph_id[which(elem_input$subsection_id == subsection_id)]
    subsection = unique(elem_input$subsection[which(elem_input$subsection_id == subsection_id)])
    
    # Find out whether subsection needs to be rendered at all
    # Are all paragraphs optional?
    all_optional = all((paragraph_ids %in% elem_hide[[input$study_objective]] | paragraph_ids %in% elem_optional))
    
    # If not, render header
    if(!all_optional){
      cat("\n\n####", subsection, "\n")
    } else { # if not, render header only when user provided optional inputs
      all_empty = T
      for(id in paragraph_ids){
        if(input[[id]] != ""){
          all_empty = F
          break
        }
      }
      if(!all_empty){
        cat("\n\n####", subsection, "\n")
      }
    }
  }
  
  knit_element_text = function(elem_name){
    cat(input[[elem_name]], "  ")
  }
  
  knit_element_empty = function(elem_name){
    if(!(elem_name %in% elem_hide[[input$study_objective]] | elem_name %in% elem_optional)){
      placeholder = elem_input$paragraph[which(elem_input$paragraph_id == elem_name)]
      cat("\\<", placeholder, "\\>  ", sep = "")
    }
  }
  
  # Render Output
  output$markdown = renderUI({
    includeMarkdown(knitr::knit("protocol_preview.Rmd", quiet = T))
  })
  
  ####################
  ### SHINY OUTPUT ###
  ####################
  # Function for rendering Shiny output
  render_section = function(section, elem_input){
    elem_tmp = elem_input[elem_input$section == section,] 
    section_rendered = renderUI({
      UI_list = vector("list", nrow(elem_tmp)) 
      subsection = ""
      for(i in 1:nrow(elem_tmp)){
        if(subsection != elem_tmp$subsection_id[i]){
          subsection = elem_tmp$subsection_id[i]
          subsection_label = unique(elem_tmp$subsection[which(elem_tmp$subsection_id == subsection)])
          UI_list[[i]] = list(
            div(id = elem_tmp$subsection_id[i], h5(subsection_label, style = "font-weight: bold")),
            textAreaInput(inputId = elem_tmp$paragraph_id[i], placeholder = elem_tmp$paragraph_placeholder[i], label = NULL, resize = "vertical")
          )
        } else {
          UI_list[[i]] = list(
            textAreaInput(inputId = elem_tmp$paragraph_id[i], placeholder = elem_tmp$paragraph_placeholder[i], label = NULL, resize = "vertical")
          )
        }
      }
      return(UI_list)
    })
    return(section_rendered)
  }
  
  # Render Output
  output$Overview_UI = render_section("Overview", elem_input)
  output$Data_UI = render_section("Data", elem_input)
  output$Model_UI = render_section("Model", elem_input)
  output$Assessment_UI = render_section("Assessment", elem_input)
  output$Prediction_UI = render_section("Prediction", elem_input)
  
  # Add tab contents to output object before rendering
  for(tab in c("Overview_UI", "Data_UI", "Model_UI", "Assessment_UI", "Prediction_UI")){
    outputOptions(output, tab, suspendWhenHidden = FALSE)
  } 
  
  ################
  ### Download ###
  ################
  output$protocol_download = downloadHandler(
    filename = function(){
      paste0("ODMAP_protocol_", Sys.Date(), ".", input$document_format)
    },
    content = function(file){
      elem_output = elem_input %>% 
        filter(!paragraph_id %in% elem_hide[[input$study_objective]]) %>% # use only relevent rows
        select(section, subsection, paragraph, paragraph_id)
      
      if(input$document_format == "csv"){
        elem_output$description = sapply(elem_output$paragraph_id, function(x){input[[x]]})
        elem_output$paragraph_id = NULL
        write.csv(elem_output, file, row.names = F)  
      } else {
        src <- normalizePath("protocol_output.Rmd")
        
        # temporarily switch to the temp dir, in case you do not have write permission to the current working directory
        owd <- setwd(tempdir())
        on.exit(setwd(owd))
        file.copy(src, "protocol_output.Rmd", overwrite = TRUE)
        report_output = rmarkdown::render("protocol_output.Rmd", rmarkdown::word_document(),
                                          params = list(title = input$title,
                                                        authors = input$authors))
        file.rename(report_output, file)
      }
    }
  )
  
  ##############-
  ### Upload ###
  ##############
  output$Upload_UI = renderUI({
    # Basic Upload Dialog
    UI_list = list(
      p("You can upload your previously saved ODMAP protocol (.csv-files only) and resume working in the Shiny app."),
      fileInput("upload", "Choose file",  accept = c(".csv"))
    )
    
    if(!is.null(input$upload)){
      protocol_upload =  read.csv(input$upload$datapath)
      if(all(c("section", "subsection", "paragraph", "description") %in% colnames(protocol_upload)) & nrow(protocol_upload) > 0){
        UI_list[[3]] = p(paste("File:", input$upload$name))
        UI_list[[4]] = radioButtons("replace_values", "Replace existing values?", choices = c("Yes", "No"), selected = "No")
        UI_list[[5]] = actionButton("copy_to_input", "Copy to input form")
      } else {
        UI_list[[3]] = p("Invalid file")
      }
    }
    return(UI_list)
  })
  
  observeEvent(input$copy_to_input, {
    protocol_upload =  read.csv(input$upload$datapath, stringsAsFactors = F) %>% 
      left_join(elem_input, by = c("section", "subsection", "paragraph")) %>% 
      mutate(description = trimws(description)) %>% 
      filter(description != "")
    
    for(i in 1:nrow(protocol_upload)){
      if(!(input[[protocol_upload$paragraph_id[i]]] != "" & input$replace_values == "No")){
              updateTextAreaInput(session, inputId = protocol_upload$paragraph_id[i], value = protocol_upload$description[i])
      }
    }
    updateNavbarPage(session, "navbar", selected = "tab_2")
  })
}

# RUN APP 
shinyApp(ui, server)