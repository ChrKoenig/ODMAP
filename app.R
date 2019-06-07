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
    title = "ODMAP",
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
               p("This application helps to implement the ODMAP approach and produces well formatted protocols that can be ...")
        ),
        column(width = 2)
      )
    )),
    
    tabPanel("Create a protocol", value = "tab_2", sidebarLayout(
      sidebarPanel(
        width = 2,
        materialSwitch("toggle_optional", label = "Hide optional fields", status = "danger"),
        radioButtons("document_format", label = "Download protocol", choices = c("docx", "csv")),
        downloadButton("protocol_download")
      ),
      
      mainPanel(
        tabsetPanel(
          id = "tabset",
          tabPanel("General information", value = "General", fluidPage(
            em(p("Provide some general information about your SDM study.", style = "padding-top: 10px; font-weight: 300")),
            h5("Study title", style = "font-weight: bold"),
            textInput("title", label = NULL),
            h5("Author(s)", style = "font-weight: bold"),
            textInput("authors", label = NULL),
            h5("Email adress of corresponding author", style = "font-weight: bold"),
            textInput("email", label = NULL),
            h5("Study objective", style = "font-weight: bold"),
            selectInput("study_objective", label = NULL, selected = NULL, multiple = F, choices = list("", "Inference and explanation", "Prediction and mapping", "Projection and transfer"))
          )),
          
          tabPanel("1. Overview", value = "Overview",  fluidPage(
            em(p("Give a brief overview of all important parts of your study. This part may go to the method section of your manuscript.", 
                 style = "padding-top: 10px; font-weight: 300")),
            uiOutput("Overview")
          )),
          
          tabPanel("2. Data", value = "Data", fluidPage(
            em(p("Describe your your data in detail.", style = "padding-top: 10px; font-weight: 300")),
            uiOutput("Data")
          )),
          
          tabPanel("3. Model", value = "Model", fluidPage(
            em(p("Describe your modeling approach in detail.", style = "padding-top: 10px; font-weight: 300")),
            uiOutput("Model")
          )),
          
          tabPanel("4. Assessment", value = "Assessment", fluidPage(
            em(p("Describe how you assessed your model results.", style = "padding-top: 10px; font-weight: 300")),
            uiOutput("Assessment")
          )),
          
          tabPanel("5. Prediction", value = "Prediction", fluidPage(
            em(p("Describe your model predictions in detail.", style = "padding-top: 10px; font-weight: 300")),
            uiOutput("Prediction")
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
    cat("<h2>", section, "</h2>", "\n")
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
      cat("<h4>", subsection, "</h4>", "\n")
    } else { # if not, render header only when user provided optional inputs
      all_empty = T
      for(id in paragraph_ids){
        if(input[[id]] != ""){
          all_empty = F
          break
        }
      }
      if(!all_empty){
        cat("<h4>", subsection, "</h4>", "\n")
      }
    }
  }
  knit_element_text = function(elem_name){
    cat(input[[elem_name]], "\n")
  }
  
  knit_element_empty = function(elem_name){
    if(!(elem_name %in% elem_hide[[input$study_objective]] | elem_name %in% elem_optional)){
      placeholder = elem_input$paragraph[which(elem_input$paragraph_id == elem_name)]
      cat("<span style='color:salmon'>", "[", placeholder,"]", "</span>", "\n", sep = "")
    }
  }
  
  # Render Output
  output$markdown = renderUI({
    includeMarkdown(knitr::knit("protocol.Rmd", quiet = T))
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
  output$Overview = render_section("Overview", elem_input)
  output$Data = render_section("Data", elem_input)
  output$Model = render_section("Model", elem_input)
  output$Assessment = render_section("Assessment", elem_input)
  output$Prediction = render_section("Prediction", elem_input)
  
  # Add tab contents to output object before rendering
  for(tab in c("Overview", "Data", "Model", "Assessment", "Prediction")){
    outputOptions(output, tab, suspendWhenHidden = FALSE)
  } 
  
  #################
  ### Downloads ###
  #################
  output$protocol_download = downloadHandler(
    filename = function(){
      paste0("ODMAP_protocol_", Sys.Date(), ".csv")
    },
    content = function(file){
      odmap_csv = elem_input %>% 
        filter(!paragraph_id %in% elem_hide[[input$study_objective]]) %>% # use only relevent rows
        select(section, subsection, paragraph, paragraph_id)
      
      odmap_csv$desciption = sapply(odmap_csv$paragraph_id, function(x){input[[x]]})
      odmap_csv$paragraph_id = NULL
      write.csv(odmap_csv, file)
    }
  )
}

# RUN APP 
shinyApp(ui, server)