library(shiny)
library(shinyjs)
library(shinyWidgets)
library(shinythemes)
library(tidyverse)

# DEFINE UI
ui <- tagList(
  tags$head(tags$style(
    HTML('.row { padding-top: 55px}'), # prevent overlap between navbar and content
    HTML('.shiny-input-container:not(.shiny-input-container-inline) { width: 100%;}') # fix bug in textAreaInput
  )),
  navbarPage(
    id = "navbar",
    title = "ODMAP",
    position = "fixed-top",
    theme = shinytheme("united"),
    selected = "tab_1",
    useShinyjs(),
    
    # HOME TAB
    tabPanel("What is ODMAP?", value = "tab_1", fluidPage(
      mainPanel(
        width = 10,
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
      )
    )),
    
    tabPanel("Create a Protocol", value = "tab_2", sidebarLayout(
      sidebarPanel(
        width = 3,
        textOutput("study_objective"),
        materialSwitch("toggle_optional", "Hide optional fields", status = "danger")
      ),
      
      mainPanel(
        width = 9,
        tabsetPanel(
          id = "tabset",
          tabPanel("General information", value = "General", fluidPage(
            textInput("title", label = "Study title"),
            textInput("authors", label = "Author(s)"),
            textInput("email", label = "Email adress of corresponding author"),
            selectInput("study_objective", label = "Study objective", selected = NULL, multiple = F, choices = list("", "Inference and explanation", "Prediction and mapping", "Projection and transfer"))
          )),
          
          tabPanel("1. Overview", value = "Overview",  fluidPage(
            uiOutput("Overview")
          )),
          
          tabPanel("2. Data", value = "Data", fluidPage(
            uiOutput("Data")
          )),
          
          tabPanel("3. Model", value = "Model", fluidPage(
            uiOutput("Model")
          )),
          
          tabPanel("4. Assessment", value = "Assessment", fluidPage(
            uiOutput("Assessment")
          )),
          
          tabPanel("5. Prediction", value = "Prediction", fluidPage(
            uiOutput("Prediction")
          )) 
        ))
    )),
    
    # PREVIEW PROTOCOL
    tabPanel("Protocol viewer", value = "tab_3", fluidPage(
      fluidRow(
        htmlOutput("markdown")
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
  
  elem_hide = list("Inference and explanation" = c(elem_input$paragraph_id[which(elem_input$inference == 0)], "p"),
                   "Prediction and mapping" = elem_input$paragraph_id[which(elem_input$prediction == 0)],
                   "Projection and transfer" = elem_input$paragraph_id[which(elem_input$projection == 0)])
  
  elem_optional = elem_input$paragraph_id[which(elem_input$optional == 1)]
  
  elem_hidden = elem_input$paragraph_id
  ######################
  ### EVENT HANDLERS ###
  ######################
  # Study objective
  observeEvent(input$study_objective, {
    # write current study objective to output (for display in sidebarPanel)
    output$study_ibjective = renderText(input$study_objective)
    
    # Dynamically show/hide corresponding input fields
    shinyjs::show(selector = paste0("#", setdiff(elem_hidden, elem_hide[[input$study_objective]])))
    shinyjs::hide(selector = paste0("#", elem_hide[[input$study_objective]]))
    elem_hidden <<- elem_hide[[input$study_objective]]
    
    # Show/hide Prediction tab
    if(input$study_objective == "Inference and explanation"){
      hideTab("tabset", "Prediction")
    } else {
      showTab("tabset", "Prediction")
    }
  })
  
  # Toggle optional fields
  observeEvent(input$toggle_optional,{
    shinyjs::toggle(selector = paste0("#", elem_input$paragraph_id[which(elem_input$optional == 1)]), condition = !input$toggle_optional)
  })
  
  
  #######################
  ### MARKDOWN OUTPUT ###
  #######################
  # Functions for dynamically knitting text elements
  knit_section= function(section_id){
    section = unique(elem_input$section[which(elem_input$section_id == section_id)])
    cat("##", section)
    cat("\n")
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
      cat("####", subsection)
      cat("\n")
    } else { # if not, render header only when user provided optional inputs
      all_empty = T
      for(id in paragraph_ids){
        if(input[[id]] != ""){
          all_empty = F
          break
        }
      }
      if(!all_empty){
        cat("####", subsection)
        cat("\n")
      }
    }
  }
  knit_element_text = function(elem_name){
    cat(input[[elem_name]])
    cat("  ")
    cat("\n")
  }
  
  knit_element_empty = function(elem_name){
    if(!(elem_name %in% elem_hide[[input$study_objective]] | elem_name %in% elem_optional)){
      placeholder = elem_input$paragraph[which(elem_input$paragraph_id == elem_name)]
      cat("<span style='color:salmon'>", "[", placeholder,"]", "</span>", sep  = "")
      cat("\n")
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
          UI_list[[i]] = list(
            h4(elem_tmp$subsection[i]),
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
}

# RUN APP 
shinyApp(ui, server)

