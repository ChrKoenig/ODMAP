source("./global.R")

server <- function(input, output, session) {
  ####################
  ### Prepare data ###
  ####################
  elem_hide = list("Inference and explanation" = c(pull(odmap_dict %>% filter(inference == 0), paragraph_id), # unused paragraphs
                                                   unique(pull(odmap_dict %>% group_by(subsection_id) %>% filter(all(inference  == 0)), subsection_id)), # unused subsections
                                                   "p"),
                   "Prediction and mapping" =  c(pull(odmap_dict %>% filter(prediction == 0), paragraph_id), 
                                                 unique(pull(odmap_dict %>% group_by(subsection_id) %>% filter(all(prediction == 0)), subsection_id))),
                   "Projection and transfer" =  c(pull(odmap_dict %>% filter(projection == 0), paragraph_id),
                                                  unique(pull(odmap_dict %>% group_by(subsection_id) %>% filter(all(projection  == 0)), subsection_id))))
  
  elem_optional = c(pull(odmap_dict %>% filter(optional == 1), paragraph_id), # optional paragraphs
                    unique(pull(odmap_dict %>% group_by(subsection_id) %>% filter(all(optional == 1)), subsection_id))) # optional subsections)
  
  elem_hidden = "" # keep track of hidden elements
  
  
  ######################
  ### EVENT HANDLERS ###
  ######################
  # Authors
  # Add dynamic name form
  authors = reactiveValues("df" = data.frame("first_name" = character(0), 
                                             "last_name" = character(0), 
                                             "affiliation" = character(0), 
                                             "email" = character(0), stringsAsFactors = F),
                           "text" =  character(0))
  
  output$authors_table = DT::renderDT(server=FALSE, {
    if(nrow(authors$df) == 0){
      authors_table_render = datatable(authors$df, escape = F, rownames = F, colnames = NULL, 
                                       options = list(dom = "t", ordering = F, language = list(emptyTable = "Author list is empty"), columnDefs = list(list(className = 'dt-left', targets = "_all"))))
    } else {
      authors_table = authors$df %>% 
        rownames_to_column("row_id") %>% 
        mutate(row_id = as.numeric(row_id),
               delete = sapply(1:nrow(.), function(row_id){as.character(actionButton(inputId = paste("remove_author", row_id, sep = "_"),
                                                                                     label = NULL, 
                                                                                     icon = icon("trash"),
                                                                                     onclick = 'Shiny.setInputValue(\"removePressed\", this.id, {priority: "event"})'))}),
               edit = sapply(1:nrow(.), function(row_id){as.character(actionButton(inputId = paste("edit_author", row_id, sep = "_"),
                                                                                   label = NULL, 
                                                                                   icon = icon("edit"),
                                                                                   onclick = 'Shiny.setInputValue(\"editPressed\", this.id, {priority: "event"})'))})) %>% 
        select(row_id, everything())
      
      authors_table_render = datatable(authors_table, escape = F, rownames = F, colnames = c("", "First name", "Last name", "Affiliation", "email", "", ""), 
                                       extensions = 'RowReorder', selection = 'none',
                                       list(dom = "t", autoWidth = TRUE, order = list(list(0, 'asc')), rowReorder = TRUE, 
                                            columnDefs = list(list(width = '30px', targets = c(0,5,6)), 
                                                              list(className = 'dt-center', targets = "_all"),
                                                              list(orderable = F, targets = c(1:6)))))
    }
    authors_table_render
  })
  
  observeEvent(input$add_author, {
    showModal(
      modalDialog(title = "Add new author", footer = NULL, easyClose = T,
                  textInput("first_name", "First name"),
                  textInput("last_name", "Last name"),
                  textInput("affiliation", "Affiliation"),
                  textInput("email", "Email address"),
                  actionButton("save_new_author", "Save")
      )
    )
  })
  
  observeEvent(input$save_new_author, {
    if(input$first_name == "" | input$last_name == ""){
      showNotification("Please provide first and last name", duration = 3, type = "message")
    } else {
      new_author = data.frame(input$first_name, input$last_name, input$affiliation, input$email, stringsAsFactors = F)
      authors$df = rbind(authors$df, new_author)
      authors$text = c(authors$text, paste_author_info(new_author))
      removeModal()
    }
  })
  
  observeEvent(input$removePressed, {
    item_remove = as.integer(parse_number(input$removePressed))
    authors$df = authors$df[-item_remove,]
    authors$text = authors$text[-item_remove]
  })
  
  observeEvent(input$editPressed, {
    item_edit = as.integer(parse_number(input$editPressed))
    author_edit = isolate(authors$df)[item_edit,]
    showModal(
      modalDialog(title = "Edit author information", footer = NULL, easyClose = T,
                  textInput("first_name", "First name", value = author_edit[1]),
                  textInput("last_name", "Last name", value = author_edit[2]),
                  textInput("affiliation", "Affiliation", value = author_edit[3]),
                  textInput("email", "Email address", value = author_edit[4]),
                  actionButton("save_author_edits", "Save")
      )
    )
  })
  
  observeEvent(input$save_author_edits, {
    if(input$first_name == "" | input$last_name == ""){
      showNotification("Please provide first and last name", duration = 3, type = "message")
    } else {
      item_edit = as.integer(parse_number(input$editPressed))
      authors$df[item_edit,] = c(input$first_name, input$last_name, input$affiliation, input$email)
      authors$text[item_edit] = paste_author_info(data.frame(input$first_name, input$last_name, input$affiliation, input$email, stringsAsFactors = F))
      removeModal()
    }
  })
  
  # ---------------------- End Authors -------------------------#
  # Study objective
  observeEvent(input$o_objective_1, {
    # Dynamically show/hide corresponding input fields
    shinyjs::show(selector = paste0("#", setdiff(elem_hidden, elem_hide[[input$o_objective_1]])))
    shinyjs::hide(selector = paste0("#", elem_hide[[input$o_objective_1]]))
    elem_hidden <<- elem_hide[[input$o_objective_1]]
    
    # Show/hide Prediction tab when study objective is inference
    if(input$o_objective_1 == "Inference and explanation"){
      hideTab("tabset", "Prediction")
    } else {
      showTab("tabset", "Prediction")
    }
  })
  
  # Toggle optional fields
  observeEvent(input$toggle_optional,{
    if(is.null(input$o_objective_1)){
      return(NULL)
    } else if(input$toggle_optional == T & input$o_objective_1 == ""){
      showNotification("Please select a model objective under '1. Overview'", duration = 5, type = "message")
      Sys.sleep(0.3)
      updateMaterialSwitch(session, "toggle_optional", value = F)
      updateTabsetPanel(session, "tabset", "Overview")
    } else {
      shinyjs::toggle(selector = paste0("#", setdiff(elem_optional, elem_hide[[input$o_objective_1]])), condition = !input$toggle_optional) 
    }
  })
  
  # Render Markdown
  # TODO - source global.R in knitr outputs
  output$markdown = renderUI({
    includeMarkdown(knitr::knit("protocol_preview.Rmd", quiet = T))
  })
  
  # Render Shiny output
  output$Overview_UI = render_section("Overview", odmap_dict)
  output$Data_UI = render_section("Data", odmap_dict)
  output$Model_UI = render_section("Model", odmap_dict)
  output$Assessment_UI = render_section("Assessment", odmap_dict)
  output$Prediction_UI = render_section("Prediction", odmap_dict)
  
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
      elem_output = odmap_dict %>% 
        filter(!paragraph_id %in% elem_hide[[input$o_objective_1]]) %>% # use only relevent rows
        select(section, subsection, paragraph, paragraph_id)
      
      # CREATE TXT FILES
      if(input$document_format == "txt"){
        # Create header
        header = c("--------------- ODMAP PROTOCOL ---------------", 
                   paste0("Study title: ", input$study_title, collapse = ""),
                   paste0("Authors: ", paste(authors$text, collapse = "; "), collapse = ""),
                   paste0("DOI: ", input$DOI, collapse = ""),
                   paste0("Date: ", as.character(Sys.Date()), collapse = ""),
                   "----------------------------------------------")
        
        # Create table
        elem_output$description = sapply(elem_output$paragraph_id, function(x){input[[x]]})
        elem_output$paragraph_id = NULL
        
        # Write output
        file_conn = file(file, open = "w")
        write_lines(header, file_conn)
        write_delim(elem_output, path = file_conn, delim = ",")
        close(file_conn)
        
        # CREATE WORD FILES
      } else {
        src <- normalizePath("protocol_output.Rmd")
        
        # temporarily switch to the temp dir, in case you do not have write permission to the current working directory
        owd <- setwd(tempdir())
        on.exit(setwd(owd))
        file.copy(src, "protocol_output.Rmd", overwrite = TRUE)
        report_output = rmarkdown::render("protocol_output.Rmd", rmarkdown::word_document(),
                                          params = list(study_title = input$study_title, authors =  paste(authors$text, collapse = ", "), DOI = input$DOI))
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
      p("You can upload your previously saved ODMAP protocol (.txt-files only) and resume working in the Shiny app."),
      fileInput("upload", "Choose file",  accept = c(".txt"))
    )
    if(!is.null(input$upload)){
      protocol_header = read_delim(input$upload$datapath, skip = 1, n_max = 4, delim=':', col_names = F, col_types = cols())
      header_valid = all(protocol_header[,1] == c("Study title", "Authors", "DOI", "Date"))
      protocol_data = read_csv(input$upload$datapath, skip = 6, col_types = cols())
      data_valid = all(c("section", "subsection", "paragraph", "description") %in% colnames(protocol_data)) & nrow(protocol_data) > 0
      if(header_valid & data_valid){
        UI_list[[3]] = p(paste("File:", input$upload$name))
        UI_list[[4]] = radioButtons("replace_values", "Overwrite non-empty fields with uploaded values?", choices = c("Yes", "No"), selected = "No")
        UI_list[[5]] = actionButton("copy_to_input", "Copy to input form")
      } else {
        UI_list[[3]] = p("Invalid file")
      }
    }
    return(UI_list)
  })
  
  observeEvent(input$copy_to_input, {
    # Fill in general information
    protocol_header = read_delim(input$upload$datapath, skip = 1, n_max=3, delim=':', col_names = F, col_types = cols())
    authors_upload = protocol_header[which(protocol_header[,1] == 'Authors'), 2]
    authors_upload = trimws(str_split(authors_upload, ";", simplify = T))
    if(!all(authors_upload == "")){
      if(input$replace_values == "Yes"){
        authors$df = authors$df[0,]
        authors$text = character(0)
      }
      for(author in authors_upload){
        parse_author_info(author)
      }
    }
    if(!(input$study_title != "" & input$replace_values == "No")){
      if(!is.na(as.character(protocol_header[which(protocol_header[,1] == 'Study title'),2]))) {
        updateTextInput(session, inputId = "study_title", value = as.character(protocol_header[which(protocol_header[,1] == 'Study title'), 2]))
      }
    }
    if(!(input$DOI != "" & input$replace_values == "No")){
      if(!is.na(as.character(protocol_header[which(protocol_header[,1]=='DOI'),2]))) {
        updateTextInput(session, inputId = "DOI", value = as.character(protocol_header[which(protocol_header[,1] == 'DOI'), 2]))
      }
    }
    
    # TODO - upload rmm objects and populate odmap protocol
    
    # Fill in ODMAP information
    protocol_data = read_csv(input$upload$datapath, skip = 6, col_types = cols()) %>% 
      left_join(odmap_dict, by = c("section", "subsection", "paragraph")) %>% 
      mutate(description = trimws(description)) %>% 
      filter(description != "")
    
    for(i in 1:nrow(protocol_data)){
      if(!(input[[protocol_data$paragraph_id[i]]] != "" & input$replace_values == "No")){
        if(protocol_data$paragraph_id[i] == "o_objective_1"){
          updateSelectInput(session, inputId = protocol_data$paragraph_id[i], label = protocol_data$description[i])
        } else {
           updateTextAreaInput(session, inputId = protocol_data$paragraph_id[i], value = protocol_data$description[i])
        }
      }
    }
    
    # Switch to "Create a protocol" 
    updateNavbarPage(session, "navbar", selected = "tab_2")
    reset("upload")
  })
}