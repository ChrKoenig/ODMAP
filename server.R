source("./global.R")

server <- function(input, output, session) {
  # ------------------------------------------------------------------------------------------#
  #                            Define reactive values and prepare data                        # 
  # ------------------------------------------------------------------------------------------#
  
  # TODO: Remove line
  output$input_values = renderText(isolate(paste(names(reactiveValuesToList(input)), collapse = ",")))
  
  # container for authors
  authors = reactiveValues(df = data.frame("first_name" = character(0),  "last_name" = character(0))) 
  
  # Container for model setting dataframes and tabset
  model_settings = reactiveValues(suggestions = rmm_dict %>% filter(field1 == "model" & field2 == "algorithm") %>% pull(field3) %>% unique() %>% trimws(),
                                  settings_tabset = NULL)
  
  # Monitor current progress
  get_progress = reactive({
    progress = c()
    for(sect in unique(odmap_dict$section)){
      all_elements = odmap_dict %>% 
        filter(section == sect & !paragraph_id %in% unlist(elem_hidden) & !paragraph_type %in% c("model_setting", "author")) %>% 
        filter(if(input$hide_optional) !paragraph_id %in% elem_optional else T) %>% 
        mutate(paragraph_id = ifelse(paragraph_type == "extent", paste0(paragraph_id, "_xmin"), paragraph_id)) %>%  
        pull(paragraph_id)
      if(length(all_elements) == 0){
        next 
      } else {
        completed_elements = sum(sapply(all_elements, function(x){!(identical(input[[x]], "") | identical(input[[x]], NULL))}, USE.NAMES = T, simplify = T))
        progress[sect] = (sum(completed_elements) / length(all_elements)) * 100
      }
    }
    return(progress)
  }) 
  
  output$progress_bars = renderUI({
    progress = get_progress()
    progress_UI_list = lapply(names(progress), function(sect){
      progressBar(paste("progress", sect, sep = "_"), value = progress[sect], title = sect)
    })
    return(progress_UI_list)
  })
  
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
  
  
  # ------------------------------------------------------------------------------------------#
  #                           Rendering functions for UI elements                             # 
  # ------------------------------------------------------------------------------------------#
  render_text = function(paragraph_id, paragraph_placeholder){
    textAreaInput(inputId = paragraph_id, placeholder = paragraph_placeholder, label = NULL, resize = "vertical")
  }
  
  render_authors = function(){
    div(
      DTOutput("authors_table", width = "100%"),
      actionButton("add_author", label = NULL, icon = icon("plus")),
      br(), br()
    )
  }
  
  render_objective = function(paragraph_id, paragraph_placeholder){
    selectizeInput(inputId = paragraph_id, label = NULL, multiple = F, options = list(create = T, placeholder = paragraph_placeholder),
                   choices = list("", "Inference and explanation", "Mapping and interpolation", "Forecast and transfer"))
  }
  
  render_suggestion = function(paragraph_id, paragraph_placeholder, suggestions){
    suggestions = sort(trimws(unlist(strsplit(suggestions, ","))))
    selectizeInput(inputId = paragraph_id, label = NULL, choices = suggestions, multiple = TRUE, options = list(create = T, placeholder = paragraph_placeholder))
  }
  
  render_extent = function(paragraph_id){
    splitLayout(
      cellWidths = c("110px",  "110px", "110px", "110px", "110px"),
      p("Spatial extent", style = "padding: 9px 12px"),
      textAreaInput(inputId = paste0(paragraph_id, "_xmin"), placeholder = "xmin", label = NULL, resize = "none"),
      textAreaInput(inputId = paste0(paragraph_id, "_xmax"), placeholder = "xmax", label = NULL, resize = "none"),
      textAreaInput(inputId = paste0(paragraph_id, "_ymin"), placeholder = "ymin", label = NULL, resize = "none"),
      textAreaInput(inputId = paste0(paragraph_id, "_ymax"), placeholder = "ymax", label = NULL, resize = "none")
    )
  }
  
  render_model_algorithm = function(paragraph_id, paragraph_placeholder){
    selectizeInput(inputId = paragraph_id, label = NULL, choices = model_settings$suggestions, multiple = TRUE, options = list(create = T,  placeholder = paragraph_placeholder))
  }
  
  render_model_settings = function(){
    div(
      tabsetPanel(id = "settings_tabset"),
      actionButton("add_setting", label = NULL, icon = icon("plus")),
      br(),br()
    )
  }
  
  render_section = function(section, odmap_dict){
    section_dict = filter(odmap_dict, section == !!section) 
    section_rendered = renderUI({
      section_UI_list = vector("list", nrow(section_dict)) # holds UI elements for all ODMAP paragraphs belonging to 'section'
      subsection = ""
      for(i in 1:nrow(section_dict)){
        paragraph_UI_list = vector("list", 3) # holds UI elements for current paragraph 
        
        # First element: Header 
        if(subsection != section_dict$subsection_id[i]){
          subsection = section_dict$subsection_id[i]
          subsection_label = section_dict$subsection[i]
          paragraph_UI_list[[1]] = div(id = section_dict$subsection_id[i], h5(subsection_label, style = "font-weight: bold"))
        }
        
        # Second element: Input field(s) 
        paragraph_UI_list[[2]] = switch(section_dict$paragraph_type[i],
                                        text = render_text(section_dict$paragraph_id[i], section_dict$paragraph_placeholder[i]),
                                        author = render_authors(),
                                        objective = render_objective(section_dict$paragraph_id[i], section_dict$paragraph_placeholder[i]),
                                        suggestion = render_suggestion(section_dict$paragraph_id[i], section_dict$paragraph_placeholder[i], section_dict$suggestions[i]),
                                        extent = render_extent(section_dict$paragraph_id[i]),
                                        model_algorithm = render_model_algorithm(section_dict$paragraph_id[i], section_dict$paragraph_placeholder[i]),
                                        model_setting = render_model_settings())
        
        # Third element: Next/previous button
        if(i == nrow(section_dict)){
          # TODO add next/previous buttons
        }
        
        # Reduce list to non-empty elements
        paragraph_UI_list = Filter(Negate(is.null), paragraph_UI_list)
        section_UI_list[[i]] = paragraph_UI_list
      }
      return(section_UI_list)
    })
    return(section_rendered)
  }
  
  # ------------------------------------------------------------------------------------------#
  #                            Rendering functions for Markdown Output                        # 
  # ------------------------------------------------------------------------------------------#
  # Functions for dynamically knitting text elements
  knit_section = function(section_id){
    section = unique(odmap_dict$section[which(odmap_dict$section_id == section_id)])
    cat("\n\n##", section, "\n")
  }
  
  knit_subsection= function(subsection_id){
    # Get all paragraphs
    paragraph_ids = odmap_dict$paragraph_id[which(odmap_dict$subsection_id == subsection_id)]
    subsection = unique(odmap_dict$subsection[which(odmap_dict$subsection_id == subsection_id)])
    
    # Find out whether subsection needs to be rendered at all
    # Are all paragraphs optional?
    all_optional = all((paragraph_ids %in% elem_hide[[input$o_objective_1]] | paragraph_ids %in% elem_optional))
    
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
  
  knit_text = function(element_id){
    if(input[[element_id]] == ""){
      knit_missing(element_id)
    } else {
      element_name = odmap_dict$paragraph[which(odmap_dict$paragraph_id == element_id)]
      cat("\n", element_name, ": ", input[[element_id]], "\n", sep="")
    }
  }
  
  knit_extent = function(element_id){
    if(any(c(input[[paste0(element_id, "_xmin")]], input[[paste0(element_id, "_xmax")]], input[[paste0(element_id, "_ymin")]], input[[paste0(element_id, "_ymax")]]) == "")){
      knit_missing(element_id)
    } else {
      element_name = odmap_dict$paragraph[which(odmap_dict$paragraph_id == element_id)]
      element_value = paste(c(input[[paste0(element_id, "_xmin")]], input[[paste0(element_id, "_xmax")]], 
                              input[[paste0(element_id, "_ymin")]], input[[paste0(element_id, "_ymax")]]), collapse = ", ")
      cat("\n", element_name, " (xmin, xmax, ymin, ymax): ", element_value, "\n", sep="")
    }
  }
  
  knit_suggestion = function(element_id){
    if(is.null(input[[element_id]])){
      knit_missing(element_id)
    } else {
      element_name = odmap_dict$paragraph[which(odmap_dict$paragraph_id == element_id)]
      cat("\n", element_name, ": ", paste(input[[element_id]], collapse = ", "), "\n", sep="")
    }
  }
  
  knit_model_settings = function(element_id){
    if(is.null(input[["o_algorithms_1"]])){
      knit_missing(element_id)
    } else {
      for(alg in input[["o_algorithms_1"]]){
        settings_tab = model_settings[[alg]] %>% filter(value != "")
        if(nrow(settings_tab) == 0) {
          cat("\n\n <span style='color:#DC3522'>\\<", alg, "\\> </span>\n", sep = "")
        } else {
          settings_char = paste0(settings_tab$setting, " (", settings_tab$value, ")", collapse = ", ")
          cat("\n", alg, ": ", settings_char, "\n", sep="")
        }
      }
    }
  }
  
  knit_missing = function(element_id){
    if(!(element_id %in% elem_hide[[input$o_objective_1]] | element_id %in% elem_optional)){
      placeholder = odmap_dict$paragraph[which(odmap_dict$paragraph_id == element_id)]
      cat("\n\n <span style='color:#DC3522'>\\<", placeholder, "\\> </span>\n", sep = "")
    }
  }
  
  # ------------------------------------------------------------------------------------------#
  #                                   Import functions                                        # 
  # ------------------------------------------------------------------------------------------#
  
  # TODO write odmap import functions
  import_odmap_to_text = function(paragraph_id, values){}
  import_odmap_to_authors = function(paragraph_id, values){}
  import_odmap_to_suggestion = function(paragraph_id, values){}
  import_odmap_to_extent = function(paragraph_id, values){}
  import_odmap_to_algorithm = function(paragraph_id, values){}
  
  import_rmm_to_text = function(paragraph_id, values){
    updateTextAreaInput(session = session, inputId = paragraph_id, 
                        value = paste(paste0(values, " (", names(values), ")"), collapse = ",\n"))
  }
  
  import_rmm_to_authors = function(paragraph_id, values){
    tryCatch(expr = {
    names_split = unlist(strsplit(values, split = " and "))
    names_split = strsplit(names_split, split = ", ")
    authors$df = authors$df[0,] # Delete previous entries
    for(i in 1:length(names_split)){
      author_tmp = names_split[[i]]
      authors$df = rbind(authors$df, data.frame("first_name" = author_tmp[2],  "last_name" = author_tmp[1]))
    }}, error = function(e){
      showNotification("Could not import author list", duration = 3, type = "error")
    })
  }
  
  import_rmm_to_suggestion = function(paragraph_id, values){
    values = unlist(strsplit(values, split = "; "))
    suggestions = unlist(strsplit(odmap_dict$suggestions[odmap_dict$paragraph_id == paragraph_id], ","))
    suggestions_new =  sort(trimws(c(suggestions, as.character(values))))
    updateSelectizeInput(session = session, inputId = paste0(paragraph_id), choices = suggestions_new, selected = as.character(values))
  }
  
  import_rmm_to_extent = function(paragraph_id, values){
    values = unlist(strsplit(values, "; "))
    for(i in 1:length(values)){
      values_split = unlist(strsplit(values[i], ": "))
      updateTextAreaInput(session = session, inputId = paste0(paragraph_id, "_", values_split[1]), value = paste(values_split[2]))
    }
  }
  
  import_rmm_to_algorithm = function(imported_values){
    
  }
  
  # ------------------------------------------------------------------------------------------#
  #                                     Export functions                                      # 
  # ------------------------------------------------------------------------------------------#
  export_text = function(paragraph_id){
    return(input[[paragraph_id]])
  }
  
  export_authors = function(paragraph_id){
    paste(authors$df$first_name, authors$df$last_name, collapse = "; ")
  }
  
  export_suggestion = function(paragraph_id){
    return(paste(input[[paragraph_id]], collapse = "; "))
  }
  export_extent = function(paragraph_id){
    return(input[[paragraph_id]])
  }
  export_model_algorithm = function(paragraph_id){}
  
  # ------------------------------------------------------------------------------------------#
  #                                      Event handlers                                       # 
  # ------------------------------------------------------------------------------------------#
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
  observeEvent(input$hide_optional,{
    if(is.null(input$o_objective_1)){
      return(NULL)
    } else if(input$hide_optional == T & input$o_objective_1 == ""){
      showNotification("Please select a model objective under '1. Overview'", duration = 3, type = "message")
      Sys.sleep(0.3)
      updateMaterialSwitch(session, "hide_optional", value = F)
      updateTabsetPanel(session, "tabset", "Overview")
      # TODO jump to Input field
    } else {
      shinyjs::toggle(selector = paste0("#", setdiff(elem_optional, elem_hide[[input$o_objective_1]])), condition = !input$hide_optional) 
    }
  })
  
  # Model algorithms and settings
  observeEvent(input$o_algorithms_1, {
    if (length(input$o_algorithms_1) > length(model_settings$settings_tabset)) { # New algorithm selected
      new_alg = setdiff(input$o_algorithms_1, model_settings$settings_tabset)
      
      # Create dataframe for model_alg
      if(new_alg %in% filter(rmm_dict, field2 == "algorithm")$field3){
        model_settings[[new_alg]] = rmm_dict %>% 
          filter(field1 == "model" & field2 == "algorithm" & field3 == new_alg) %>%
          mutate(setting = entity, value = NA) %>% 
          select(setting, value)
      } else {
        model_settings[[new_alg]] = data.frame(setting = character(0), value = character(0))
      }
      
      # Add new dataframe to output and settings_tabset
      output[[new_alg]] = renderDT(model_settings[[new_alg]], editable = T, rownames = F, options = list(dom = "t", pageLength = 50))
      appendTab(inputId = "settings_tabset", select = TRUE, tabPanel(title = new_alg, value = new_alg, DTOutput(outputId = new_alg)))
      observeEvent(input[[paste0(new_alg, '_cell_edit')]], {
        model_settings[[new_alg]][input[[paste0(new_alg, '_cell_edit')]]$row, input[[paste0(new_alg, '_cell_edit')]]$col + 1] = input[[paste0(new_alg, '_cell_edit')]]$value
      })
      
      model_settings$settings_tabset = input$o_algorithms_1 
    } else {
      hide_alg = setdiff(model_settings$settings_tabset, input$o_algorithms_1)
      removeTab(inputId = "settings_tabset", target = hide_alg)
      model_settings$settings_tabset = input$o_algorithms_1
    }
    
  }, ignoreNULL = FALSE, ignoreInit = TRUE)
  
  observeEvent(input$add_setting, {
    if(!is.null(input$settings_tabset)){
      empty_row = data.frame(setting = NA, value = NA)
      model_settings[[input$settings_tabset]] = rbind(model_settings[[input$settings_tabset]], empty_row)
    } else {
      showNotification("Please select or add a model algorithm under '1. Overview'", duration = 5, type = "message")
      updateTabsetPanel(session, "tabset", "Overview")
      # TODO jump to Input field
    }
  })
  
  # Authors
  output$authors_table = DT::renderDT({
    if(nrow(authors$df) == 0){
      authors_dt = datatable(authors$df, escape = F, rownames = F, colnames = NULL, 
                             options = list(dom = "t", ordering = F, language = list(emptyTable = "Author list is empty"), columnDefs = list(list(className = 'dt-left', targets = "_all"))))
    } else {
      authors_tmp = authors$df %>% 
        rownames_to_column("row_id") %>% 
        mutate(row_id = as.numeric(row_id),
               delete = sapply(1:nrow(.), function(row_id){as.character(actionButton(inputId = paste("remove_author", row_id, sep = "_"), label = NULL, 
                                                                                     icon = icon("trash"),
                                                                                     onclick = 'Shiny.setInputValue(\"remove_author\", this.id, {priority: "event"})'))})) %>% 
        dplyr::select(-row_id)
      
      authors_dt = datatable(authors_tmp, escape = F, rownames = F, editable = T, colnames = c("First name", "Last name", ""), 
                             options = list(dom = "t", autoWidth = TRUE, 
                                            columnDefs = list(list(width = 'px', targets = c(2)), list(orderable = F, targets = c(0:2)))))
    }
    return(authors_dt)
  })
  
  observeEvent(input$add_author, {
    showModal(
      modalDialog(title = "Add new author", footer = NULL, easyClose = T,
                  textInput("first_name", "First name"),
                  textInput("last_name", "Last name"),
                  actionButton("save_new_author", "Save")
      )
    )
  })
  
  observeEvent(input$save_new_author, {
    if(input$first_name == "" | input$last_name == ""){
      showNotification("Please provide first and last name", duration = 3, type = "message")
    } else {
      new_author = data.frame("first_name" = input$first_name, "last_name" = input$last_name, stringsAsFactors = F)
      authors$df = rbind(authors$df, new_author)
      removeModal()
    }
  })
  
  observeEvent(input$remove_author, {
    item_remove = as.integer(parse_number(input$remove_author))
    authors$df = authors$df[-item_remove,]
    output$authors_df = renderDT(authors$df)
  })
  
  observeEvent(input$authors_table_cell_edit, {
    authors$df[input$authors_table_cell_edit$row, input$authors_table_cell_edit$col + 1] = input$authors_table_cell_edit$value
    output$authors_df = renderDT(authors$df)
  })
  
  # Render Markdown
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
  
  # ------------------------------------------------------------------------------------------#
  #                                         Download                                          # 
  # ------------------------------------------------------------------------------------------#
  output$protocol_download = downloadHandler(
    filename = function(){
      paste0("ODMAP_protocol_", Sys.Date(), ".", input$document_format)
    },
    content = function(file){
      odmap_download = odmap_dict %>% 
        filter(!paragraph_id %in% elem_hide[[input$o_objective_1]]) %>% # use only relevent rows
        dplyr::select(section, subsection, paragraph, paragraph_id, paragraph_type) %>% 
        mutate(Value = NA)
      
      # Create .csv-files
      if(input$document_format == "csv"){
        # Create table
        for(i in 1:nrow(odmap_download)){
          switch(odmap_download$paragraph_type[i],
                 text = export_text(paragraph_id = odmap_download$paragraph_id[i]),
                 author = export_authors(paragraph_id = odmap_download$paragraph_id[i]),
                 suggestion = export_suggestion(paragraph_id = odmap_download$paragraph_id[i]),
                 extent = export_extent(paragraph_id = odmap_download$paragraph_id[i]),
                 model_algorithm = export_model_algorithm(paragraph_id = odmap_download$paragraph_id[i]))
        }

        odmap_download$paragraph_id = NULL
        odmap_download$paragraph_type = NULL
        
        # Write output
        file_conn = file(file, open = "w")
        write.csv(odmap_download, file = file_conn, na = "")
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
  
  # ------------------------------------------------------------------------------------------#
  #                                      Upload and Import                                    # 
  # ------------------------------------------------------------------------------------------#
  output$Upload_UI = renderUI({
    # Basic Upload Dialog
    UI_list = list()
    if(!is.null(input$upload)){
      # Obtain file extension
      file_type = gsub( "(^.*)(\\.[A-z]*$)", replacement = "\\2", input$upload$datapath)
      if(!file_type %in% c(".csv", ".RDS")){
        showNotification("Please select a provide a .csv (ODMAP, RMMS) or .RDS file (RMMS).", duration = 3, type = "error")
        reset("upload")
        return()
      }
      
      # Read in file
      if(file_type == ".RDS"){
        tryCatch({
          protocol_upload = rmmToCSV(protocol_upload, input$upload$datapath)
        }, error = function(e){
          showNotification("Could not read file.", duration = 3, type = "error")
          reset("upload")
          return()
        })
      } else {
        protocol_upload = read.csv(file = input$upload$datapath, header = T, sep = ",", stringsAsFactors = F, na.strings = c("NA", "", "NULL"))
      }
      
      # Identify protocol type
      if(all(c("Section", "Subsection", "Paragraph", "Value") %in% colnames(protocol_upload))){
        protocol_type = reactive("ODMAP")
      } else if(all(c("Field.1", "Field.2", "Field.3", "Entity", "Value") %in% colnames(protocol_upload))){
        protocol_type = "RMMS"
      } else {
        showNotification("Please select a valid ODMAP or RMMS file", duration = 3, type = "error")
        reset("upload")
        return()
      }
      
      UI_list[[1]] = p(paste0("File: ", input$upload$name, " (", protocol_type, " protocol, ", sum(!is.na(protocol_upload$Value)), " non-empty values)"))
      UI_list[[2]] = radioButtons("replace_values", "Overwrite non-empty fields with uploaded values?", choices = c("Yes", "No"), selected = "No")
      UI_list[[3]] = actionButton(paste0(protocol_type, "_to_input"), "Copy to input form")
    }
    return(UI_list)
  })
  
  observeEvent(input$ODMAP_to_input, {
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
  
  observeEvent(input$RMMS_to_input, {
    protocol_upload = read.csv(input$upload$datapath, sep = ",", stringsAsFactors = F, na.strings = c("NA", "", "NULL")) %>% 
      mutate(Value = trimws(Value)) %>% 
      filter(!is.na(Value))
    
    # 1. Prepare imported values
    imported_values = list()
    for(i in 1:nrow(protocol_upload)){
      rmm_fields = c(protocol_upload$Field.1[i], protocol_upload$Field.2[i], protocol_upload$Field.3[i])
      rmm_fields = rmm_fields[!is.na(rmm_fields)]
      rmm_fields = paste0(rmm_fields, collapse = "$")
      rmm_entity = protocol_upload$Entity[i]
      
      odmap_subset = odmap_dict[grepl(rmm_fields, str_split(odmap_dict$rmm_fields, pattern = ","), fixed = T) & grepl(rmm_entity, str_split(odmap_dict$rmm_entities, pattern = ","), fixed = T),]
      if(nrow(odmap_subset) == 1){
        imported_values[[odmap_subset$paragraph_id]][[paste(rmm_fields, rmm_entity, sep = "-")]] = protocol_upload$Value[i]
      }
    }
    
    # 2. Update ODMAP input fields with imported values
    for(i in 1:length(imported_values)){
      switch(odmap_dict$paragraph_type[which(odmap_dict$paragraph_id == names(imported_values)[i])],
             text = import_rmm_to_text(paragraph_id = names(imported_values)[i], values = imported_values[[i]]),
             author = import_rmm_to_authors(paragraph_id = names(imported_values)[i], values = imported_values[[i]]),
             suggestion = import_rmm_to_suggestion(paragraph_id = names(imported_values)[i], values = imported_values[[i]]),
             extent = import_rmm_to_extent(paragraph_id = names(imported_values)[i], values = imported_values[[i]]),
             model_algorithm = import_rmm_to_algorithm())
    }
    
    # Switch to "Create a protocol" 
    updateNavbarPage(session, "navbar", selected = "tab_2")
    reset("upload")
  })
}