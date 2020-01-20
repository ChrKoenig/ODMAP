library(shiny)
library(shinyjs)
library(shinyWidgets)
library(shinythemes)
library(DT)
library(tidyverse)
library(rangeModelMetadata)


odmap_dict = read_csv("www/odmap_dict.csv", skip_empty_rows = T, col_types = c(section = col_character(), section_id = col_character(), subsection = col_character(), subsection_id = col_character(),  
                                                                                   paragraph = col_character(), paragraph_id = col_character(), paragraph_placeholder = col_character(),  
                                                                                   optional = col_double(), inference = col_double(), prediction = col_double(), projection = col_double()))
rmm_dict = rangeModelMetadata::rmmDataDictionary()

##########################
### Author information ###
##########################
# Input fields --> character string
paste_author_info = function(author_data){
  if(author_data[1,3] == "" & author_data[1,4] == ""){
    paste0(author_data[1,2], ", ", author_data[1,1], collapse = "")
  } else if(author_data[1,3] == "") {
    paste0(author_data[1,2], ", ", author_data[1,1], " (", author_data[1,4], ")", collapse = "")
  } else if(author_data[1,4] == "") {
    paste0(author_data[1,2], ", ", author_data[1,1], " (", author_data[1,3], ")", collapse = "")
  } else {
    paste0(author_data[1,2], ", ", author_data[1,1], " (", author_data[1,3], ", ", author_data[1,4], ")", collapse = "")
  }
}

# Character string --> input fields
parse_author_info = function(author_text){
  author_info = trimws(str_split(author_text, pattern = ",|\\(|\\)", simplify = T))
  author_info = author_info[author_info != ""]
  if(length(author_info) > 4 | length(author_info) < 2){
    showNotification(paste("Couldn't parse author:", author_text), duration = 5, type = "warning")
    return(NULL)
  } else {
    last_name = author_info[1]
    first_name = author_info[2]
    email = grep(author_info[-c(1:2)], pattern = "@", value = T)
    email = ifelse(length(email) == 0, "", email)
    affiliation = grep(author_info[-c(1:2)], pattern = "@", invert = T, value = T)
    affiliation = ifelse(length(affiliation) == 0, "", affiliation)
    
    new_author = data.frame(first_name, last_name, affiliation, email, stringsAsFactors = F)
    authors$df = rbind(authors$df, new_author)
    authors$text = c(authors$text, paste_author_info(new_author))
  }
}


#######################
### MARKDOWN OUTPUT ###
#######################
# Functions for dynamically knitting text elements
knit_section= function(section_id){
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

knit_element_text = function(elem_name){
  placeholder = odmap_dict$paragraph[which(odmap_dict$paragraph_id == elem_name)]
  cat("\n", placeholder, ": ", input[[elem_name]], "\n", sep="")
}

knit_element_empty = function(elem_name){
  if(!(elem_name %in% elem_hide[[input$o_objective_1]] | elem_name %in% elem_optional)){
    placeholder = odmap_dict$paragraph[which(odmap_dict$paragraph_id == elem_name)]
    cat("\n\n <span style='color:#DC3522'>\\<", placeholder, "\\> </span>\n", sep = "")
  }
}

####################
### SHINY OUTPUT ###
####################
# Function for rendering Shiny output
render_section = function(section, odmap_dict){
  elem_tmp <- odmap_dict[odmap_dict$section == section,] 
  section_rendered <- renderUI({
    UI_list <- vector("list", nrow(elem_tmp)-1) 
    subsection = ""
    for(i in 1:nrow(elem_tmp)){
      if (section=='Overview' & i == 1){
        subsection = elem_tmp$subsection_id[i]
        subsection_label = unique(elem_tmp$subsection[which(elem_tmp$subsection_id == subsection)])
        UI_list[[i]] = list(
          div(id = elem_tmp$subsection_id[i], h5(subsection_label, style = "font-weight: bold")),
          selectInput("o_objective_1", label = NULL, selected = NULL, multiple = F, choices = list("", "Inference and explanation", "Mapping and interpolation", "Forecast and transfer"))
        )
      } else
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