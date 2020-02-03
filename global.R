library(shiny)
library(shinyjs)
library(shinyWidgets)
library(shinythemes)
library(shinydashboard)
library(DT)
library(tidyverse)
library(rangeModelMetadata)

odmap_dict = read.csv("www/odmap_dict.csv", header = T, stringsAsFactors = F)
rmm_dict = read.csv("www/rmm_dict.csv", header = T, stringsAsFactors = F) # TODO Change to pull table directly from package

# ------------------------------------------------------------------------------------------#
#                                  Author information                                       # 
# ------------------------------------------------------------------------------------------#
# Character string -> input fields
parse_author_info_text = function(author_text){
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

# rmm fields -> input fields
parse_author_info_rmm = function(author_text){
  # TODO
}

# Input fields --> character string
paste_author_info = function(author_data){
  # TODO (if even needed with simplified author structure)
}
