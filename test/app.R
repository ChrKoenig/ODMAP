#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinydashboard)

# Define UI for application that draws a histogram
ui <-  dashboardPage(
    skin = "blue",
    dashboardHeader(title = "Example"),
    dashboardSidebar(
        sidebarMenu(id = "sidebarmenu",
                    menuItem("Tab1", icon = icon("line-chart"),
                             menuSubItem("SubTab1", tabName = "sub1", icon = icon("bar-chart")),
                             menuSubItem("SubTab2", tabName = "sub2", icon = icon("database"))),
                    menuItem("Tab2", tabName = "tab2", icon = icon("users"))
        )
    ),
    dashboardBody(
        tabItems(
            tabItem(tabName = "sub1",
                    tags$div(href="#s2t2",
                             infoBox(value = "Go to table 2 in SubTab2 (not working)",title = "Click me")),
                    tags$div(href="#shiny-tab-tab2", "data-toggle" = "tab",
                             infoBox(value = "Go to Tab2 (this works)",title = "Click me"))
            ),
            tabItem(tabName = "sub2",
                    tableOutput("s2t1"),
                    tableOutput("s2t2")
            ),
            tabItem(tabName = "tab2",
                    tableOutput("t2t1"),
                    tableOutput("t2t2")
            )
        )
    )
)


# Define server logic required to draw a histogram
server <- function(input, output) {
    output$s2t1<- renderTable(mtcars)
    output$s2t2<- renderTable(mtcars)
    output$t2t1<- renderTable(mtcars)
    output$t2t2<- renderTable(mtcars)
}

# Run the application 
shinyApp(ui = ui, server = server)
