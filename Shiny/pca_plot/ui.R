library(shiny)
library(shinyIncubator)
library(affycoretools)

shinyUI(pageWithSidebar(
  headerPanel('Profile Plot'),
  sidebarPanel(
    
    selectInput("dataset", "Choose a dataset:", 
                  choices = data_names),    
    
    uiOutput("checkbGroups")
  ),

  mainPanel(
    plotOutput('plot')
  )
))