# library(shiny)
# 
# shinyUI(fluidPage(
#   titlePanel("Profile Plot"),
# 
#   sidebarLayout(
#     sidebarPanel(
#     width=3,
#     selectInput("selection", "Choose a dataset:", 
#                 choices=c('raw', 'normalised')),
#     hr(),
# #       sliderInput("probes",
# #                   "Number of probes:",
# #                   min = 2,
# #                   max = 3540,
# #                   value = 10)
# #     ),
#     mainPanel(plotOutput("distPlot"))
#   )  
# ))

library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
  # Application title
  titlePanel("Hello Shiny!"),
  
  # Sidebar with a slider input for the number of bins
  sidebarLayout(
    sidebarPanel(
      #uiOutput("select"),
      
#       sliderInput("probes",
#                   "Number of probes:",
#                   min = 1,
#                   max = 50,
#                   value = 30)
      selectInput("selection", "Choose a dataset:", 
                   choices=c('raw', 'normalised')),
      uiOutput("slider")
    ),

    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("distPlot")
    )
  )
))
