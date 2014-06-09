library(shiny)

shinyServer(function(input, output) {
  
  source("profile_plot.R")
  load("test.Rdata")
  
  output$slider <- renderUI({
    sliderInput("probes",
                "Number of probes:",
                min = 2,
                max = 1300,
                value = 10)
  })

  output$distPlot <- renderPlot({
    if(input$selection == "raw") {
      plot_data <- as.matrix(obatch[,1:36])
    } else if(input$selection == "normalised") {
      plot_data <- as.matrix(eset.spike[,1:36])
    } 
    limit <<- length(plot_data[,1])
    plot_profile(plot_data[1:input$probes,], treatments = treatment, sep = TRUE)
  })
  
})
