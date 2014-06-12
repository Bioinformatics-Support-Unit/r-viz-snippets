library(shiny)
library(shinyIncubator)
library(affycoretools)
source("profile_plot.R")
load("test.Rdata")

shinyServer(function(input, output) {
  
  values <- reactiveValues()
  
  datasetInput <- reactive({
    for(i in 1:length(data_in)) {
      if(input$dataset == data_names[i]) {
        values$df        <- data_in[[i]]
        values$treatment <- treatment_in[[i]]
      }
    }
  })
  
  rangeInput <- reactive({
    datasetInput()
    values$range  <- length(values$df[,1])
  })
  
  output$checkbGroups <- renderUI({
    checkboxGroupInput(inputId = 'foo', label = 'foo', choices = colnames(values$df), selected=colnames(values$df))
#     lapply(1:length(data_in), function(x){
#       do.call(checkboxGroupInput, list(inputId = x, label = x, choices = colnames(data_in[[x]])))
#     })
  })

  plotInput <- reactive({
    datasetInput()
    rangeInput()
#     plotPCA(dataset$value, groups=as.numeric(as.factor(treatment)), groupnames=levels(as.factor(treatment)), addtext=treatment, pcs=c(1,2,3), plot3d=TRUE)

    df <- values$df[, colnames(values$df) %in% input$foo]
    
    tmp <- c()
    for(i in 1:length(colnames(df))) {
      tmp<-c(tmp,grep(colnames(df)[i], colnames(values$df)))
    } 
    tment <- values$treatment[tmp]

    plotPCA(df, groups=as.numeric(as.factor(tment)), 
            groupnames=levels(as.factor(tment)), 
            addtext=(input$foo), pcs=c(1,2,3), plot3d=TRUE)
  })

  output$plot <- renderPlot({
    plotInput()
  })

  
  output$downloadPlot <- downloadHandler(
    filename = function() { paste(input$dataset, '_ProfilePlot.png', sep='') },
    content = function(file) {
      png(file)
      plotInput()
      dev.off()
    }
  )
  
})

# df <- data_in[[2]]
# df2 <- data_in[[2]]
# df2 <- df2[,-c(31:36)]
# treatment <- treatment_in[[2]]
# tmp <- c()
# for(i in 1:length(colnames(df2))) {
#   tmp<-c(tmp,grep(colnames(df2)[i], colnames(df)))
# } 
# tment <- treatment[tmp]
    
