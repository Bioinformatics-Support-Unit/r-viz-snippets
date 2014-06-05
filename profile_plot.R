##'----------------------------------------------------------------------------------------------------#
##'  Author      : BSU - Dr. Simon Cockell                                                             |
##'  Language    : R Statistical Programming Language                                                  |
##'  Developers  : Dr. Simon Cockell                                                                   |
##'                Andrew Skelton                                                                      |
##'  Description : An R function that takes an expression set and optionally a character               |
##'                vector for custom x-axis labels. Produces a "GeneSpring" style profile plot from    |
##'                microarray data.                                                                    |
##'  Parameters  : expression_matrix | type: expressionset | Required | Def: NA                        |
##'                x_lab             | type: chr vector    | Optional | Def: colnames of expressionset |
##'                                                                                                    |
##'----------------------------------------------------------------------------------------------------#

require(ggplot2)
require(reshape2)
require(gridExtra)

##' This function plots a "GeneSpring" style profile plot from microarray data
plot_profile <- function(expression_matrix, treatments=colnames(expression_matrix), sep=FALSE) {
  
  if(sep == TRUE) {
    unique_treatments <- unique(treatments)
    treatments_master        <- treatments
    expression_matrix_master <- expression_matrix
    printer <- list()
  } else {
    unique_treatments <- c("All")
  }
  
  for(i in 1:length(unique_treatments)) {
    
    if(sep == TRUE){expression_matrix <- expression_matrix_master[, grep(unique_treatments[i], treatments_master)]}
    treatments <- rep(unique_treatments[i], length(colnames(expression_matrix)))
    
    ##' Rearrange the data matrix as a data frame, and add a column for grouping
    for_profile <- as.data.frame(expression_matrix)
    for_profile$probe <- row.names(for_profile)
    ##' melt the data into the long form (1 row per observation)
    melted <- melt(for_profile, id='probe')
    ##' order by the grouping factor (probe name)
    melted <- melted[with(melted, order(probe)), ]
    ##' Add an observation to colour by (as with GeneSpring plots, this is expression
    ##' in the left-most array)  
    melted$value_in_first = rep(melted[melted$probe==melted$probe & melted$variable==colnames(expression_matrix)[1],]$value,
                                each=ncol(expression_matrix))
  
    ##' construct the plot in ggplot2, grouping by probe, and colouring by expression
    ##' in the first sample
    printer[[i]] <- ggplot(data=melted, aes(x=as.factor(variable), y=value)) +
                      scale_x_discrete(labels=treatments) + 
                      #geom_vline(xintercept=sample_sep, colour="black", linetype = "longdash") + 
                      theme_bw() +
                      geom_point(aes(group=probe, colour=value_in_first)) +
                      geom_line(aes(group=probe, colour=value_in_first)) +
                      theme(legend.position="none") + 
                      scale_colour_gradient2(low='blue', high='red', mid='yellow', 
                                             midpoint=(max(melted$value_in_first) + min(melted$value_in_first))/2)
  }
  
  args.list <- c(printer, list(nrow=1,ncol=length(printer)))
  do.call(grid.arrange, args.list)
}


##' generate some random, not microarray like data for example
em <- matrix(rnorm(50*10), ncol=10)
colnames(em) <- letters[1:10]
plot_profile(em)


par(mfrow = c(3, 1))
#length(unique_treatments)
for(i in 1:length(printer)) {
  print(printer[[i]])  
}
printer <<- printer
