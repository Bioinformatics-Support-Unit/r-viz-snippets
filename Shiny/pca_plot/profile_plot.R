##'----------------------------------------------------------------------------------------------------#
##'  Author      : BSU - Dr. Simon Cockell                                                             |
##'  Version     : 1.3                                                                                 |
##'  Language    : R Statistical Programming Language                                                  |
##'  Developers  : Dr. Simon Cockell                                                                   |
##'                Andrew Skelton                                                                      |
##'  Description : An R function that takes an expression set and optionally a character               |
##'                vector for custom x-axis labels. Produces a "GeneSpring" style profile plot from    |
##'                microarray data.                                                                    |
##'  Parameters  : expression_matrix | type: expressionset | Required | Def: NA                        |
##'                treatments        | type: chr vector    | Optional | Def: colnames of expressionset |
##'                sep               | type: logical       | Optional | Def: FALSE                     |
##'----------------------------------------------------------------------------------------------------#

##'Test Inputs
# expression_matrix <- exprs(eset.spike[,1:36])
# treatments <- treatment

# sudo su -   -c "R -e \"source('http://bioconductor.org/biocLite.R'); biocLite('lumi')\""
# setwd("~/Documents/Bioinformatics/Web/Shiny/app1/") 

require(ggplot2)
require(reshape2)

##' This function plots a "GeneSpring" style profile plot from microarray data
plot_profile <- function(expression_matrix, treatments=colnames(expression_matrix), sep=FALSE) {

    ##' Rearrange the data matrix as a data frame, and add a column for grouping
    for_profile <- as.data.frame(expression_matrix)
    for_profile$probe <- row.names(for_profile)
    ##' melt the data into the long form (1 row per observation)
    melted <- melt(for_profile, id='probe')
    ##'This provides the slot for 'melted' to determine how to split the facets
    melted$facet_split = rep(treatments, each=nrow(for_profile))
    ##' order by the grouping factor (probe name)
    melted <- melted[with(melted, order(probe)), ]
    ##' Add an observation to colour by (as with GeneSpring plots, this is expression
    ##' in the left-most array)
    melted$value_in_first = rep(melted[melted$probe==melted$probe & melted$variable==colnames(expression_matrix)[1],]$value,
                                each=ncol(expression_matrix))
    ##' construct the plot in ggplot2, grouping by probe, and colouring by expression
    ##' in the first sample
    g <- ggplot(data=melted, aes(x=as.factor(variable), y=value)) +
                      #theme(axis.text.x = element_blank()) +
                      theme_bw() +
                      theme(axis.text.x  = element_text(angle=90, vjust=0.5, size=10)) +
                      geom_point(aes(group=probe, colour=value_in_first)) +
                      geom_line(aes(group=probe, colour=value_in_first)) +
                      
                      #
                      scale_colour_gradient2(low='blue', high='red', mid='yellow',
                                             midpoint=(max(melted$value_in_first) + min(melted$value_in_first))/2)

    if(sep == TRUE){
      g <- g + 
           facet_grid(. ~ facet_split, space="free", scales="free_x") +
           scale_x_discrete(breaks=levels(factor(melted$variable)))
    } else {
      g <- g +
           scale_x_discrete(breaks=levels(factor(melted$variable)) , labels=treatments)
    }
#     print(g)
      return(g)
}

##' generate some random, not microarray like data for example
#em <- matrix(rnorm(50*10), ncol=10)
#colnames(em) <- letters[1:10]
#plot_profile(em)
