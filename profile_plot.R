require(ggplot2)
require(reshape2)

plot_profile <- function(expression_matrix) {
  # This function plots a "GeneSpring" style profile plot from microarray data
  # Rearrange the data matrix as a data frame, and add a column for grouping
  for_profile <- as.data.frame(expression_matrix)
  for_profile$probe <- row.names(for_profile)
  # melt the data into the long form (1 row per observation)
  melted <- melt(for_profile, id='probe')
  # order by the grouping factor (probe name)
  melted <- melted[with(melted, order(probe)), ]
  # Add an observation to colour by (as with GeneSpring plots, this is expression
  # in the left-most array)
  melted$value_in_first = rep(melted[melted$probe==melted$probe & melted$variable==colnames(expression_matrix)[1],]$value,
                              each=ncol(expression_matrix))

  # construct the plot in ggplot2, grouping by probe, and colouring by expression
  # in the first sample
  g <- ggplot(data=melted, aes(x=as.factor(variable), y=value))

  g +
    geom_point(aes(group=probe, colour=value_in_first)) +
    geom_line(aes(group=probe, colour=value_in_first)) +
    scale_colour_gradient2(low='blue', high='red', mid='yellow', midpoint=(max(melted$value_in_first) + min(melted$value_in_first))/2)
}


# generate some random, not microarray like data for example
em <- matrix(rnorm(50*10), ncol=10)
colnames(em) <- letters[1:10]

plot_profile(em)
