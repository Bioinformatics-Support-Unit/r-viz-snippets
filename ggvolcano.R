# uncomment to check for & install dependencies
# packages = c('ggplot2', 'scales', 'devtools', 'ggvis')
# installed = packages %in% installed.packages()
# install_deps = function(dep, ins) {
#   if (dep=='ggvis' & !ins) devtools::install_github(c("rstudio/rmarkdown", "rstudio/ggvis"), build_vignettes = FALSE)
#   else if (!ins) install.packages(dep)
#   else "installed"
# }
# mapply(install_deps, packages, installed)
require(ggplot2)
require(scales)
require(ggvis)

gg_volcano = function(unfiltered_toptable, afc=2, pval=0.05) {
  # this function takes the unfiltered results of topTable (limma), and plots a
  # ggplot2 Volcano Plot. Optional arguments are an absolute fold change and adj
  # p-Value cutoff.

  # add a pass/fail column, based on the afc and pval cutoffs
  unfiltered_toptable$pass = unfiltered_toptable$adj.P.Val < pval & abs(unfiltered_toptable$logFC) > log2(afc)
  # plot basics - x axis is lfc, y axis is -log10(pVal)
  g = ggplot(unfiltered_toptable, aes(x=logFC, y=-log(adj.P.Val, 10)))
  # add points and delimiter lines, colour according to pass/fail criteria
  g + geom_point(aes(colour=pass), show_guide=F) + scale_colour_manual(values=c(alpha('black', 0.5), 'red')) +
    geom_hline(yintercept=-log(pval, 10), colour="red", linetype=2) +
    geom_vline(xintercept=-log2(afc), colour="red", linetype=2) +
    geom_vline(xintercept=log2(afc), colour="red", linetype=2) +
    theme_bw()
}

ggvis_volcano = function(unfiltered_toptable, afc=2, pval=0.05, tooltip="symbol") {
  # this function takes the unfiltered results of topTable (limma), and plots a
  # ggvis Volcano Plot with tooltips (genenames). 
  # Optional arguments are an absolute fold change, adj
  # p-Value cutoff and name of tooltip column in results
  # add a pass/fail column, based on the afc and pval cutoffs - 
  # needs ot be numeric for colouring
  pass = as.numeric(unfiltered_toptable$adj.P.Val < pval & abs(unfiltered_toptable$logFC) > log2(afc))
  tooltip_column = unfiltered_toptable[,tooltip]
  #remove NAs (otherwise points disappear)
  tooltip_column[is.na(tooltip_column)] = "NA"
  df = data.frame(logFC=unfiltered_toptable$logFC, 
                  adj.P.Val=unfiltered_toptable$adj.P.Val, 
                  pass=pass, 
                  tooltip=tooltip_column, 
                  keys=rownames(unfiltered_toptable))
  # this prevents a bug (plot disappears when tooltipping a particular point) - why?
  head(df, 50)
  # Need data frames for ablines
  h_abline = data.frame(logFC=range(unfiltered_toptable$logFC),adj.P.Val=pval)
  v_abline1 = data.frame(logFC=-log2(afc),adj.P.Val=range(unfiltered_toptable$adj.P.Val))
  v_abline2 = data.frame(logFC=log2(afc),adj.P.Val=range(unfiltered_toptable$adj.P.Val))
  #function for generating tooltips
  tooltip_info = function(x=NULL) {
    if(is.null(x)) return(NULL)
    key = x["keys"][[1]]
    df[df$keys==key,]$tooltip
  }
  #build the plot
  ggvis(data=NULL, x=~logFC, y=~-log10(adj.P.Val)) %>%
    layer_points(size:=30, fill=~factor(pass),
                 opacity:=0.5, key:=~keys, data=df) %>%
    add_tooltip(tooltip_info) %>%
    layer_paths(stroke:='red', data=h_abline) %>%
    layer_paths(stroke:='red', data=v_abline1) %>%
    layer_paths(stroke:='red', data=v_abline2)  
}



# sample usage
gg_volcano(topTable(fit2, coef=1, number=Inf), 1.5, 0.01)
