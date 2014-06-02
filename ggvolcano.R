require(ggplot2)

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

# sample usage
gg_volcano(topTable(fit2, coef=1, number=Inf), 1.5, 0.01)
