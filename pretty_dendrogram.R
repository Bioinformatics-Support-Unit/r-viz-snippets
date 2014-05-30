require(ggdendro)

# some example data
data(golub)

# cluster the samples (method choice may vary)
d <- dist(t(golub), method='euclidian')
h <- hclust(d, method='complete')

# base graphics plot
plot(h)

# now use ggdendro
dd <- dendro_data(h)
labs <- label(dd)
# need to colour labels by sample group - sample data provides classes
# would usually take this from phenoData
labs$group = golub.cl[labs$label]

# build the plot
ggdendrogram(h, leaf_labels=F, labels=F, rotate=T) +
  geom_text(data=labs, aes(label=label, x=x, y=y, colour=as.factor(group)), hjust=1.1, size=3) +
  scale_x_discrete(labels = c())
