source("profile_plot.R")
load("test.Rdata")
load("post_qc.Rdata")
library(affycoretools)


data_in <- list()
data_in[[1]] <- obatch
data_in[[2]] <- eset.spike
data_in[[3]] <- as.matrix(eset.spike.qc[,1:35])

treatment_in <- list()
treatment_in[[1]] <- treatment
treatment_in[[2]] <- treatment
treatment_in[[3]] <- treatment_2

data_names <- c('Raw Data', 'Normalised Data - Pre QC', 'Normalised Data - Post QC')