# this script removes correlated variables from
# remove correlated variables from predictors
all.dat <- read.csv('cached_data/filteredWQ_DOC_ABS.csv', stringsAsFactors = FALSE)

predictors.all <- all.dat[,c(3:105)]
predictors.cor <- cor(predictors.all)
drop.predictors <- findCorrelation(predictors.cor, cutoff = 0.99, verbose = FALSE, exact = TRUE)

predictors.keep <- names(predictors.all)[-drop.predictors]
