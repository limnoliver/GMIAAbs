# this script removes correlated variables from
# remove correlated variables from predictors
predictors.all <- all.dat[,c(3:105)]
predictors.cor <- cor(predictors)
drop.predictors <- findCorrelation(predictors.cor, cutoff = 0.9, verbose = FALSE, exact = TRUE)

predictors.keep <- names(predictors)[-drop.predictors]
