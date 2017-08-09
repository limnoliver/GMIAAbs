# script to do some basic visualizations of 
# response and predictor variables
library(caret)
library(corrplot)
library()

all.dat <- read.csv('cached_data/filteredWQ_DOC_ABS.csv', stringsAsFactors = FALSE)

#############################################################
# visualize correlation between predictors after highly correlated 
# variables are removed
predictors.all <- all.dat[,c(3:100)]
predictors.cor <- cor(predictors.all)
drop.predictors <- findCorrelation(predictors.cor, cutoff = 0.99, verbose = FALSE, exact = TRUE)
predictors.keep <- names(predictors.all)[-drop.predictors]

predictors <- predictors.all[,predictors.keep]

predictors.cor <- cor(predictors)

png('figures/corplot_predictors.png', height = 1000, width = 1000)
corrplot(predictors.cor, method='color', 
         cl.cex = 2)
dev.off()

#############################################################
# visualize the relationship/correlation between response variables
# and the distribution of those responses

library(psych)
responses <- all.dat[,c('COD', 'BOD', 'DOC', 'Propylene_glycol', 'Acetate', 'Sodium', 'X4.Methyl.1H.Benzotriazole', 'X5.Methyl.1H.benzotriazole', 
                        'Ethylene_glycol', 'Formate')]
responses <- log10(responses)
names(responses) <- c(names(responses)[1:3], 'PG', 'Acetate', 'Na', '4-meth', '5-meth', 'EG', 'Formate')

png('figures/log_responses_pairwise_check.png', height = 1000, width = 1000)
pairs.panels(responses, method = 'pearson', hist.col = "#00AFBB", density = FALSE, 
             ellipses = FALSE, cex = 1.8, cex.cor = 1, col = "red", cex.axis = 2)
dev.off()


##############################################################

