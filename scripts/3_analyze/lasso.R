library(glmnet)
library(censReg)
library(caret)
# get data
all.dat <- read.csv('cached_data/filteredWQ_DOC_ABS.csv')

df <- all.dat
response <- 'COD'
predictors <- names(all.dat)[c(3:105)]

abs.lasso <- function(df, response, predictors)

# reduce df to just response (first col) and predictors
response.col <- which(names(df) %in% response)
predictor.cols <- which(names(df) %in% predictors)
df <- df[,c(response, predictors)]
df <- df[complete.cases(df),]

# log transform absorbance IVs
abs.ivs <- grep('A\\d+', names(df))
df[,abs.ivs] <- log10(df[,abs.ivs])
  
# log transform response
df[,response] <- log10(df[,response])

# scale predictors
df[,-1] <- scale(df[,-1])

# create matrix for predictor vars
matIVs = as.matrix(df[,-1])
colnames(matIVs) <- names(df)[-1]

# write equation
equation <- paste(response, "~", paste(predictors, collapse = " + "), sep = " ")
  

# find outliers


# run model
g1<-cv.glmnet(matIVs,y,alpha=1,type.measure="mse",family='gaussian',nlambda=200,standardize=TRUE)

}