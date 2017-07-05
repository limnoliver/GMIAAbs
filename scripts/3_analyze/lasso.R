library(glmnet)
library(censReg)

# get data
all.dat <- read.csv('cached_data/mergedWQ_DOC_ABS.csv')

# response is name of column
# predictors is column index for predictors in df
# log.response <- T/F should the response be log transformed?
deicer.lasso <- function(df, response, predictors, log.response){

# rearrange vars so all IVs are at the end
  response.index <- grep(response, names(df))
  df <- df[,c(response, predictors)]
  
# log transform response
  

# write equation
  equation <- paste(response, "~", names(df[,c(2:length(df))]), sep = " ")
  

# find outliers

# deal with censored values

# 
}