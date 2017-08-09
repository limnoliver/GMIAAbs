library(glmnet)
library(censReg)
library(caret)
# get data
all.dat <- read.csv('cached_data/filteredWQ_DOC_ABS.csv', stringsAsFactors = FALSE)

# remove variables that are highly correlated
predictors.all <- all.dat[,c(3:100)]
predictors.cor <- cor(predictors.all)
drop.predictors <- findCorrelation(predictors.cor, cutoff = 0.99, verbose = FALSE, exact = TRUE)
predictors.keep <- names(predictors.all)[-drop.predictors]

df <- all.dat
response <- 'COD'
predictors <- predictors.keep
left.censor.val = 0.5
right.censor.val = 2

abs.lasso <- function(df, response, predictors, left.censor.val = 0.5, right.censor.val = 2)
  
# find out if variable is censored
remarks <- paste('r', response, sep = '')
rows.left <- which(df[,remarks]=="<")
rows.right <- which(df[,remarks]=='>')
censored <- FALSE

if (length(rows.left)>0|length(rows.right)>0) {
  censored <- TRUE
  df[rows.left, response] <- left.censor.val*df[rows.left, response]
  df[rows.right, response] <- right.censor.val*df[rows.right, response]
}

# reduce df to just response (first col) and predictors
storms <- df$ProjectID
response.col <- which(names(df) %in% response)
predictor.cols <- which(names(df) %in% predictors)
df <- df[,c(response, predictors)]
df.complete <- complete.cases(df)
df <- df[complete.cases(df),]
storms <- storms[df.complete]
sites <- substr(storms, 1, 2)
# log transform absorbance IVs
abs.ivs <- grep('A\\d+', names(df))
df[,abs.ivs] <- log10(df[,abs.ivs])
  
# log transform response
df[,response] <- log10(df[,response])

# scale predictors
save.process <- preProcess(df[,-1], method = c('center', 'scale'))
df[,-1] <- scale(df[,-1])

# create matrix for predictor vars
matIVs = as.matrix(df[,-1])
colnames(matIVs) <- names(df)[-1]

# set response var
y = df[,1]

