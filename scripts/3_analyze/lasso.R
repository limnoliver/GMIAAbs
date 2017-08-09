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
response <- 'Acetate'
predictors <- predictors.keep
left.censor.val = 0.5
right.censor.val = 2

abs.lasso <- function(df, response, predictors)
  
# find out if variable is censored
remarks <- paste('r', response, sep = '')
remarks <- gsub('rX', 'r', remarks)
rows.left <- which(df[,remarks]=="<")
#rows.right <- which(df[,remarks]=='>')

# impute censored values by assuming a log-normal distribution
# first, get red of real missing values, and set 
# censored values to NA

y <- df[, response]
na.vals <- which(is.na(y))
y[rows.left] <- NA
y <- y[-na.vals]
y <- log(y)
y <- as.data.frame(y)

# impute values from distribution

if (length(rows.left)>0) {
  y.imp <- impute.QRILC(as.data.frame(y))[[1]]
  y <- as.numeric(y.imp)

  }else{
    y <- as.numeric(y)
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
  
# scale predictors
save.process <- preProcess(df[,-1], method = c('center', 'scale'))
df[,-1] <- scale(df[,-1])

# create matrix for predictor vars
matIVs = as.matrix(df[,-1])
colnames(matIVs) <- names(df)[-1]

# run model
mod <- run.caret.glmnet(matIVs, y)
}

