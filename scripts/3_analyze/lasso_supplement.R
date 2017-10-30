library(glmnet)
library(censReg)
library(caret)

# script to run the lasso routine for each site/parameter combo 
# to add to the supplement
# the only difference between this and the regular LASSO routine
# is that this find the correlated vars by site

source('scripts/2_process/fxn_imputeQRILC.R')
#source('scripts/3_analyze/caret_glmnet.R')
source('scripts/3_analyze/holdout_cv_glmnet_supplement.R')
# get data
all.dat <- read.csv('cached_data/filteredWQ_DOC_ABS_allsites.csv', stringsAsFactors = FALSE)
all.dat$rDOC <- NA
responses <- c('COD', 'BOD', 'DOC', 'Propylene_glycol', 'Acetate', 'X4.Methyl.1H.Benzotriazole', 'X5.Methyl.1H.benzotriazole')
responses.clean <- c('COD', 'BOD', 'DOC', 'Propylene glycol', 'Acetate', '4-Methyl-1H-benzotriazole', '5-Methyl-1H-benzotriazole')
site_ids <- c('CG', 'OUT', 'OAK', 'LK')

# remove variables that are highly correlated
predictors.temp <- all.dat[,c(3:100)]
#predictors.cor <- cor(predictors.temp)
#drop.predictors <- findCorrelation(predictors.cor, cutoff = 0.99, verbose = FALSE, exact = TRUE)
#predictors.keep <- names(predictors.temp)[-drop.predictors]
#predictors <- predictors.keep
predictors <- names(predictors.temp)

# get list of ouput
out <- list()

for (i in 1:length(site_ids)) {
  temp.dat <- all.dat[grep(site_ids[i], all.dat$ProjectID), ]

  
  #df <- all.dat
  #left.censor.val = 0.5
  #right.censor.val = 2
  
  for (j in 1:length(responses)) {
    response <- responses[j]
    # find out if variable is censored
    remarks <- paste('r', response, sep = '')
    remarks <- gsub('rX', 'r', remarks)
    
    rows.left <- which(temp.dat[,remarks]=="<")
    #rows.right <- which(df[,remarks]=='>')
    
    # impute censored values by assuming a log-normal distribution
    # first, get red of real missing values, and set 
    # censored values to NA
    
    y <- temp.dat[, response]
    na.vals <- which(is.na(y))
    y[rows.left] <- NA
    
    if(length(na.vals)>0){
      y <- y[-na.vals]
    }
    y <- log(y)
    
    # impute values from distribution
    
    if (length(rows.left)>0) {
      y.imp <- impute.QRILC(as.data.frame(y))[[1]]
      y <- as.numeric(y.imp[,1])
      
    }else{
      y <- as.numeric(y)
    }
    
    
    df <- temp.dat
    
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
    #save.process <- preProcess(df[,-1], method = c('center', 'scale'))
    #df[,-1] <- scale(df[,-1])
    
    # create matrix for predictor vars
    matIVs = as.matrix(df[,-1])
    colnames(matIVs) <- names(df)[-1]
    
    # write a csv of matIVs + y for later use if needed
    temp <- cbind(y, matIVs)
    names(temp)[1] <- response
    temp.name <- paste('cached_data/', response, '_model_dat.csv', sep = '')
    
    #write.csv(temp, temp.name, row.names = FALSE)
    # run model
    out[[1+length(out)]] <- run.holdout.supp(predictors = predictors, response = response, df = df)
    names(out)[length(out)] <- paste0(site_ids[i], "_", response)
    
    
  }
}

saveRDS(out, 'cached_data/model_out_supplement.rds')



