# data prep for models
# extracts response
# interpolates censored values

data.ready <- function(dat, response, preds) {

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
  
  y <- all.dat[, response]
  na.vals <- which(is.na(y))
  y[rows.left] <- NA
  y <- y[-na.vals]
  y <- log(y)
  
  # impute values from distribution
  
  if (length(rows.left)>0) {
    y.imp <- impute.QRILC(as.data.frame(y))[[1]]
    y <- as.numeric(y.imp[,1])
    
  }else{
    y <- as.numeric(y)
  }
  }