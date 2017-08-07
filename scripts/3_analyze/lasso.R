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

abs.lasso <- function(df, response, predictors, censor.val)
  
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
response.col <- which(names(df) %in% response)
predictor.cols <- which(names(df) %in% predictors)
df <- df[,c(response, predictors)]
df.complete <- complete.cases(df)
df <- df[complete.cases(df),]

# log transform absorbance IVs
abs.ivs <- grep('A\\d+', names(df))
df[,abs.ivs] <- log10(df[,abs.ivs])
  
# log transform response
df[,response] <- log10(df[,response])

# scale predictors
#df[,-1] <- scale(df[,-1])

# create matrix for predictor vars
matIVs = as.matrix(df[,-1])
colnames(matIVs) <- names(df)[-1]

# set response var
y = df[,1]

# run bootstrap of glmnet
mods <- bootstrap.glmnet(n.run = 10)

##########################################################################
# Sort out variables for Stepwise regressions and run stepwise regressions
##########################################################################

testvars.orig <- mods[[2]]
testvars.orig.1se <- mods[[3]]
modelvars.1se <- mods[[4]]
varstring.1se <- mods[[5]]

#Determine which model appears most frequently in LASSO cross validation
unique.1se <- unique(varstring.1se)
num.occur <- numeric()
max.occur.1se <- 0
for (k in 1:length(unique.1se)){
  num.occur[k] <- length(which(varstring.1se==unique.1se[k]))
}

model.1se <- which.max(num.occur)
vars.1se.exists <- !is.na(unique.1se[model.1se])

if(vars.1se.exists){
  vars.1se <- unlist(strsplit(unique.1se[model.1se],fixed=T,split="+"))
  form <- formula(paste(response,"~",unique.1se[model.1se]))
  CR.1se <- censReg(form,left=leftCens,right=rightCens,data=df)
  
  #compute LASSO model with CV identified variables
  form <- formula(paste(response,"~",paste(unique(testvars.orig.1se),collapse="+")))
  X <- scale(as.matrix(df[,unique(testvars.orig.1se)]))
  m.CV <-cv.glmnet(X,y,alpha=1,type.measure="mse",family='gaussian',nlambda=200,nfolds=trunc(nrow(X)/3))
  vars.CVcen <- coef(m.CV)[-1,]
  vars.CVcen <- vars.CVcen[which(abs(vars.CVcen)>0)]
  
  #recalibrate the lasso with censReg
  form <- formula(paste(response,"~",paste(names(vars.CVcen),collapse="+")))
  m.CVcen <- censReg(form,left=leftCens,right=rightCens,data=df)
  
}

plot(mods[[1]][1])
# write equation
equation <- paste(response, "~", paste(predictors, collapse = " + "), sep = " ")
  

# find outliers


# run model
g1<-cv.glmnet(matIVs,y,alpha=1,type.measure="mse",family='gaussian',nlambda=200,standardize=TRUE)

}