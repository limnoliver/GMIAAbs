# do recursive feature selection within caret & glmnet
# is this redundant?

####################################
# RFE parameters
####################################
library(ipred)
library(e1071)

#Custom Functions
glmnetFuncs <- caretFuncs #Default caret functions

glmnetFuncs$fit <- function (x, y, first, last, ...) { #Fits a GLMNET model
  library("glmnet")
  glmnet(as.matrix(x), y, family = "gaussian", alpha = 0, lambda = 0.02)
}

glmnetFuncs$pred <- function (object, x) { #Makes predictions (in a format other cart functions recognize) from a glmnet model
  tmp <- predict(object, newx=as.matrix(x))
  tmp <- data.frame(tmp)
  names(tmp) <- sub('.s0','',names(tmp))
  tmp$pred <- ifelse(tmp[,1]>tmp[,2],names(tmp)[1],names(tmp)[2])
  tmp
}

glmnetFuncs$rank <- function (object, x, y) { #Ranks predictions, and numbers them from best to worst
  vimp <- sort(object$beta[[2]][, 1])
  vimp <- as.data.frame(vimp)
  vimp$var <- row.names(vimp)
  vimp$'Overall' <- seq(nrow(vimp),1)
  vimp
}


MyRFEcontrol <- rfeControl(
  functions = glmnetFuncs,
  method = "repeatedCV",
  number = 5,
  repeats = 10,
  rerank = FALSE,
  returnResamp = "final",
  saveDetails = FALSE,
  verbose = TRUE)

MyTrainControl=trainControl(
  method = "repeatedCV",
  number=5,
  repeats=10,
  returnResamp = "all",
  summaryFunction=twoClassSummary
)

####################################
# Training parameters
####################################
MyTrainControl=trainControl(
  method = "repeatedcv",
  number=10,
  repeats=5,
  returnResamp = "all",
)

####################################
# Select Features-GLMNET
####################################

x <- matIVs
y <- y

RFE <- rfe(x,y,
           metric = "Rsquared",maximize=TRUE,rfeControl = MyRFEcontrol,
           method='glmnet',
           tuneGrid = 10,
           trControl = MyTrainControl)

NewVars <- RFE$optVariables
RFE
plot(RFE)

FL <- as.formula(paste("Target ~ ", paste(NewVars, collapse= "+"))) #RFE
