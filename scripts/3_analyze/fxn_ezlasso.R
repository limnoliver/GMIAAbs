##returns variables from lasso variable selection, use alpha=0 for ridge

ezlasso=function(df,yvar,folds=10,trace=F,alpha=1){
  x<-model.matrix(as.formula(paste(yvar," ~ .")),data=df)
  x=x[,-1] ##remove intercept
  
  glmnet1<-glmnet::cv.glmnet(x=x,y=df[,yvar],type.measure='mse',nfolds=folds,alpha=alpha, standardize = TRUE)
  
  co<-coef(glmnet1,s = "lambda.1se")
  inds<-which(co!=0)
  variables<-row.names(co)[inds]
  variables<-variables[!(variables %in% '(Intercept)')];
  return(c(yvar,variables));
}
