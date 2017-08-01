###########################################
# Define functions for parallel processing#

bootstrap.glmnet <- function(n.run = 100){

  # set parameters
  testvars.orig <- character()
  testvars.orig.1se <- character()
  varstring.1se <- character()
  modelvars.1se <- list()
  cv.models <- list()
  
  CV.var.summary <- list()
  for (i in 1:n.run){
   
    # run model
    g1 <- cv.glmnet(matIVs,y,alpha=1,type.measure="mse",family='gaussian')
    cv.models[[i]] <- g1
    # extract model output
    c1<-coef(g1, s='lambda.min')
    c1.1se <- coef(g1,s='lambda.1se')
    beta<-which(abs(c1)>0)[-1]-1
    beta.1se <- which(abs(c1.1se)>0)[-1]-1
    testvars.orig <- c(testvars.orig,colnames(matIVs)[beta])
    testvars.orig.1se <- c(testvars.orig.1se,colnames(matIVs)[beta.1se])
    if(length(beta.1se)>0) {modelvars.1se[[i]] <- colnames(matIVs)[beta.1se]
    }else{modelvars.1se[[i]] <- "intercept"}
    varstring.1se[i] <- paste(modelvars.1se[[i]][order(modelvars.1se[[i]])],collapse="+")
    
  }
  return(list(cv.models, testvars.orig, testvars.orig.1se, modelvars.1se,varstring.1se))
}
