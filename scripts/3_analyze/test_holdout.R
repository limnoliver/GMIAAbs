# script to generate a holdout dataset, 
# run cvglmnet with 5 folds, 10 repeats
# repeat 10 times with different training to see variability


run.holdout <- function(predictors, response, df) {
tuning.pars <- list()
varimps <- data.frame(var.names = predictors, imp = NA)
coefs <- data.frame(var.names = c('intercept', predictors), coef = NA)
r2 <- c()
rmse <- c()
hold.predict <- list()
train.predict <- list()


for (i in 1:10){
out <- sample(1:nrow(df), size = round(nrow(df)*.2, 0), replace = FALSE)
y.train <- y[-out]
matIVs.train <- matIVs[-out,]
y.test <- y[out]
matIVs.test <- matIVs[out,]

mod <- run.caret.glmnet(matIVs.train, y.train)
result.row <- which(mod$results$lambda==mod$bestTune$lambda & mod$results$alpha == mod$bestTune$alpha)

tuning.pars[[i]] <- mod$bestTune
varimp <- varImp(mod)
varimps[,i+1] <- varimp$importance[,1]
coefs[,i+1] <- as.numeric(coef(mod$finalModel, mod$bestTune$lambda))
r2[i] <- mod$results$Rsquared[result.row]
rmse[i] <- mod$results$RMSE[result.row]
test.dat <- as.data.frame(matIVs.test)
test.dat[,response] <- y.test
hold.predict[[i]] <- data.frame(hold.obs = y.test,
                                hold.pred = predict(mod, test.dat))
train.predict[[i]] <- data.frame(train.obs = y.train,
                                 train.pred = predict(mod))

}
return(tuning.pars, varimps, coefs, r2, rmse, hold.predict, train.predict)
}



