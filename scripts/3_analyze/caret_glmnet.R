library(caret)

# run glmnet with caret
run.caret.glmnet <- function(matIVs, y)
ctrl <- trainControl(method = 'repeatedcv',
                     number = 5,
                     repeats = 10, 
                     selectionFunction = "oneSE")

tuning.pars <- list()
varimps <- data.frame(var.names = predictors,
                    imp = NA)
coefs <- data.frame(var.names = c('intercept', predictors),
                      coef = NA)
for (i in 1:10){
  cM <- train(matIVs, y,
              method="glmnet",
              trControl = ctrl,
              tuneLength = 10,
              preProcess = c('center', 'scale')
  )
  tuning.pars[[i]] <- cM$bestTune
  varimp <- varImp(cM)
  varimps[,i+1] <- varimp$importance[,1]
  coefs[,i+1] <- as.numeric(coef(cM$finalModel, cM$bestTune$lambda))
}



# run all 10 within caret

ctrl <- trainControl(method = 'repeatedcv',
                     number = 5,
                     repeats = 20, 
                     selectionFunction = "oneSE")

cM.all <- train(matIVs, y,
            method="glmnet",
            trControl = ctrl,
            tuneLength = 10,
            preProcess = c('center', 'scale'))

final.varimp <- varImp(cM.all)$importance[,1]

# get coefficients from final model
coefs.final <- coef(cM.all$finalModel, cM.all$bestTune$lambda)

plot(df$COD~test, xlab = 'Predicted', ylab = 'Observed')
abline(0,1,col = 'red')
plot(resid(cM))

# use RFE to do variable selection
ctrl <- rfeControl(functions = "glmnet",
                    method = 'repeatedcv',
                     number = 5,
                     repeats = 10)

profile <- rfe(matIVs, y, refControl = ctrl)
