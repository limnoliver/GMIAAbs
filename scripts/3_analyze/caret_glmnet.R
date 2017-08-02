library(caret)

# run glmnet with caret

ctrl <- trainControl(method = 'cv',
                     number = 5,
                     repeats = 1, 
                     selectionFunction = "oneSE")

tuning.pars <- list()
coefs <- data.frame(var.names = predictors,
                    imp = NA)
for (i in 1:10){
  cM <- train(matIVs, y,
              method="glmnet",
              trControl = ctrl,
              tuneLength = 10,
              preProcess = c('center', 'scale')
  )
  tuning.pars[[i]] <- cM$bestTune
  varimp <- varImp(cM)
  coefs[,i+1] <- varimp$importance[,1]
}



# run all 10 within caret

ctrl <- trainControl(method = 'repeatedcv',
                     number = 5,
                     repeats = 10, 
                     selectionFunction = "oneSE")

cM.all <- train(matIVs, y,
            method="glmnet",
            trControl = ctrl,
            tuneLength = 10,
            preProcess = c('center', 'scale'))

final.varimp <- varImp(cM.all)

# get coefficients from final model
coefs.final <- coef(cM.all$finalModel, cM.all$bestTune$lambda)

plot(df$COD~test, xlab = 'Predicted', ylab = 'Observed')
abline(0,1,col = 'red')
plot(resid(cM))
