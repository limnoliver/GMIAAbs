library(caret)

# run glmnet with caret
run.caret.glmnet <- function(matIVs, y) {

# run all 10 within caret

ctrl <- trainControl(method = 'repeatedcv',
                     number = 5,
                     repeats = 10, 
                     selectionFunction = "oneSE")

mod <- train(matIVs, y,
            method="glmnet",
            trControl = ctrl,
            tuneLength = 10,
            preProcess = c('center', 'scale'))

return(mod)

}

final.varimp <- varImp(mod)$importance[,1]

# get coefficients from final model
coefs.final <- coef(mod$finalModel, cM.all$bestTune$lambda)

preds <- predict(cM.all)
resids <- residuals(cM.all)
plot(y~preds)
abline(0,1,col = "red")
abline(lm(y~preds), col = "blue")
