library(caret)

# run glmnet with caret
run.caret.glmnet <- function(matIVs, y) {

# run all 10 within caret
#tolerance2 <- tolerance(x, metric, tol = 5, maximize = FALSE)
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

mod2 <- cv.glmnet(matIVs, y)
