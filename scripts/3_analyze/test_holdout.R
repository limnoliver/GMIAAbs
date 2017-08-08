# script to generate a holdout dataset, 
# run cvglmnet with 5 folds, 10 repeats
# repeat 10 times with different training to see variability



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


png('figures/holdout_COD_test.png', width = 3000, height = 1500)
par(mfrow=c(2,5), mar = c(2,2,0,0), oma = c(1,1,0,0), cex = 2.5)

for (i in 1:10){
  plot(hold.obs~hold.pred, dat = hold.predict[[i]], main ='', ylim = c(1,5), xlim = c(1,5))
  abline(0,1,col = 'red')
  obs.pred <- lm(hold.obs~hold.pred, dat = hold.predict[[i]])
  legend("bottomright", paste('R2 = ', round(summary(obs.pred)$r.squared, 2), sep = ''), col = 'red') 
}

dev.off()

png('figures/train_COD_test.png', width = 3000, height = 1500)
par(mfrow=c(2,5), mar = c(2,2,0,0), oma = c(1,1,0,0), cex = 2.5)

for (i in 1:10){
  plot(train.obs~train.pred, dat = train.predict[[i]], main ='', ylim = c(1,5), xlim = c(1,5))
  abline(0,1,col = 'red')
  obs.pred <- lm(train.obs~train.pred, dat = train.predict[[i]])
  legend("bottomright", paste('R2 = ', round(summary(obs.pred)$r.squared, 2), sep = ''), col = 'red') 
}

dev.off()

# calculate model performance for 10 interations
# R2 through cv, R2 of train obs~pred, R2 of test obs~pred
r2.train <- c()
r2.test <- c()

for (i in 1:10){
  mod.train <- lm(train.obs~train.pred, dat = train.predict[[i]])
  mod.test <- lm(hold.obs~hold.pred, dat = hold.predict[[i]])
  r2.train[i] <- round(summary(mod.train)$r.squared, 2)
  r2.test[i] <- round(summary(mod.test)$r.squared, 2)
  
}
r2.df <- data.frame(R2_cv = r2,
                    R2_train = r2.train,
                    R2_test = r2.test)
r2.df <- as.data.frame(t(r2.df))

png('figures/holdout_model_performance.png', height = 500)
par(mar = c(5,5,1,1), oma = c(0,4,0,0))
dotchart2(r2.df[,1], labels = row.names(r2.df), dotsize = 2, xlim = c(0.5, 0.8), xlab = 'R2', bty = "L", width.factor = 0.2,
          col = rgb(100,100,100,alpha = 100, maxColorValue = 255))
for (i in 2:10){
  dotchart2(r2.df[,i], dotsize = 2, add = TRUE, col = rgb(100,100,100,alpha = 100, maxColorValue = 255))
}

dev.off()

library(Hmisc)
png('figures/holdout_coef_effect.png', height = 500)
par(mar = c(5,5,1,1), oma = c(0,4,0,0))
dotchart2(coefs$coef[2:15], labels = coefs$var.names[2:15], dotsize = 2, xlim = c(-.2,0.4), 
          xlab = "Coefficients", bty= "L",width.factor = .2, col = rgb(100,100,100,alpha = 100, maxColorValue = 255))
for (i in 3:11) {
  dotchart2(coefs[2:15,i], dotsize = 2, add = TRUE, col = rgb(100,100,100,alpha = 100, maxColorValue = 255))
}
dev.off()

png('figures/holdout_varimp_effect.png', height = 500)
par(mar = c(5,5,1,1), oma = c(0,4,0,0))
dotchart2(varimps$imp, labels = coefs$var.names[2:15], dotsize = 2, xlim = c(-1,100), 
          xlab = "Variable Importance", bty= "L",width.factor = .2, col = rgb(100,100,100,alpha = 100, maxColorValue = 255))
for (i in 3:11) {
  dotchart2(varimps[,i], dotsize = 2, add = TRUE, col = rgb(100,100,100,alpha = 100, maxColorValue = 255))
}
dev.off()

