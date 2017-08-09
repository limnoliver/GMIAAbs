# visualize variability in model results
# when randomly withholding 20% of data

library(Hmisc)
source('scripts/3_analyze/test_holdout.R')

run_holdout()

png(paste('figures/holdout_test_', response, '.png', sep = ''), width = 3000, height = 1500)
par(mfrow=c(2,5), mar = c(2,2,0,0), oma = c(1,1,0,0), cex = 2.5)

for (i in 1:10){
  plot(hold.obs~hold.pred, dat = hold.predict[[i]], main ='', ylim = c(1,5), xlim = c(1,5))
  abline(0,1,col = 'red')
  obs.pred <- lm(hold.obs~hold.pred, dat = hold.predict[[i]])
  legend("bottomright", paste('R2 = ', round(summary(obs.pred)$r.squared, 2), sep = ''), col = 'red') 
}

dev.off()

png(paste('figures/train_test_', response, '.png', sep = ''), width = 3000, height = 1500)
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

png(paste('figures/holdout_model_performance_', response, '.png', sep = ''), height = 500)
par(mar = c(5,5,1,1), oma = c(0,4,0,0))
dotchart2(r2.df[,1], labels = row.names(r2.df), dotsize = 2, xlim = c(0.5, 0.8), xlab = 'R2', bty = "L", width.factor = 0.2,
          col = rgb(100,100,100,alpha = 100, maxColorValue = 255))
for (i in 2:10){
  dotchart2(r2.df[,i], dotsize = 2, add = TRUE, col = rgb(100,100,100,alpha = 100, maxColorValue = 255))
}

dev.off()

library(Hmisc)
png(paste('figures/holdout_coef_effect_', response, '.png', sep = ''), height = 500)
par(mar = c(5,5,1,1), oma = c(0,4,0,0))
dotchart2(coefs$coef[2:15], labels = coefs$var.names[2:15], dotsize = 2, xlim = c(-.2,0.4), 
          xlab = "Coefficients", bty= "L",width.factor = .2, col = rgb(100,100,100,alpha = 100, maxColorValue = 255))
for (i in 3:11) {
  dotchart2(coefs[2:15,i], dotsize = 2, add = TRUE, col = rgb(100,100,100,alpha = 100, maxColorValue = 255))
}
dev.off()

png(paste('figures/holdout_varimp_effect_', response, '.png', sep = ''), height = 500)
par(mar = c(5,5,1,1), oma = c(0,4,0,0))
dotchart2(varimps$imp, labels = coefs$var.names[2:15], dotsize = 2, xlim = c(-1,100), 
          xlab = "Variable Importance", bty= "L",width.factor = .2, col = rgb(100,100,100,alpha = 100, maxColorValue = 255))
for (i in 3:11) {
  dotchart2(varimps[,i], dotsize = 2, add = TRUE, col = rgb(100,100,100,alpha = 100, maxColorValue = 255))
}
dev.off()



