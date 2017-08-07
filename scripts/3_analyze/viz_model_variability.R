# visualize variability in model results

library(Hmisc)
library(corrplot)
par(mfrow=c(1,1), cex = 1, oma = c(1,5,2,1))
dotchart2(coefs$coef, labels = coefs$var.names, dotsize = 2, xlim = c(-.07,0.5), 
          xlab = "Variable Importance", bty= "L",width.factor = .2, col = rgb(100,100,100,alpha = 100, maxColorValue = 255))
for (i in 3:10) {
  dotchart2(coefs[,i], dotsize = 2, add = TRUE, col = rgb(100,100,100,alpha = 100, maxColorValue = 255))
}

dotchart2(as.numeric(coefs.final), dotsize = 2, add = TRUE, col = rgb(200,100,100, alpha = 100, maxColorValue = 255))

# visualize correlation between remaining predictors 
# after taking out variables with r > 0.99
preds <- all.dat[,predictors.keep]
predictors.cor <- cor(preds)
png('figures/corplot_predictors.png', height = 1000, width = 1000)
corrplot(predictors.cor, method='color', 
         cl.cex = 2)
dev.off()



