# script to scrutinize individual models, 
# look at residuals, etc

# define variables of interest
obs <- df[,response]
residuals <- resid(cM.all)
preds <- predict(cM.all)

top.pred.row <- which.max(as.numeric(final.varimp$importance[,1]))
top.pred <- row.names(final.varimp$importance)[top.pred.row]
top.pred <- df[,top.pred]
# pull out storm month to assess time effect
months <- all.dat[df.complete,'date']
months <- substr(months, start = 5, stop = 6)

# pull out site ID to assess site effect
sites <- all.dat[df.complete, 'ProjectID']
sites <- substr(sites, start=1, stop=2)

# look at obs, pred, residuals by month and by site
plot(residuals, col = as.factor(months), pch = 16)
plot(residuals, col = as.factor(sites), pch = 16)
plot(residuals~preds, col = as.factor(sites), pch = 16)

abline(0,1,col = 'red')
legend('bottomright', legend = levels(as.factor(months)), pch = 16, col = c(1:length(levels(as.factor(months)))))

png('figures/CODvsTopPredictor_bysite.png')
plot(obs ~ top.pred, col = as.factor(sites), pch = 16, ylab = 'log Observed COD', xlab = 'Top Predictor - SAG281_299',
     cex.lab = 1.4, cex.axis = 1.2)
legend('bottomright', legend = levels(as.factor(sites)), pch = 16, col = c(1:length(levels(as.factor(sites)))), cex = 1.4)
dev.off()

png('figures/ObsvsPred_bysite.png')
plot(obs ~ preds, col = as.factor(sites), pch = 16, ylab = 'log Observed COD', xlab = 'log Predicted COD',
     cex.lab = 1.4, cex.axis = 1.2)
abline(0,1,lwd=2,lty=2)
legend('bottomright', legend = levels(as.factor(sites)), pch = 16, col = c(1:length(levels(as.factor(sites)))), cex = 1.4)
dev.off()



