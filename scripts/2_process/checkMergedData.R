
# find missing COD data (should have the WQ samples)
all.dat <- dat.all
missing <- all.dat[is.na(all.dat$COD),]
# data checks
library(RColorBrewer)
all.dat$colors <- all.dat$date
all.dat$colors <- as.factor(all.dat$colors)

palette(colorRampPalette(c("pink", "red", "purple", "purple4"))(22))
plot(log10(all.dat$COD)~log10(all.dat$DOC), col = levels(as.factor(all.dat$date)), pch = 16)
legend("topleft", legend = levels(as.factor(all.dat$date)), pch = 16, col = levels(as.factor(all.dat$date)))

install.packages('caret')
library(caret)

featurePlot()

# look at WQ data

library(psych)
wq.data <- dat.all[,c(175:204)]
wq.data.use <- dat.all[,c(175, 188,198,204)]
log.wq <- log10(wq.data.use)

png('figures/log_wq_data_check.png', height = 1000, width = 1000)
pairs.panels(log.wq, method = 'pearson', hist.col = "#00AFBB", density = FALSE, 
             ellipses = FALSE, cex = 1.8, cex.cor = 1, col = "red", cex.axis = 2)
dev.off()

png('figures/wq_data_check.png', height = 1000, width = 1000)
pairs.panels(wq.data.use, method = 'pearson', hist.col = "#00AFBB", density = FALSE, 
             ellipses = FALSE, cex = 1.8, cex.cor = 1, col = "red", cex.lab = 1.3)
dev.off()
