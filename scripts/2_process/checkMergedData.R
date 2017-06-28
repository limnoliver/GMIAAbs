
# find missing COD data (should have the WQ samples)
dat.all <- all.dat
missing <- all.dat[is.na(all.dat$COD),]
missing.DOC <- all.dat[is.na(all.dat$DOC), ]

# data checks
library(RColorBrewer)
all.dat$colors <- all.dat$date
all.dat$colors <- as.factor(all.dat$colors)

palette(colorRampPalette(c("pink", "red", "purple", "purple4"))(22))
plot(log10(all.dat$COD)~log10(all.dat$DOC), col = levels(as.factor(all.dat$date)), pch = 16)
legend("topleft", legend = levels(as.factor(all.dat$date)), pch = 16, col = levels(as.factor(all.dat$date)))

library(caret)

featurePlot()

# look at WQ data

library(psych)
wq.data <- dat.all[,c(175:202)]
wq.data.use <- dat.all[,c(174, 186,196,202)]
log.wq <- log10(wq.data.use)

png('figures/log_wq_data_check.png', height = 1000, width = 1000)
pairs.panels(log.wq, method = 'pearson', hist.col = "#00AFBB", density = FALSE, 
             ellipses = FALSE, cex = 1.8, cex.cor = 1, col = "red", cex.axis = 2)
dev.off()

png('figures/wq_data_check.png', height = 1000, width = 1000)
pairs.panels(wq.data.use, method = 'pearson', hist.col = "#00AFBB", density = FALSE, 
             ellipses = FALSE, cex = 1.8, cex.cor = 1, col = "red", cex.lab = 1.3)
dev.off()

# some COD vs. acetate values look like outliers. E.g., COD is high when acetate is nondetect.
# This could be just because PG is high and acetate is low, where COD is coming from PG
# calculate the expected COD based on COD per unit of potassium acetate and propylene glycol
# molecular weight of PA = 98.15 g/mol, molecular weight acetate = 60.05, so acetate maeks up
# 61.6% of PA by weight
# 
CODperPG <- 1.57*(1/.88)*wq.data$Propylene_glycol
CODperA <- .34*(1/.5)*(1/.616)*wq.data$Acetate
expectedCOD <- CODperPG + CODperA

png('figures/COD_exp_obs.png', height = 500, width = 500)
plot(log10(wq.data$COD) ~ log10(expectedCOD), xlab = "Expected COD", ylab = "Oberved COD")
abline(0,1,col = "red")
dev.off()
