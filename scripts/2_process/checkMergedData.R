library(readxl)
dat.all <- read.csv('cached_data/mergedWQ_DOC_ABS.csv')
all.dat <- dat.all
# find missing COD data (should have the WQ samples)
dat.all <- all.dat
missing <- all.dat[is.na(all.dat$COD),]
missing.DOC <- all.dat[is.na(all.dat$DOC), ]

# verify that all storm events from optics.sample.log.ALL are in the 
# pulled data

sample.log <- read_xlsx('M:/NonPoint Evaluation/gmia/SLOH labforms and budget/optics.sample.log.ALL.xlsx')
sample.log$Storm <- gsub("-", ".", sample.log$Storm)
samples.in <- which(sample.log$Storm %in% dat.all$ProjectID)
samples.out <- sample.log[-samples.in, ]
upstream <- grep('US.', samples.out$Storm)
samples.out <- samples.out[-upstream, ]

# data checks
library(RColorBrewer)
all.dat$colors <- all.dat$date
all.dat$colors <- as.factor(all.dat$colors)

palette(colorRampPalette(c("pink", "red", "purple", "purple4"))(22))
plot(log10(all.dat$COD)~log10(all.dat$DOC), col = levels(as.factor(all.dat$date)), pch = 16)
legend("topleft", legend = levels(as.factor(all.dat$date)), pch = 16, col = levels(as.factor(all.dat$date)))




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

# Look into DOC outliers
DOC.outliers <- subset(log.wq, log.wq$COD < 2.2 & log.wq$DOC > 1.5 & log.wq$DOC <2.2)
outliers <- c(91,112,114,193)
wq.data.outs <- all.dat[which(rownames(all.dat) %in% as.character(outliers)), ]
