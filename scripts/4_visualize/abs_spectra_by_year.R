library(dplyr)
library(tidyr)
library(ggjoy)

abs <- read.csv('cached_data/filteredWQ_DOC_ABS.csv')
abs$year <- substr(abs$datetime, 1, 4)

deicers <- read.csv('raw_data/DeicerAbs.csv')

cargo <- grep('CG', abs$ProjectID)
abs.joy <- abs %>%
  filter(grepl('CG', ProjectID))

years <- substr(abs.joy$datetime, 1, 4)
#abs.joy$uniqueid <- 1:nrow(abs.joy)

abs.joy <- abs.joy %>%
  select(A500:A239) 

abs.joy <- as.data.frame(t(abs.joy))
abs.joy$wavelength <- row.names(abs.joy)
abs.joy$wavelength <- gsub('A', '', abs.joy$wavelength)

abs.year <- as.factor(years)
levels(abs.year) <- c(rgb(215,25,28, max = 255, alpha = 100), 
                      rgb(253,174,97, max = 255, alpha = 100),
                      rgb(171,221,164, max = 255, alpha = 100),
                      rgb(43,131,186, max = 255, alpha = 100))

# plot just 2015 data

yr2015 <- grep(2015, years)
plot(abs.joy[, yr2015[1]]~abs.joy$wavelength, type = 'n', ylim = c(0,0.1), lwd = 2, col = as.character(abs.year[1]))
abline(v = 500, lwd = 3, col = 'lightgray')
rect(xleft = 281, xright = 299, ybottom = -0.05, ytop = .11, col = rgb(200,200,200,max = 255,100), border = NA)
rect(xleft = 239, xright = 248, ybottom = -0.05, ytop = .11, col = rgb(200,200,200,max = 255,100), border = NA)
rect(xleft = 263, xright = 293, ybottom = -0.05, ytop = .11, col = rgb(200,200,200,max = 255,100), border = NA)

for (cols in yr2015) {
  points(abs.joy[,cols]~abs.joy$wavelength, type = 'l', col = 'darkgray', lwd = 2)
  
}

# add deicer
deicer.short <- deicers[101:188,c(3,4, 14)]
points(deicer.short$Cryotech.Polar.Plus.Type.I_Group002GMIA0006_2014.20141216/50 ~ deicer.short$Wavelength, type = 'l', lwd = 3, col = 'orangered')
points(deicer.short$Cryotech.Polar.Guard.Advance.Type.IV_Group002GMIA0007_2014.20141216/100 ~ deicer.short$Wavelength, type = 'l', lwd = 3, col = 'green3')


# add labels
legend(x = 350, y = 0.105, legend = c('top predictor regions', '2015 environmental samples', 'Type I', 'Type IV'), 
       col = c('lightgray', rgb(253,174,97, max = 255, alpha = 100),'orangered', 'green3'), lwd = c(5,3,3,3), lty = 1)

plot(abs.joy[, 1]~abs.joy$wavelength, type = 'l', ylim = c(0,0.1), lwd = 2, col = as.character(abs.year[1]))

for (cols in 2:45) {
  points(abs.joy[,cols]~abs.joy$wavelength, type = 'l', col = as.character(abs.year[cols]), lwd = 2)
  
}

legend('topright', legend = 2014:2017, col = levels(abs.year), lty = 1, lwd = 2)

plot(abs.joy[, 7]~abs.joy$wavelength, type = 'l', ylim = c(0,0.1), lwd = 2, col = as.character(abs.year[1]))
points(abs.joy[, 4]~abs.joy$wavelength, type = 'l', ylim = c(0,0.1), lwd = 2, col = as.character(abs.year[1]))

dyes <- read.csv('raw_data/DyeDilution.csv')


# plot just one line where PG is known
png('figures/enviro_samples_vs_deicers.png')
par(mar=c(5,5,1,1))
plot(abs.joy[, yr2015[4]]~abs.joy$wavelength, type = 'n', ylim = c(0,0.2), lwd = 3, col = 'blue2',
     xlab = 'Wavelength (nm)', ylab = 'Absorbance', cex.lab = 1.5, cex.axis = 1.2)
abline(v = 500, lwd = 3, col = 'lightgray')
rect(xleft = 281, xright = 299, ybottom = -0.05, ytop = .21, col = rgb(200,200,200,max = 255,100), border = NA)
rect(xleft = 239, xright = 248, ybottom = -0.05, ytop = .21, col = rgb(200,200,200,max = 255,100), border = NA)
rect(xleft = 263, xright = 293, ybottom = -0.05, ytop = .21, col = rgb(200,200,200,max = 255,100), border = NA)
points(abs.joy[, yr2015[4]]~abs.joy$wavelength, type = 'l', ylim = c(0,0.2), lwd = 3, col = 'blue2')

points(deicer.short$Cryotech.Polar.Plus.Type.I_Group002GMIA0006_2014.20141216/67.69 ~ deicer.short$Wavelength, type = 'l', lwd = 3, col = 'orangered')
points(deicer.short$Cryotech.Polar.Guard.Advance.Type.IV_Group002GMIA0007_2014.20141216/36.15 ~ deicer.short$Wavelength, type = 'l', lwd = 3, col = 'green3')
points(abs.joy[, yr2015[2]]~abs.joy$wavelength, type = 'l', lwd = 3, col = 'blue4')

legend(x = 300, y = 0.2, legend = c('top predictor regions', 'cargo sample - Dec 13 2015', '(13 mg/L PG)', 
                                      'cargo sample - Dec 29 2015', '(13000 mg/L PG)', 'Type I deicer - 1.5% (13,000 mg/L PG)', 'Type IV deicer - 2.8% (13,000 mg/L PG)'), 
       col = c('lightgray', 'blue4', NA, 'blue2', NA, 'orangered', 'green3'), lwd = c(5,3,NA,3,NA,3,3), lty = 1, bty = 'n')
dev.off()




