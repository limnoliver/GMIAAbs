library(dplyr)
library(tidyr)
library(ggjoy)

abs <- read.csv('cached_data/filteredWQ_DOC_ABS.csv')
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

plot(abs.joy[, 1]~abs.joy$wavelength, type = 'l', ylim = c(0,0.2), lwd = 2, col = as.character(abs.year[1]))

for (cols in 2:45) {
  points(abs.joy[,cols]~abs.joy$wavelength, type = 'l', col = as.character(abs.year[cols]), lwd = 2)
  
}
