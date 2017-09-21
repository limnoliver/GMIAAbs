# create plots of deicer absorbance
library(RColorBrewer)

dyes <- read.csv('raw_data/DyeDilution.csv')
deicers <- read.csv('raw_data/DeicerAbs.csv')
deicers_ids <- c('Group003GMIA0004', 'Group003GMIA0005')
location <- "//igsarmewwshg8/HG8Data/Aqualog/AquaLog_Data/2017/20170127/"

# get type I deicer measured in 2017
all.files <- list.files(path = location)
file1 <- all.files[grep(paste0(deicers_ids[1], 'ABS.dat'), all.files)]
deicer_2017_1 <- read.table(paste(location, file1, sep = '/'), header = FALSE)
names(deicer_2017_1) <- c('Wavelength', 'CPP-I_')

# correct for dilution factor which is 20

deicer_2017_1$`CPP-I_` <- deicer_2017_1$`CPP-I_`*20

# get type IV deicer measured in 2017
file2 <- all.files[grep(paste0(deicers_ids[2], 'ABS.dat'), all.files)]
deicer_2017_2 <- read.table(paste(location, file2, sep = '/'), header = FALSE)
names(deicer_2017_2) <- c('Wavelength', 'CPGA-IV_')

# correct for dilution factor which is 50
deicer_2017_2$`CPGA-IV_` <- deicer_2017_2$`CPGA-IV_`*50

deicers <- merge(deicers, deicer_2017_1, all.x = TRUE)
deicers <- merge(deicers, deicer_2017_2, all.x = TRUE)


# plot four panels (type I, type IV, pavement, dyes)
type.iv <- grep('IV_', names(deicers))
type.i <- grep('I_', names(deicers))
pavement <- grep('pavement', names(deicers), ignore.case = TRUE)

# colors type I = orange, type IV = green, pavement = , dyes = individual
type.i.cols <- brewer.pal(n = 6, 'YlOrRd')[2:6]
type.iv.cols <- brewer.pal(n = 6, 'YlGn')[2:6]
pavement.cols <- brewer.pal(n = 6, 'Blues')[c(3,6)]
oranges <- brewer.pal(n = 6, 'Oranges')[c(2,4)]
dyes.col <- c('orangered', 'orange1', 'yellow2', 'royalblue2')
# set y axis limits
type.1.axes <- c(min(deicers[,type.i]), max(deicers[,type.i]))
type.iv.axes <- c(min(deicers[,type.iv]), max(deicers[,type.iv]))
pavement.axes <- c(min(deicers[,pavement]), max(deicers[,pavement]))
dyes.axes <- c(min(dyes[,2:5]), max(dyes[,2:5]))

pdf('figures/dyes_deicers.pdf')
# plot
layout(matrix(1:4, nrow = 2, ncol = 2, byrow = TRUE))
par(mar=c(4.5,4.5,0.2,0.3), cex.lab = 1.4, cex.axis = 1.2, oma = c(0,0,0,0))
# type I
plot(deicers[,type.i[1]]~deicers$Wavelength, type = "l", lwd = 2, ylim = type.1.axes, col = type.i.cols[1],
     xlim = c(239, max = 800), xlab = '', ylab = "Absorbance")
for(i in 2:length(type.i)){
  points(deicers[,type.i[i]]~deicers$Wavelength, type = "l", lwd = 2, col = type.i.cols[i])
}
legend('topright', legend = c('c', 'd', 'b', 'a', 'c (2017)'), col = type.i.cols, lwd = 2, title = 'Type I')

# type IV
plot(deicers[,type.iv[1]]~deicers$Wavelength, type = "l", lwd = 2, ylim = type.iv.axes, col = type.iv.cols[1],
     xlim = c(239, max = 800), xlab = '', ylab = "")
for(i in 2:length(type.iv)){
  points(deicers[,type.iv[i]]~deicers$Wavelength, type = "l", lwd = 2, col = type.iv.cols[i])
}
legend('topright', legend = c('b', 'c', 'd', 'a', 'c (2017)'), col = type.iv.cols, lwd = 2, title = 'Type IV')

# pavement 
plot(deicers[,pavement[1]]~deicers$Wavelength, type = "l", lwd = 2, ylim = pavement.axes, col = pavement.cols[1],
     xlim = c(239, max = 800), xlab = 'Wavelength (nm)', ylab = "Absorbance")
points(deicers[,pavement[2]]~deicers$Wavelength, type = "l", lwd = 2, col = pavement.cols[2])
legend('topright', legend = c('c (liquid)', 'c (solid)'),col = pavement.cols, lwd = 2, title = 'Pavement')


# dyes

plot(dyes[,2]~dyes$Wavelength, type = 'l', lwd = 2, ylim = dyes.axes, col = dyes.col[1],
     xlim = c(239, max = 800), xlab = "Wavelength (nm)", ylab = "")
for(i in 3:5){
  points(dyes[,i]~dyes$Wavelength, type = "l", lwd = 2, col = dyes.col[i-1])
}
legend(x = 280, y = 0.131, legend = c('Orange II', 'Sunset Yellow', 'Tartrazine', 'Erioglycine'),col = dyes.col, lwd = 2, 
       title = 'Dyes')

dev.off()
