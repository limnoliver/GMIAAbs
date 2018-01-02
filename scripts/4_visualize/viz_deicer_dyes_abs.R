# create plots of deicer absorbance
library(RColorBrewer)
library(ggplot2)

# read in data from data release for dyes/deicers that has 
# already been processed/corrected

dd_dat <- read.csv('cached_data/dyes_deicers_for_sb.csv', stringsAsFactors = F)

# modify type to fit how we want plotting to occur
dd_dat$id[grep("solid", dd_dat$type)] <- "c (solid)"
dd_dat$id[grep("liquid", dd_dat$type)] <- "c (liquid)"
dd_dat$type[grep("Pavement", dd_dat$type)] <- "Pavement Deicer"
dd_dat$type[dd_dat$type == "Deicer - Type I"] <- "Type I Deicer"
dd_dat$type[dd_dat$type == "Deicer - Type IV"] <- "Type IV Deicer"

summary(as.factor(dd_dat$type))

# colors type I = orange, type IV = green, pavement = , dyes = individual
type.i.cols <- brewer.pal(n = 6, 'YlOrRd')[2:6]
type.iv.cols <- brewer.pal(n = 7, 'YlGn')[2:7]
pavement.cols <- brewer.pal(n = 6, 'Blues')[c(3,6)]
oranges <- brewer.pal(n = 6, 'Oranges')[c(2,4)]
dyes.col <- c('orangered', 'orange1', 'yellow2', 'royalblue2')
# set y axis limits
type.1.axes <- c(min(dd_dat$absorbance[dd_dat$type == "Type I Deicer"]), max(dd_dat$absorbance[dd_dat$type == "Type I Deicer"]))
type.iv.axes <- c(min(dd_dat$absorbance[dd_dat$type == "Type IV Deicer"]), max(dd_dat$absorbance[dd_dat$type == "Type IV Deicer"]))
pavement.axes <- c(min(dd_dat$absorbance[grep('Pavement', dd_dat$type)]), max(dd_dat$absorbance[grep('Pavement', dd_dat$type)]))
dyes.axes <- c(min(dd_dat$absorbance[dd_dat$type == "Dye"]), max(dd_dat$absorbance[dd_dat$type == "Dye"]))



pdf('figures/dyes_deicers.pdf')
# plot
layout(matrix(1:4, nrow = 2, ncol = 2, byrow = TRUE))
par(mar=c(1,2.5,0.2,0.3), cex.lab = 1.4, cex.axis = 1.2, oma = c(3,2,0,0))
# type I
typei <- filter(dd_dat, type == "Type I Deicer")
typeiids <- unique(typei$id)
plot(typei$absorbance[typei$id == typeiids[4]]~typei$wavelength[typei$id == typeiids[4]], 
     type = "l", lwd = 2, ylim = type.1.axes, col = type.i.cols[1],
     xlim = c(239, max = 800), xlab = '', ylab = "", xaxt = 'n')
axis(side = 1, tick = T, labels  = F)
mtext("Absorbance", side = 2, line = 2.5)
for(i in c(3,1,5,2)){
  points(typei$absorbance[typei$id == typeiids[i]]~typei$wavelength[typei$id == typeiids[i]], 
         type = "l", lwd = 2, col = type.i.cols[i])
}
legend('topright', legend = c('a', 'b', 'c', 'c (2017)', 'd'), col = type.i.cols, lwd = 2, title = 'Type I')

# type IV
typeiv <- filter(dd_dat, type == "Type IV Deicer")
typeivids <- unique(typeiv$id)

plot(typeiv$absorbance[typeiv$id == typeivids[4]]~typeiv$wavelength[typeiv$id == typeivids[4]], 
     type = "l", lwd = 2, ylim = type.iv.axes, col = type.iv.cols[1],
     xlim = c(239, max = 800), xlab = '', ylab = "", xaxt = 'n')
axis(side = 1, tick = T, labels  = F)

for(i in c(1,2,6,3,5)){
  points(typeiv$absorbance[typeiv$id == typeivids[i]]~typeiv$wavelength[typeiv$id == typeivids[i]], 
         type = "l", lwd = 2, col = type.iv.cols[i])
}
legend('topright', legend = c('a', 'b', 'c', 'c (2017)', 'd (F1)', 'd (F2)'), col = type.iv.cols, lwd = 2, title = 'Type IV')

# pavement 
pavement <- filter(dd_dat, type == "Pavement Deicer")
pavementids <- unique(pavement$id)

plot(pavement$absorbance[pavement$id == pavementids[1]]~pavement$wavelength[pavement$id == pavementids[1]],
      type = "l", lwd = 2, 
     ylim = pavement.axes, 
     col = pavement.cols[1],
     xlim = c(239, max = 800), xlab = 'Wavelength (nm)', ylab = "Absorbance")
points(pavement$absorbance[pavement$id == pavementids[2]]~pavement$wavelength[pavement$id == pavementids[2]], 
       type = "l", lwd = 2, col = pavement.cols[2])
mtext("Absorbance", side = 2, line = 2.5)
legend('topright', legend = c('c (liquid)', 'c (solid)'),col = pavement.cols, lwd = 2, title = 'Pavement')
mtext(text = "Wavelength (nm)", side = 1, line = 2.5)

# dyes
dyes <- filter(dd_dat, type == "Dye")
dyesids <- unique(dyes$id)

plot(dyes$absorbance[dyes$id == dyesids[1]]~dyes$wavelength[dyes$id == dyesids[1]], 
     type = 'l', lwd = 2, ylim = dyes.axes, col = dyes.col[1],
     xlim = c(239, max = 800), xlab = "Wavelength (nm)", ylab = "")
for(i in 2:4){
  points(dyes$absorbance[dyes$id == dyesids[i]]~dyes$wavelength[dyes$id == dyesids[i]], 
         type = "l", lwd = 2, col = dyes.col[i])
}
legend(x = 280, y = 0.131*20, legend = c('Orange II', 'Sunset Yellow', 'Tartrazine', 'Erioglycine'),col = dyes.col, lwd = 2, 
       title = 'Dyes')
mtext(text = "Wavelength (nm)", side = 1, line = 2.5)
dev.off()
