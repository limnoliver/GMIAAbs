# find the minimum reporting limit from field blanks
abs.raw <- read.csv('raw_data/rawCompiledAbs.csv')

# test if blanks by site makes a difference
# that is, is there contamination at any sites?

blankGRnums.all <- grep('^Q[[:alpha:]]', names(abs.raw), value = TRUE)
blankGRnums.OUT <- grep('QOUT', names(abs.raw), value = TRUE)
blankGRnums.LK <- grep('QLK', names(abs.raw), value = TRUE)
blankGRnums.CG <- grep('QCG', names(abs.raw), value = TRUE)
blankGRnums.US <- grep('QUS', names(abs.raw), value = TRUE)

data.list <- list()
data.list[[1]] <- absMRL(abs.raw, "Wavelength", blankGRnums.all)
data.list[[2]] <- absMRL(abs.raw, "Wavelength", blankGRnums.OUT)
data.list[[3]] <- absMRL(abs.raw, "Wavelength", blankGRnums.LK)
data.list[[4]] <- absMRL(abs.raw, "Wavelength", blankGRnums.CG)
data.list[[5]] <- absMRL(abs.raw, "Wavelength", blankGRnums.US)


# visualize data for major differences
absMRL.viz <- function(data.list, var){
plot(data.list[[1]][, var]~data.list[[1]][, 'Wavelength'], type = "l")
points(data.list[[2]][, var]~data.list[[2]][, 'Wavelength'], type = "l", col = "red")
points(data.list[[3]][, var]~data.list[[3]][, 'Wavelength'], type = "l", col = "blue")
points(data.list[[4]][, var]~data.list[[4]][, 'Wavelength'], type = "l", col = "green")
points(data.list[[5]][, var]~data.list[[5]][, 'Wavelength'], type = "l", col = "purple")
legend("topright", legend = c("All", "OUT", "LK", "CG", "US"), 
       col = c('black', 'red', 'blue', 'green', 'purple'), lty = 1, title = paste(var," of Blank", sep = ""))
}
data.list <- list()
data.list[[1]]<- test.all
data.list[[2]] <- test.OUT
data.list[[3]] <- test.LK
data.list[[4]] <- test.CG
data.list[[5]] <- test.US

par(mfrow=c(1,1))

absMRL.viz(data.list = data.list, var = "mean")

