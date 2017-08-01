library(USGSHydroOpt)

# read in processed data
dataAbs <- read.csv('cached_data/correctedAbsData.csv')
dataSummary <- data.frame(GRnumber = names(dataAbs)[-1])


# set wavelengths of interest, use getExpResid to calculate residuals
wavelength <- 491
rangeReg <- c(419,602)
rangeGap <- c(461,521)
waveCol <- "Wavelength"
colSubsetString <- "Gr"
grnum <- "GRnumber"

testdfOpt <- getExpResid(wavelength,rangeReg,rangeGap,dataAbs,waveCol,colSubsetString,dataSummary,grnum)
names(testdfOpt)[ncol(testdfOpt)] <- 'Resids491'

# do same as above, but at different wavelength. merge data frames together
wavelength <- 629
rangeReg <- c(560,698)
rangeGap <- c(605,662)
dataSummary <- testdfOpt
testdfOpt2 <- getExpResid(wavelength,rangeReg,rangeGap,dataAbs,waveCol,colSubsetString,dataSummary,grnum)
names(testdfOpt2)[ncol(testdfOpt2)] <- 'Resids629'

# do same as above, but at different wavelength. merge data frames together
wavelength <- 422
rangeReg <- c(377,497)
rangeGap <- c(404,452)
dataSummary <- testdfOpt2
testdfOpt3 <- getExpResid(wavelength,rangeReg,rangeGap,dataAbs,waveCol,colSubsetString,dataSummary,grnum)
names(testdfOpt3)[length(testdfOpt3)] <- "Resids422"

#write data
write.csv(testdfOpt3, 'cached_data/absResiduals.csv', row.names = FALSE)

