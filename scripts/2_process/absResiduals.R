library(USGSHydroOpt)

# read in processed data
testAbsWorking <- read.csv('cached_data/cleanedAbsData.csv')
FinalAbsDf <- read.csv('cached_data/SummarizedAbsData.csv')

# set wavelengths of interest, use getExpResid to calculate residuals
wavelength <- 491
rangeReg <- c(419,602)
rangeGap <- c(461,521)
colsAbs <- unique(testAbsWorking$GRnumber)
#colsAbs <- colsAbs[-which(substr(colsAbs,1,1)=='Q')]
colsAbs <- c(as.character(colsAbs),"Wavelength")
dataAbs <- FinalAbsDf[,colsAbs]
waveCol <- "Wavelength"
colSubsetString <- "Gr"
dataSummary <- testAbsWorking
grnum <- "GRnumber"
testdfOpt <- getExpResid(wavelength,rangeReg,rangeGap,dataAbs,waveCol,colSubsetString,dataSummary,grnum)

# do same as above, but at different wavelength. merge data frames together
wavelength <- 629
rangeReg <- c(560,698)
rangeGap <- c(605,662)
dataSummary <- testdfOpt
testdfOpt2 <- getExpResid(wavelength,rangeReg,rangeGap,dataAbs,waveCol,colSubsetString,dataSummary,grnum)
cols2 <- colnames(testdfOpt2)
cols2 <- cols2[1:158]
cols2 <- c(cols2,"Resids490","Resids630")
colnames(testdfOpt2) <- cols2

# do same as above, but at different wavelength. merge data frames together
wavelength <- 422
rangeReg <- c(377,497)
rangeGap <- c(404,452)
dataSummary <- testdfOpt2
testdfOpt3 <- getExpResid(wavelength,rangeReg,rangeGap,dataAbs,waveCol,colSubsetString,dataSummary,grnum)
names(testdfOpt3)[length(testdfOpt3)] <- "Resids422"

#write data
write.csv(testdfOpt3, 'cached_data/absResiduals.csv', row.names = FALSE)

