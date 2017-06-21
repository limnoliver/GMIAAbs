library(USGSHydroOpt)
testAbsWorking <- read.csv('cached_data/AbsData.csv')
FinalAbsDf <- read.csv('cached_data/SummarizedAbsData.csv')
# calculate residuals for selected absorbence ranges
#source("/Users/jlthomps/Desktop/git/GMIA/getExpResidJT.R")
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

wavelength <- 629
rangeReg <- c(560,698)
rangeGap <- c(605,662)
dataSummary <- testdfOpt
testdfOpt2 <- getExpResid(wavelength,rangeReg,rangeGap,dataAbs,waveCol,colSubsetString,dataSummary,grnum)
cols2 <- colnames(testdfOpt2)
cols2 <- cols2[1:158]
cols2 <- c(cols2,"Resids490","Resids630")
colnames(testdfOpt2) <- cols2

wavelength <- 422
rangeReg <- c(377,497)
rangeGap <- c(404,452)
dataSummary <- testdfOpt2
testdfOpt3 <- getExpResid(wavelength,rangeReg,rangeGap,dataAbs,waveCol,colSubsetString,dataSummary,grnum)
names(testdfOpt3)[length(testdfOpt3)] <- "Resids422"


# calculate slopes for selected absorbence values
sag <- read.csv("SagVals.csv",stringsAsFactors=FALSE)
colSubsetString <- "Gr"
dataSummary <- testdfOpt3
grnum <- "GRnumber"
source("/Users/jlthomps/Desktop/git/GMIA/getSagJT.R")
GMIASag <- getSagJT(FinalAbsDf,waveCol,sag,colSubsetString,dataSummary,"GRnumber")

save(GMIASag,file="GMIASagFinal.RData")
# file saved 10/7/15