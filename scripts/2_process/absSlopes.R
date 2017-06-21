# script that uses output from absResiduals, and adds spectral slopes

# calculate slopes for selected absorbence values
sag <- read.csv("SagVals.csv",stringsAsFactors=FALSE)
colSubsetString <- "Gr"
dataSummary <- testdfOpt3
grnum <- "GRnumber"
source("/Users/jlthomps/Desktop/git/GMIA/getSagJT.R")
GMIASag <- getSagJT(FinalAbsDf,waveCol,sag,colSubsetString,dataSummary,"GRnumber")

save(GMIASag,file="GMIASagFinal.RData")
# file saved 10/7/15