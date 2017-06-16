
# appears to get rid of standards/blanks
# this appears to be identifying samples of interest -- where columns
# start with the identifiers "ou"...etc. Gets rid of blanks, 1%, ETC

absraw <- read.csv("raw_data/rawCompiledAbs.csv")
FinalAbsDf <- absraw
finalcols <- colnames(absraw)
finalcols <- finalcols[which(substr(finalcols,1,2) %in% c("OU","Ou","CG","LK","OA","Wa"))]
FinalAbsDf <- FinalAbsDf[,finalcols]


# saved above files 09/30/2015 - all correct except missing OAK S106 with file issue
# note testAbs = SummaryAbsCoef.csv

abscoef <- read.csv('cached_data/SummaryAbsCoef.csv', header = TRUE)

# format text from column names to pull out date, time, project ID, etc
temp <- as.character(abscoef$GRnumber)
temp2 <- substr(abscoef$GRnumber,unlist(gregexpr(pattern="_2",temp))+1,unlist(gregexpr(pattern="_2",temp))+13)
temp3 <- temp
c <- grep("redo",temp)
for (i in 1:length(temp)) {
  a <- temp[i]
  b <- unlist(strsplit(a,"_"))
  temp3[i] <- b[1]
  if (i %in% c) {temp3[i] <- substr(temp3[i],1,nchar(temp3[i])-4)}
}

# extract date
abscoef$date <- substr(temp2,nchar(temp2)-7,nchar(temp2))
abscoef$datetime <- strptime(abscoef$date,format="%Y%m%d")

# extract project ID
abscoef$ProjectID <- temp3

# reduce to sites of interest for GMIA
abscoefGMIA <- abscoef[substr(abscoef$ProjectID,1,2) %in% c("OU","Ou","CG","LK","US","OA"),]
# remove unneeded characters for ProjectID field
abscoefGMIA$ProjectID <- gsub("-R","",abscoefGMIA$ProjectID)

abscoefOAK <- abscoef[grep("OAK",abscoef$ProjectID),]
# remove duplicate sample for OAK site
abscoefOAK <- abscoefOAK[which(paste(abscoefOAK$ProjectID,abscoefOAK$date,sep="")!="OAK-S10720140225"),]

abscoefOUT <- abscoef[substr(abscoef$ProjectID,1,2) %in% c("OU","Ou"),]
abscoefOUT <- abscoefOUT[-grep(".R",abscoefOUT$ProjectID),]
# remove duplicate samples for OUT site
abscoefOUT <- abscoefOUT[which(paste(abscoefOUT$ProjectID,abscoefOUT$date,sep="")!="OUT.S10720140225"),]
abscoefOUT <- abscoefOUT[which(paste(abscoefOUT$ProjectID,abscoefOUT$date,sep="")!="OUT.S107G20140225"),]
abscoefOUT <- abscoefOUT[which(substr(abscoefOUT$GRnumber,1,18)!="OUT.S110G_Group003"),]
abscoefOUT <- abscoefOUT[which(abscoefOUT$ProjectID!="OUT.S118-D"),]
abscoefOUT <- abscoefOUT[which(substr(abscoefOUT$GRnumber,1,10)!="OUT.S114D_"),]
abscoefOUT <- abscoefOUT[which(substr(abscoefOUT$GRnumber,1,10)!="OUT.S114E_"),]
abscoefOUT <- abscoefOUT[which(substr(abscoefOUT$GRnumber,1,10)!="OUT.S114F_"),]
abscoefOUT <- abscoefOUT[which(substr(abscoefOUT$GRnumber,1,10)!="OUT.S114G_"),]
abscoefOUT <- abscoefOUT[which(substr(abscoefOUT$GRnumber,1,10)!="OUT.S114H_"),]
abscoefOUT <- abscoefOUT[which(substr(abscoefOUT$GRnumber,1,10)!="OUT.S114J_"),]
abscoefOUT <- abscoefOUT[which(substr(abscoefOUT$GRnumber,1,10)!="OUT.S114K_"),]
abscoefOUT$ProjectID <- gsub('Out','OUT',abscoefOUT$ProjectID)

abscoefCG <- abscoef[grep("CG.",abscoef$ProjectID),]
# remove duplicate and QC samples for CG site
abscoefCG <- abscoefCG[which(paste(abscoefCG$ProjectID,abscoefCG$date,sep="")!="CG.S10720140225"),]
abscoefCG <- abscoefCG[which(abscoefCG$ProjectID!="CG.S116B"),]
abscoefCG <- abscoefCG[which(abscoefCG$ProjectID!="CG.Q23C"),]

abscoefLK <- abscoef[grep("LK.",abscoef$ProjectID),]
# remove duplicate and QC samples for LK site

#############
# Fix? Not sure this part is doing what it's supposed to
############
abscoefLK <- abscoefLK[which(abscoefLK$ProjectID!="LK.Q23C"),]
abscoefLK <- abscoefLK[-grep(".R",abscoefLK$ProjectID),]
abscoefLK <- abscoefLK[which(paste(abscoefLK$ProjectID,abscoefLK$date,sep="")!="LK.S10720140225"),]
abscoefLK <- abscoefLK[which(paste(abscoefLK$ProjectID,abscoefLK$date,sep="")!="LK.S107G20140225"),]

abscoefWorking <- rbind(abscoefOUT,abscoefCG)
abscoefWorking <- rbind(abscoefWorking,abscoefLK)
abscoefWorking <- rbind(abscoefWorking,abscoefOAK)
# after merging sites of interest, remove any remaining QC samples
abscoefWorking <- abscoefWorking[-which(substr(abscoefWorking$GRnumber,1,1)=='Q'),]
# limit samples to Nov-April deicing period
abscoefWorking <- abscoefWorking[-which(abscoefWorking$ProjectID %in% c("OUT.S110","OUT.S110G","OUT.S117","OUT.S118","CG.S110","CG.S117","CG.S118","LK.S110","LK.S110G","LK.S117","LK.S118","OAK.S110","OAK.S118")),]

grnumsIn <- unique(abscoefWorking$GRnumber)
grnumsIn <- as.character(grnumsIn)
grnumsIn <- c(grnumsIn,"Wavelength")
FinalAbsDf <- FinalAbsDf[,grnumsIn]
finalcols <- colnames(FinalAbsDf)
finalcols <- finalcols[which(substr(finalcols,1,2) %in% c("OU","Ou","CG","LK","OA","Wa"))]
finalcolsOUT <- finalcols[which(substr(finalcols,1,2) %in% c("OU","Ou"))]
finalcolsCG <- finalcols[which(substr(finalcols,1,2) %in% c("CG"))]
finalcolsLK <- finalcols[which(substr(finalcols,1,2) %in% c("LK"))]
finalcolsOA <- finalcols[which(substr(finalcols,1,2) %in% c("OA"))]
finalcolsALL <- finalcols[which(substr(finalcols,1,2) %in% c("OU","Ou","CG","LK","OA"))]

FinalAbsDf$meanAbs <- rowMeans(FinalAbsDf[,finalcolsALL])
FinalAbsDf$meanAbsOUT <- rowMeans(FinalAbsDf[,finalcolsOUT])
FinalAbsDf$meanAbsCG <- rowMeans(FinalAbsDf[,finalcolsCG])
FinalAbsDf$meanAbsLK <- rowMeans(FinalAbsDf[,finalcolsLK])
FinalAbsDf$meanAbsOA <- rowMeans(FinalAbsDf[,finalcolsOA])
FinalAbsDf[FinalAbsDf<0] <- NA
FinalAbsDf$minAbs <- do.call(pmin,c(FinalAbsDf[,finalcolsALL],na.rm=TRUE))
FinalAbsDf[is.na(FinalAbsDf)] <- min(FinalAbsDf$minAbs)

pathToSave <- "/Users/jlthomps/Documents/R/GMIA"
colsKeep <- c('Wavelength','meanAbs','meanAbsCG','meanAbsOUT','meanAbsOA','meanAbsLK')
absDf <- FinalAbsDf[,colsKeep]
WaveCol <- "Wavelength"
titleSize <- 1.1
mainTitle <- "GMIA Absorbance Plot"
xlim <- c(min(FinalAbsDf$Wavelength),max(FinalAbsDf$Wavelength))
ylim <- c(0,.1)
pdf(paste(pathToSave,"/","plotAbs.pdf",sep=""))
plot(absDf[,WaveCol],absDf[,2],type="l",lty=1,col="blue",xlab="Wavelength (nm)",ylab="Absorbance coefficient",ylim=c(0,.03))
points(absDf[,WaveCol],absDf[,3],type="l",col="green")
points(absDf[,WaveCol],absDf[,4],type="l",col="red")
points(absDf[,WaveCol],absDf[,5],type="l",col="black")
points(absDf[,WaveCol],absDf[,6],type="l",col="pink")
legend("topright",c("All","OUT","CG","LK","OAK"),col=c("blue","green","red","black","pink"),lty=c(1,1,1,1,1))
dev.off()

absDf <- FinalAbsDf[,c(1:105)]
nsteps <- 104
pdf(paste(pathToSave,"/","PlotAbsSamples.pdf",sep=""))
for (i in 1:nsteps) {  
  par(tcl=0.3)
  plot(absDf[,WaveCol],absDf[,i],type="l",lty=1,col="blue",xlab="Wavelength (nm)",ylab="Absorbance coefficient",ylim=c(0,0.05),main=paste("Absorbance plot ",finalcolsALL[i],sep=""))
}
dev.off()

pdf(paste(pathToSave,"/","PlotAbsSamplesSubset.pdf",sep=""))
parOriginal <- par(no.readonly = TRUE)

sample1 <- "OUT-S105_Group002GMIA0006_2013/20131223"
plot(absDf[,WaveCol],absDf[,sample1],type="l",lty=1,col="blue",xlab="Wavelength (nm)",ylab="Absorbance coefficient",ylim=c(0,0.05),main="Absorbance plot for Outfall Storm 105")
legend("topright",c("Acetate 1430","Ethylene Glycol <10","Propylene Glycol 1100","Formate <25"))
par(parOriginal)
sample1 <- "Out-S106_Group001GMIA0008_2014/20140121"
plot(absDf[,WaveCol],absDf[,sample1],type="l",lty=1,col="blue",xlab="Wavelength (nm)",ylab="Absorbance coefficient",ylim=c(0,0.05),main="Absorbance plot for Outfall Storm 106")
legend("topright",c("Acetate 490","Ethylene Glycol <10","Propylene Glycol 330","Formate <2.5"))
par(parOriginal)
sample1 <- "OUT-S107_Group002GMIA0003_2014/20140227"
plot(absDf[,WaveCol],absDf[,sample1],type="l",lty=1,col="blue",xlab="Wavelength (nm)",ylab="Absorbance coefficient",ylim=c(0,0.05),main="Absorbance plot for Outfall Storm 107")
legend("topright",c("Acetate 461","Ethylene Glycol <10","Propylene Glycol 340","Formate <2.5"))
par(parOriginal)
sample1 <- "CG-S106_Group001GMIA0006_2014/20140121"
plot(absDf[,WaveCol],absDf[,sample1],type="l",lty=1,col="blue",xlab="Wavelength (nm)",ylab="Absorbance coefficient",ylim=c(0,0.05),main="Absorbance plot for Cargo Storm 106")
legend("topright",c("Acetate 622","Ethylene Glycol 37","Propylene Glycol 2500","Formate <2.5"))
par(parOriginal)
sample1 <- "CG-S107_Group002GMIA0008_2014/20140227"
plot(absDf[,WaveCol],absDf[,sample1],type="l",lty=1,col="blue",xlab="Wavelength (nm)",ylab="Absorbance coefficient",ylim=c(0,0.05),main="Absorbance plot for Cargo Storm 107")
legend("topright",c("Acetate 125","Ethylene Glycol 32","Propylene Glycol 1500","Formate 4.66"))
par(parOriginal)
sample1 <- "CG-S111_Group002GMIA0008_2014/20141201"
plot(absDf[,WaveCol],absDf[,sample1],type="l",lty=1,col="blue",xlab="Wavelength (nm)",ylab="Absorbance coefficient",ylim=c(0,0.05),main="Absorbance plot for Cargo Storm 111")
legend("topright",c("Acetate 864","Ethylene Glycol 100","Propylene Glycol 1400","Formate <2.5"))
par(parOriginal)
sample1 <- "OUT-S114A_Group002GMIA0001_2015/20150305"
plot(absDf[,WaveCol],absDf[,sample1],type="l",lty=1,col="blue",xlab="Wavelength (nm)",ylab="Absorbance coefficient",ylim=c(0,0.05),main="Absorbance plot for Outfall Storm 114")
legend("topright",c("Acetate 136","Ethylene Glycol 23","Propylene Glycol 77","Formate <2.5"))
par(parOriginal)
sample1 <- "OUT-S113_Group002GMIA0005_2015/20150106"
plot(absDf[,WaveCol],absDf[,sample1],type="l",lty=1,col="blue",xlab="Wavelength (nm)",ylab="Absorbance coefficient",ylim=c(0,0.05),main="Absorbance plot for Outfall Storm 113")
legend("topright",c("Acetate 179","Ethylene Glycol <10","Propylene Glycol 1400","Formate <2.5"))
par(parOriginal)
dev.off()


library(USGSHydroOpt)
# calculate residuals for selected absorbence ranges
source("/Users/jlthomps/Desktop/git/GMIA/getExpResidJT.R")
wavelength <- 491
rangeReg <- c(419,602)
rangeGap <- c(461,521)
colsAbs <- unique(testAbsWorking$GRnumber)
#colsAbs <- colsAbs[-which(substr(colsAbs,1,1)=='Q')]
colsAbs <- c(colsAbs,"Wavelength")
dataAbs <- FinalAbsDf[,colsAbs]
waveCol <- "Wavelength"
colSubsetString <- "Gr"
dataSummary <- testAbsWorking
grnum <- "GRnumber"
testdfOpt <- getExpResidJT(wavelength,rangeReg,rangeGap,dataAbs,waveCol,colSubsetString,dataSummary,grnum)

wavelength <- 629
rangeReg <- c(560,698)
rangeGap <- c(605,662)
dataSummary <- testdfOpt
testdfOpt2 <- getExpResidJT(wavelength,rangeReg,rangeGap,dataAbs,waveCol,colSubsetString,dataSummary,grnum)
cols2 <- colnames(testdfOpt2)
cols2 <- cols2[1:158]
cols2 <- c(cols2,"Resids490","Resids630")
colnames(testdfOpt2) <- cols2

wavelength <- 422
rangeReg <- c(377,497)
rangeGap <- c(404,452)
dataSummary <- testdfOpt2
testdfOpt3 <- getExpResidJT(wavelength,rangeReg,rangeGap,dataAbs,waveCol,colSubsetString,dataSummary,grnum)
cols2 <- colnames(testdfOpt2)
cols2 <- c(cols2,"Resids422")
colnames(testdfOpt3) <- cols2

# calculate slopes for selected absorbence values
sag <- read.csv("SagVals.csv",stringsAsFactors=FALSE)
colSubsetString <- "Gr"
dataSummary <- testdfOpt3
grnum <- "GRnumber"
source("/Users/jlthomps/Desktop/git/GMIA/getSagJT.R")
GMIASag <- getSagJT(FinalAbsDf,waveCol,sag,colSubsetString,dataSummary,"GRnumber")

save(GMIASag,file="GMIASagFinal.RData")
# file saved 10/7/15
