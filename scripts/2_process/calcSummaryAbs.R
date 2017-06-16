library(USGSHydroOpt)

FinalAbsDf <- read.csv("raw_data/rawCompiledAbs.csv", header = TRUE)
abscoef <- read.csv('cached_data/SummaryAbsCoef.csv', header = TRUE)

# Filter out specific column names
finalcols <- colnames(FinalAbsDf)
finalcols <- finalcols[which(substr(finalcols,1,2) %in% c("OU","Ou","CG","LK","OA","Wa"))]
FinalAbsDf <-FinalAbsDf[,finalcols]


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
abscoefGMIA$ProjectID <- gsub(".R","",abscoefGMIA$ProjectID)

abscoefOAK <- abscoef[grep("OAK",abscoef$ProjectID),]
# remove duplicate sample for OAK site
abscoefOAK <- abscoefOAK[which(paste(abscoefOAK$ProjectID,abscoefOAK$date,sep="")!="OAK.S10720140225"),]

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

###################################
# stops this function from failing
# decide where this should belong, but for now can go here
###################################

testnames <- colnames(FinalAbsDf)
testnames <- gsub("USGS","Group",testnames)
colnames(FinalAbsDf) <- testnames

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

write.csv(FinalAbsDf, "cached_data/SummarizedAbsData.csv", row.names = FALSE)
