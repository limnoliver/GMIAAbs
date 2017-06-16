
setwd("/Users/jlthomps/Desktop/git/GMIA")
COD2014 <- read.csv(file="COD2014.csv",stringsAsFactors=FALSE)
COD2014$ProjectID <- paste(COD2014$Site,COD2014$Storm,sep="-")
DOC2014 <- c("030414.csv","090814.csv","03182014.csv","04242014.csv","20141201b.csv","20141210.csv","Lenaker012214.csv","lenaker_041214.csv","20140506.csv","20150112.csv","20150120.csv","20150311.csv","20150324.csv","20150326.csv","20150626.csv","12232013.csv","lenaker121113.csv")
FolderName="GMIA_Corsi-Lenaker"
DfMerge<-COD2014
DfMergeSamps='ProjectID'
for (i in 1:length(DOC2014)) {
  FileName=DOC2014[i]
  cat(paste(i,FileName,'\n',sep=""))
  FilePath <- paste('//Igsarmewwsscu/SCUData/TOC_REPORTS',FolderName,FileName,sep='/')
  
  DOC <- read.csv(FilePath,stringsAsFactors=FALSE)
  Blankrows <- grep('blanks',DOC[,'SampleName'])
  Blankrows <- c(grep('blank',DOC[,'SampleName']),Blankrows)
  Blankrows <- c(grep('BLANK',DOC[,'SampleName']),Blankrows)
  
  TwentyCheckrows <- grep('20 ppm test',DOC[,'SampleName'])
  TwentyCheckrows <- c(grep('20PPMCHECK',DOC[,'SampleName']),TwentyCheckrows)
  TwentyCheckrows <- c(grep('20 PPM CHECK',DOC[,'SampleName']),TwentyCheckrows)
  
  OneCheckrows <- grep('1 PPM CHECK',DOC[,'SampleName'])
  OneCheckrows <- c(grep('1 ppmtest',DOC[,'SampleName']),OneCheckrows)
  
  Unknownrows <- grep('unknown',DOC[,'SampleName'])
  
  Standardrows <- grep('standards',DOC[,'SampleName'])
  Standardrows <- c(grep('high stds',DOC[,'SampleName']),Standardrows)
  
  AllQA <- sort(c(Blankrows,TwentyCheckrows,OneCheckrows,Unknownrows,Standardrows))
  if (length(AllQA)==0) {
    Df <- DOC
  } else {Df <- DOC[-c(AllQA),]}
  Df[,'MeanConc.'] <- as.numeric (Df[,'FinalConc'])
  DfNew <- data.frame(SampleName=as.character(),DOCResult=as.numeric()) 
  
  if (length(unique(Df[,'SampleName']))>0) {
  for(j in 1:length(unique(Df[,'SampleName']))){
    SampName <- unique(Df[,'SampleName'])[j]
    TempDf <- Df[which(Df$SampleName==SampName),]
    DfNew[,'SampleName'] <- as.character(DfNew[,'SampleName'])
    DfNew[j,'SampleName'] <- SampName
    DfNew[j,'DOCResult'] <- mean(TempDf[,'FinalConc'])
  }
  }
  
  SummarySamps <- DfMerge[,DfMergeSamps]
  DOCSamps <- DfNew[,'SampleName']
  
  nCom <- which(SummarySamps %in% DOCSamps)
  
  
  if (length(nCom)>0) {
  for (k in 1:length(nCom)){
    DfMerge[nCom[k],'DOCResult'] <- DfNew[,'DOCResult'][which(DfNew[,'SampleName']==DfMerge[nCom[k],DfMergeSamps])]
  }
  DfFinal <- DfMerge
  }
}

DfFinal <- DfFinal[which(!is.na(DfFinal$COD) & !is.na(DfFinal$DOCResult)),]
DfFinal$SiteAll <- ifelse(DfFinal$Site=='CG',"#009E73",ifelse(DfFinal$Site=='LK',"#E69F00",ifelse(DfFinal$Site=='OAK',"#0072B2","#CC79A7")))
DfFinal$SiteCG <- ifelse(DfFinal$Site=='CG',"#009E73","#999999")
DfFinal$SiteLK <- ifelse(DfFinal$Site=='LK',"#E69F00","#999999")
DfFinal$SiteOAK <- ifelse(DfFinal$Site=='OAK',"#0072B2","#999999")
DfFinal$SiteOUT <- ifelse(DfFinal$Site=='OUT',"#CC79A7","#999999")
DfFinal$logCOD <- log10(DfFinal$COD)
DfFinal$logDOC <- log10(DfFinal$DOCResult)

pdf("CODvsDOC.pdf")
parOriginal <- par(no.readonly = TRUE)

plot(DfFinal$logDOC,DfFinal$logCOD,pch=20,col=DfFinal$SiteAll,type="p",main="COD vs DOC for Runoff Events at GMIA USGS sites, 2013-2014",xlab="log(DOC)",ylab="log(COD)")  
lmFormula <- "logCOD ~ logDOC"
lmfit <- do.call("lm", list(lmFormula, data = DfFinal))
temp <- summary(lmfit)
rsquare <- round(temp$adj.r.squared,2)
abline(lmfit, col="red")
mtext(paste("log(COD) = ",round(coef(lmfit)[1],3)," + ",round(coef(lmfit)[2],3),"log(DOC) \nR-squared=",rsquare,sep=""), side = 1, line = -1.5, cex = 0.7)
legend("topleft",c("LK","CG","OAK","OUT"),pch=c(20,20,20,20),col=c("#E69F00","#009E73","#0072B2","#CC79A7"))
par(parOriginal)
plot(DfFinal$logDOC,DfFinal$logCOD,pch=20,col=DfFinal$SiteCG,type="p",main="COD vs DOC with Cargo highlighted",xlab="log(DOC)",ylab="log(COD)")
lmFormula <- "logCOD ~ logDOC"
lmfit <- do.call("lm", list(lmFormula, data = DfFinal))
temp <- summary(lmfit)
rsquare <- round(temp$adj.r.squared,2)
abline(lmfit, col="red")
mtext(paste("log(COD) = ",round(coef(lmfit)[1],3)," + ",round(coef(lmfit)[2],3),"log(DOC) \nR-squared=",rsquare,sep=""), side = 1, line = -1.5, cex = 0.7)
legend("topleft",c("LK","CG","OAK","OUT"),pch=c(20,20,20,20),col=c("#999999","#009E73","#999999","#999999"))
par(parOriginal)
plot(DfFinal$logDOC,DfFinal$logCOD,pch=20,col=DfFinal$SiteLK,type="p",main="COD vs DOC with St Luke's highlighted",xlab="log(DOC)",ylab="log(COD)")
lmFormula <- "logCOD ~ logDOC"
lmfit <- do.call("lm", list(lmFormula, data = DfFinal))
temp <- summary(lmfit)
rsquare <- round(temp$adj.r.squared,2)
abline(lmfit, col="red")
mtext(paste("log(COD) = ",round(coef(lmfit)[1],3)," + ",round(coef(lmfit)[2],3),"log(DOC) \nR-squared=",rsquare,sep=""), side = 1, line = -1.5, cex = 0.7)
legend("topleft",c("LK","CG","OAK","OUT"),pch=c(20,20,20,20),col=c("#E69F00","#999999","#999999","#999999"))
par(parOriginal)
plot(DfFinal$logDOC,DfFinal$logCOD,pch=20,col=DfFinal$SiteOAK,type="p",main="COD vs DOC with Oak Street highlighted",xlab="log(DOC)",ylab="log(COD)")
lmFormula <- "logCOD ~ logDOC"
lmfit <- do.call("lm", list(lmFormula, data = DfFinal))
temp <- summary(lmfit)
rsquare <- round(temp$adj.r.squared,2)
abline(lmfit, col="red")
mtext(paste("log(COD) = ",round(coef(lmfit)[1],3)," + ",round(coef(lmfit)[2],3),"log(DOC) \nR-squared=",rsquare,sep=""), side = 1, line = -1.5, cex = 0.7)
legend("topleft",c("LK","CG","OAK","OUT"),pch=c(20,20,20,20),col=c("#999999","#999999","#0072B2","#999999"))
par(parOriginal)
plot(DfFinal$logDOC,DfFinal$logCOD,pch=20,col=DfFinal$SiteOUT,type="p",main="COD vs DOC with Outfall highlighted",xlab="log(DOC)",ylab="log(COD)")
lmFormula <- "logCOD ~ logDOC"
lmfit <- do.call("lm", list(lmFormula, data = DfFinal))
temp <- summary(lmfit)
rsquare <- round(temp$adj.r.squared,2)
abline(lmfit, col="red")
mtext(paste("log(COD) = ",round(coef(lmfit)[1],3)," + ",round(coef(lmfit)[2],3),"log(DOC) \nR-squared=",rsquare,sep=""), side = 1, line = -1.5, cex = 0.7)
legend("topleft",c("LK","CG","OAK","OUT"),pch=c(20,20,20,20),col=c("#999999","#999999","#999999","#CC79A7"))
par(parOriginal)
dev.off()

# save(DfFinal,file="CODDOCMerge.RData") file saved 10/27/15
