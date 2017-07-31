# This script modifies the formatAbsSamples function in the package USGSAqualogFormatting
# to deal with issues that JT described below:
# added lines 8 to 24 to deal with source file issues and directories
# added line 32 in testing
# added if in line 101 to exclude entries with na dilution
# changed filesNeeded into to include project information in output
# had to do some fooling around in function b/c of missing files in 20140121b
# also added Project ID to name so can differentiate OUT/LK/CG

library(readxl)

formatAbsSamplesRevised <- function(dateLower,dateUpper,Type,Project){
  
#   dateRangeFiles <- list.files(path='//igsarmewwshg9/HG9Data/AquaLog/AquaLog_Data')
#   XLfile <- grep('xlsx',dateRangeFiles)
#   dateRangeFiles <- dateRangeFiles[-c(XLfile)]
#   dateRangeFiles <- dateRangeFiles[which(dateRangeFiles <= dateUpper)]
#   dateRangeFiles <- dateRangeFiles[which(dateRangeFiles >= dateLower)]  
#   dateRangeFiles <- dateRangeFiles[which(dateRangeFiles!='2014')]
  
  dateRangeFilesb <- list.files(path='//igsarmewwshg9/HG9Data/AquaLog/AquaLog_Data/2013')
  dateRangeFilesb <- dateRangeFilesb[which(dateRangeFilesb <= dateUpper)]
  dateRangeFilesb <- dateRangeFilesb[which(dateRangeFilesb >= dateLower)]  
  dateRangeFilesb <- paste('2013/',dateRangeFilesb,sep="")
  
  dateRangeFilesc <- list.files(path='//igsarmewwshg9/HG9Data/AquaLog/AquaLog_Data/2014')
  dateRangeFilesc <- dateRangeFilesc[which(dateRangeFilesc <= dateUpper)]
  dateRangeFilesc <- dateRangeFilesc[which(dateRangeFilesc >= dateLower)]
  dateRangeFilesc <- dateRangeFilesc[which(nchar(dateRangeFilesc)<10)]
  dateRangeFilesc <- paste('2014/',dateRangeFilesc,sep="")
  
  dateRangeFilesd <- list.files(path='//igsarmewwshg9/HG9Data/AquaLog/AquaLog_Data/2015')
  dateRangeFilesd <- dateRangeFilesd[which(dateRangeFilesd <= dateUpper)]
  dateRangeFilesd <- dateRangeFilesd[which(dateRangeFilesd >= dateLower)]
  dateRangeFilesd <- dateRangeFilesd[which(nchar(dateRangeFilesd)<10)]
  dateRangeFilesd <- paste('2015/',dateRangeFilesd,sep="")
  
  dateRangeFilese <- list.files(path='//igsarmewwshg9/HG9Data/AquaLog/AquaLog_Data/2016')
  dateRangeFilese <- dateRangeFilese[which(dateRangeFilese <= dateUpper)]
  dateRangeFilese <- dateRangeFilese[which(dateRangeFilese >= dateLower)]
  dateRangeFilese <- dateRangeFilese[which(nchar(dateRangeFilese)<10)]
  dateRangeFilese <- paste('2016/',dateRangeFilese,sep="")
  
  dateRangeFilesf <- list.files(path='//igsarmewwshg8/HG8Data/Aqualog/AquaLog_Data/2017')
  dateRangeFilesf <- dateRangeFilesf[which(dateRangeFilesf <= dateUpper)]
  dateRangeFilesf <- dateRangeFilesf[which(dateRangeFilesf >= dateLower)]
  dateRangeFilesf <- dateRangeFilesf[which(nchar(dateRangeFilesf)<10)]
  #for some reason, there were 2016 files in 2017 folder that were also in 2016 folder
  #following line of code gets rid of anything without "2017" to start file name
  dateRangeFilesf <- dateRangeFilesf[grep("2017[[:digit:]]{4}",dateRangeFilesf)]
  dateRangeFilesf <- paste('2017/',dateRangeFilesf,sep="")
  
  dateRangeFiles <- dateRangeFilesb
  dateRangeFiles <- append(dateRangeFiles,dateRangeFilesc)
  dateRangeFiles <- append(dateRangeFiles,dateRangeFilesd)
  dateRangeFiles <- append(dateRangeFiles, dateRangeFilese)
  dateRangeFiles <- append(dateRangeFiles, dateRangeFilesf)
  
  #dateRangeFiles <- dateRangeFiles[-which(dateRangeFiles=='20150223')]
  #dateRangeFiles <- dateRangeFiles[-which(dateRangeFiles=='2014/20141219')]
  
  
  AbsList <- list()
  

  for(i in 1:length(dateRangeFiles)){
    if (length(grep("2017/", dateRangeFiles[i])) == 1) {
      fileName <- paste('//igsarmewwshg8/HG8Data/Aqualog/AquaLog_Data',dateRangeFiles[i],sep='/')
      cat(paste(i,fileName,'\n',sep="\n"))
    } else {
    fileName <- paste('//igsarmewwshg9/HG9Data/AquaLog/AquaLog_Data',dateRangeFiles[i],sep='/')
    cat(paste(i,fileName,'\n',sep="\n"))
    }
    setwd(fileName)
    
    allFiles <- list.files(path='.')
    
    DescriptionFile <- allFiles[grep('Sample Log',allFiles)]
    if (length(grep("\\$", DescriptionFile)) > 0){
      DescriptionFile <- DescriptionFile[-grep("\\$", DescriptionFile)]
    }

    if (length(DescriptionFile)==0|length(grep('Test', DescriptionFile))>0) {
      AbsList[[i]] <- NA
    } else {
      if (length(grep('.csv', DescriptionFile))>0){
      DescriptionFile <- read.csv(DescriptionFile[grep('.csv', DescriptionFile)],header=TRUE,stringsAsFactors=FALSE)
      DescriptionFile <- DescriptionFile[which(DescriptionFile[,1] !=''),]
      } else {
      filename <- DescriptionFile
      discard <- grep('\\D\\.xlsx', filename)
      if (length(discard)>0 & length(filename)>1) {
        filename = filename[-discard]
      }
      DescriptionFile <- read_xlsx(filename[grep('.xlsx', filename)],col_names=TRUE,na = c("", NA, "HH:MM:SS", "MM/DD/YYYY"))
      if (length(grep("X_", names(DescriptionFile)))>0) {
        DescriptionFile <- read_xlsx(filename[grep('.xlsx', filename)],col_names=TRUE, skip=10,na = c("", NA, "HH:MM:SS", "MM/DD/YYYY"))
      }
      
      if (all(is.na(DescriptionFile$ActivityStartDate))){
        DescriptionFile$ActivityStartDate <- as.character(DescriptionFile$ActivityStartDate)
        DescriptionFile$ActivityStartTime.Time <- as.character(DescriptionFile$ActivityStartTime.Time)
      } else {
        DescriptionFile$ActivityStartTime.Time <- as.character(strftime(DescriptionFile$ActivityStartTime.Time, '%H:%M:%S', tz = 'UTC'))
        DescriptionFile$ActivityStartDate <- as.character(format(DescriptionFile$ActivityStartDate, "%D"))
      }
      if (all(is.na(DescriptionFile$ActivityEndDate))){
        DescriptionFile$ActivityEndDate <- as.character(DescriptionFile$ActivityEndDate)
        DescriptionFile$ActivityEndTime.Time <- as.character(DescriptionFile$ActivityEndTime.Time)
      } else {
        DescriptionFile$ActivityEndTime.Time <- as.character(strftime(DescriptionFile$ActivityEndTime.Time, '%H:%M:%S', tz = 'UTC'))
        DescriptionFile$ActivityEndDate <- as.character(format(DescriptionFile$ActivityEndDate, "%D"))
      }
      DescriptionFile <- as.data.frame(DescriptionFile)
      start.col <- grep("field", names(DescriptionFile), ignore.case = TRUE)
      DescriptionFile <- DescriptionFile[,c(start.col:length(DescriptionFile))]
     
      }
    
      if (Project == ''){
        DescriptionFile <- DescriptionFile
      }else{
        #col.num <- grep('project', names(DescriptionFile), ignore.case = TRUE)
        DescriptionFile <- DescriptionFile[which(DescriptionFile[,9]==Project),]
        if(length(which(DescriptionFile[,9]==Project))<1) next}
      
      if(Type =='Environmental Samples'){
        exclude <- grep('lank',DescriptionFile[,1],ignore.case = TRUE)
        exclude <- c(exclude,grep('tandard',DescriptionFile[,1],ignore.case = TRUE))
        exclude <- c(exclude,grep('aseline',DescriptionFile[,1],ignore.case = TRUE))
        exclude <- c(exclude,grep('-R',DescriptionFile[,1],ignore.case = TRUE))
        exclude <- c(exclude,grep('-D',DescriptionFile[,1],ignore.case = TRUE))
        exclude <- c(exclude,grep('1',DescriptionFile[,2],ignore.case = TRUE))
        exclude <- c(exclude,grep('Tea',DescriptionFile[,1],ignore.case = TRUE))
        
        if(length(exclude)==0){
          DescriptionFile <- DescriptionFile}else{
            DescriptionFile <- DescriptionFile[-c(exclude),]
          }
      }
      # add a function that gets rid of samples labeled "1" in the "exclude" column
      # (if there is this column)
      if (length(grep('exclude', names(DescriptionFile), ignore.case = TRUE)) > 0) {
        col.num <- grep('exclude', names(DescriptionFile), ignore.case = TRUE)
        n.exclude <- length(which(DescriptionFile[,col.num] == 1))
        if (n.exclude > 0){
          DescriptionFile <- DescriptionFile[-which(DescriptionFile[,col.num] == 1), ]
        } else {
          DescriptionFile <- DescriptionFile
        }
      }
      
      if(Type == 'Blank'){
        n = grep('bla',DescriptionFile[,1],ignore.case = TRUE)
        DescriptionFile <- DescriptionFile[c(n),]} 
      
      if(Type == 'Tea Standard'){
        DescriptionFile <- DescriptionFile[which(DescriptionFile[,1] == '1% Tea Standard'),]}
      
      if(Type == 'Replicate'){
        r <- grep('-R',DescriptionFile[,1],ignore.case = TRUE)
        r <- c(r,grep('-D',DescriptionFile[,1],ignore.case = TRUE))
        DescriptionFile <- DescriptionFile[r,]}
      
      if(Type == 'All'){
        DescriptionFile <- DescriptionFile[which(DescriptionFile[,1] != 'Baseline'),]
      } 
      
      filesNeeded <- as.data.frame(DescriptionFile[,c(1,3)])
      if (sum(nchar(filesNeeded[,2]))>0) {
        filesNeed <- paste(filesNeeded[,2],'ABS.dat',sep='')
        dilution <- as.data.frame(DescriptionFile)[,4]
        names(dilution) <- filesNeed
        
        AbsfileNames <- filesNeed[which(filesNeed!='ABS.dat')]
        AbsStructure <- read.delim(paste(fileName,AbsfileNames[1],sep="/"), header=FALSE, stringsAsFactors=FALSE,row.names=NULL)
        AbsWavs <- AbsStructure[,1]
        
        
        AbsDf <- data.frame(matrix(numeric(), length(AbsWavs), length(AbsfileNames)), stringsAsFactors=FALSE)
        filesNeeded <- filesNeeded[nchar(filesNeeded[,2])>1,]
        colnames(AbsDf) <- paste(filesNeeded[,1],filesNeeded[,2],sep="_")
        colnames(AbsDf) <- paste(colnames(AbsDf),dateRangeFiles[i],sep='_')
        
        
        for (j in 1:length(AbsfileNames)){
          file <- paste(fileName,AbsfileNames[j],sep="/")
          name <- gsub('ABS.dat','',AbsfileNames[j])
          tempDf <- read.delim(file, header=FALSE, stringsAsFactors=FALSE,col.names=c('Wavelengths',name),row.names=NULL)
          tempDf <- tempDf[,-c(1)]
          if (!is.na(dilution[j])) {
            AbsDf[,j] <- tempDf*dilution[filesNeed[j]]
          } else {
            AbsDf[,j] <- tempDf
          }
        }
        AbsList[[i]] <- AbsDf
      }
    }
  }  
  
  AbsList[sapply(AbsList,is.null)] <- NULL
  FinalAbsDf <- do.call("cbind", AbsList)
  FinalAbsDf$Wavelength <- AbsWavs
  
  return(FinalAbsDf)
}
