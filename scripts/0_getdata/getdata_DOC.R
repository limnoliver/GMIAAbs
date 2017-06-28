library(readxl)
library(dplyr)
# set years of data you want to retrieve
years = c(2013:2017)

# i = years
# j = files (dates) within year folders

doc.data <- list()
for (i in 1:length(years)){
  # identify files in each year folder that we want to keep
  # some files have "lenaker" or something else attached to them
  # just use files with dates and no text
  FilePath <- paste('//Igsarmewwsscu/SCUData/TOC_REPORTS/GMIA_Corsi-Lenaker',years[i],sep='/')
  files <- list.files(FilePath)
  files <- files[grep('.xlsx', files)]
  files <- files[grep('^\\d{5,}', files)]
  
  temp.df <- tibble(ProjectID = as.character(), 
                        DOC = as.numeric(),
                        DOC_dilution_corrected = as.logical(),
                        Date = as.character(), 
                        Keep = as.integer())
  
  for (j in 1:length(files)){
    # read in file - need to consider that the sheet we're interested
    # in can either be the first or second sheet
    temp <- read_xlsx(paste(FilePath, files[j], sep = "/"), sheet = 1)
    if (length(grep('sample', names(temp)[1], ignore.case= TRUE)) == 0){
      temp <- read_xlsx(paste(FilePath, files[j], sep = "/"), sheet = 2)
    }
    # prior to 2017, use log to find which files should be used for which ProjectID
    # 2017 and on, use the column "samplekeep" to define whether each observation should be 
    # used or not. This should solve "rerun" duplicate samples.
    if (length(grep('final', names(temp), ignore.case = TRUE))==0){
      temp$DOC <- as.numeric(unlist(temp[,grep('mean', names(temp), ignore.case = TRUE)]))
      temp$DOC_dilution_corrected <- FALSE
    } else {
      temp$DOC <- as.numeric(unlist(temp[,grep('final', names(temp), ignore.case = TRUE)]))
      temp$DOC_dilution_corrected <- TRUE
    }
    if (years[i]>2016){
      names(temp)[grep('keep', names(temp), ignore.case = TRUE)] <- 'Keep'
    } else {
      temp$Keep <- NA
    }
    names(temp)[grep('sample', names(temp), ignore.case = TRUE)] <- "ProjectID"
    temp$Date <- gsub('(\\d{5,}.)(\\.xlsx)', '\\1', files[j])
    
    temp <- temp[,c('ProjectID', 'DOC', 'DOC_dilution_corrected', 'Date', 'Keep')]
   
    temp.df <- bind_rows(temp.df, temp)
    
  }
  doc.data[[i]] <- temp.df
}
# now combine all year dataframes (currently in list in doc.data)
# to one data frame
doc.data <- bind_rows(doc.data)

# save data frame
write.csv(doc.data, 'raw_data/rawDOCdata.csv', row.names = FALSE)

