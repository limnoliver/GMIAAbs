library(readxl)
library(dplyr)
years = c(2013:2016)

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
  files <- files[grep('^\\d{6,}', files)]
  
  temp.df <- tibble(ProjectID = as.character(), 
                        DOC = as.numeric(),
                        DOC_dilution_corrected = as.logical(),
                        Date = as.character())
  
  for (j in 1:length(files)){
    # read in file - need to consider that the sheet we're interested
    # in can either be the first or second sheet
    temp <- read_xlsx(paste(FilePath, files[j], sep = "/"), sheet = 1)
    if (length(grep('sample', names(temp)[1], ignore.case= TRUE)) == 0){
      temp <- read_xlsx(paste(FilePath, files[j], sep = "/"), sheet = 2)
    }
    # find out whether the file has been post-processed
    # which means that manual diluation was accounted for
    # not sure if this means that FALSE means that this info needs to be corrected,
    # or that files were not diluted. Check with Pete/Mari on this front
    if (length(grep('final', names(temp), ignore.case = TRUE))==0){
      temp$DOC <- as.numeric(unlist(temp[,grep('mean', names(temp), ignore.case = TRUE)]))
      temp$DOC_dilution_corrected <- FALSE
    } else {
      temp$DOC <- as.numeric(unlist(temp[,grep('final', names(temp), ignore.case = TRUE)]))
      temp$DOC_dilution_corrected <- TRUE
    }
    names(temp)[grep('sample', names(temp), ignore.case = TRUE)] <- "ProjectID"
    temp$Date <- gsub('(\\d{6,})(.xlsx)', '\\1', files[j])
    temp <- temp[,c('ProjectID', 'DOC', 'DOC_dilution_corrected', 'Date')]
    temp.df <- bind_rows(temp.df, temp)
  }
  doc.data[[i]] <- temp.df
}
