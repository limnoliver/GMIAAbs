# script to test data imports against sample log
# this is to ensure we have all storm events
# as well as make sure we got the correct piece of data
# (e.g., if a sample was run twice, ensure that it came from the correct date file)

#################################
# check absorbance data
################################

# read in cleaned absorbance data
samples.compare <- read.csv('cached_data/cleanedAbsData.csv')

# filter to only date and ProjectID
samples.compare <- samples.compare[,c('date', 'ProjectID')]

# read in sample log and make it compatible for merge to samples.compare
sample.log <- read_xlsx('M:/NonPoint Evaluation/gmia/SLOH labforms and budget/optics.sample.log.ALL.xlsx')
names(sample.log)[2] <- 'ProjectID'
sample.log$ProjectID <- gsub('-', '.', sample.log$ProjectID)


comparison <- merge(sample.log[,c(2,7)], samples.compare, all.x = TRUE)

# for each row, decide whether the import occured ('missing'), 
# and whether it was from the right date file, e.g., correct ('correct') or not ('wrong date')
for (i in 1:nrow(comparison)){
  if (comparison$`Optics processed date`[i] %in% comparison$date[i]) {
    comparison$test[i] <- 'correct'
  } else if (comparison$`Optics processed date`[i] == 'No Sample' & is.na(comparison$date[i])) {
    comparison$test[i] <- 'correct'
  } else if (nchar(comparison$`Optics processed date`[i]) == 8 & is.na(comparison$date[i])) {
    comparison$test[i] <- 'missing'
  } else {
    comparison$test[i] <- 'wrong date'
  }
}

