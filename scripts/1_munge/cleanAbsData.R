library(readxl)
library(dplyr)
# read in transposed absorbance data where each wavelength has own column
abscoef <- read.csv('cached_data/transposedAbsCoef.csv', header = TRUE, stringsAsFactors = F)

temp <- as.character(abscoef$GRnumber)

# extract date from GRnumber
abscoef$date <- gsub('(^.+)(_[[:digit:]]{4}\\.)([[:digit:]]{8})', '\\3', temp)
abscoef$datetime <- as.Date(abscoef$date, format = "%Y%m%d")
#abscoef$datetime <- strptime(abscoef$date, format = "%Y%m%d")

# extract site or sample id (e.g., blanks, standards, OUT.W103)
abscoef$ProjectID <- gsub('(^.+)([[:punct:]]+Group.+)', '\\1', temp)

# keep a running list of QA samples
samples.qa <- grep("\\.R|blank|standard|Q", abscoef$ProjectID, ignore.case = TRUE, value = TRUE)
samples.qa <- grep("Q", samples.qa, ignore.case = TRUE, value = TRUE) #narrow to QA samples that are field blanks
samples.qa <- samples.qa[-grep("^Q$", samples.qa, ignore.case = T)] # get rid of MQ water blanks
samples.qa <- samples.qa[-grep("PQ", samples.qa, ignore.case = T)] # get rid of storm sample with "Q" in name
samples.qa <- samples.qa[-grep("\\.R", samples.qa, ignore.case = T)] # get rid of field blank reps


# reduce to sites of interest for GMIA
samples.keep <- grep("out\\.|cg\\.|lk\\.|us\\.|oak\\.", abscoef$ProjectID, ignore.case = TRUE, value = TRUE)

# get rid of replicates/redos
samples.keep <- samples.keep[-grep('\\.R|redo', samples.keep, ignore.case = TRUE)]

# get rid of quality control samples
samples.keep <- samples.keep[-grep('^Q', samples.keep)]

# get rid of duplicated or diluted samples
samples.keep <- samples.keep[-grep('dil|dup',samples.keep, ignore.case = TRUE)]

# get rid of random samples that made it through that have long names - 
# think these were runs of the deicers themselves
samples.keep <- samples.keep[-which(nchar(samples.keep)>12)]

# get rid fo samples that have "Q" after the dash or decimal
samples.keep <- samples.keep[-grep('\\.Q', samples.keep)]

# filter abscoef
abscoef.f <- abscoef[abscoef$ProjectID %in% samples.keep, ] 

# find all unique project ids
unique.PID <- unique(abscoef.f$ProjectID)

# some duplicate Project IDs left - pull in optics sample log to verify we are using the correct date for each sample

sample.log <- read_xlsx('M:/NonPoint Evaluation/gmia/SLOH labforms and budget/optics/optics.sample.log.ALL.xlsx')
names(sample.log)[2] <- 'ProjectID'
sample.log$ProjectID <- gsub('-', '.', sample.log$ProjectID)


# create a column to populate with decision to remove or not based 
# on wrong date assessment
abscoef.f$remove <- FALSE

for (i in 1:nrow(abscoef.f)){
  if (length(grep(paste('^',abscoef.f$ProjectID[i],'$', sep = ""), sample.log$ProjectID, ignore.case = TRUE)) == 0) {
    abscoef.f$remove[i] = FALSE
  } else {
    row.num <- grep(paste('^',abscoef.f$ProjectID[i],'$', sep = ""), sample.log$ProjectID, ignore.case = TRUE)
    log <- sample.log[row.num, ]
    if (length(grep(paste('^',abscoef.f$ProjectID[i],'$', sep = ""), abscoef.f$ProjectID)) > 1 & abscoef.f$date[i] != log$`Optics processed date`){
      abscoef.f$remove[i] = TRUE
  }
 
  }
}

# filter out duplicated samples with the wrong date
abscoef.f <- abscoef.f[abscoef.f$remove == FALSE, ]

write.csv(abscoef.f,'cached_data/cleanedAbsData.csv', row.names = FALSE)

# clean up QA data and export
qa <- abscoef[abscoef$ProjectID %in% samples.qa, ]

# write QA data
write.csv(qa, "cached_data/QA_Absdata.csv", row.names = F)

# add qa samples to final data
abscoef.f <- abscoef.f[,-193]
abscoef.f.qa <- bind_rows(qa, abscoef.f)

# write full dataset with QA
write.csv(abscoef.f,'cached_data/cleanedAbsData_withQA.csv', row.names = FALSE)


