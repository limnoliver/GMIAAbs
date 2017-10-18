# this script cleans up DOC data

doc.raw <- read.csv('raw_data/rawDOCdata.csv', stringsAsFactors = F)

# get sample ids from absorbance data to ID samples we want to keep
sample.ids <- read.csv('cached_data/absSlopesResiduals.csv')
sample.ids <- sample.ids$ProjectID

# identify QA rows from 'ProjectID' column
# these include blanks and standards
rows.exclude <- grep('blank*|ppm|unknown|standard*|std|dilution', doc.raw$ProjectID, ignore.case = TRUE)
doc.cleaned <- doc.raw[-rows.exclude, ]

# get rid of rows with no info in them, either ProjectID or DOC
doc.cleaned <- doc.cleaned[!is.na(doc.cleaned$DOC), ]
doc.cleaned <- doc.cleaned[!is.na(doc.cleaned$ProjectID), ]

#########################################
# get rid of duplicate information
# several reasons why a sample ID might be repeated
# 1) machine replicates that measure the same aliquot of water, and the means of these are exported for
# every sample ID - sample ID exactly the same. SOLUTION: take one of the values, or mean (same answer)
# 2) analytical replicates
# these have "-rep" or "-REP" added to the end. SOLUTION: drop values -- will use later for QA
# 3) sample reruns - where same water was run on a later date due to measurement errors/issues,
# these are likely to have same sample id but different date. SOLUTION: take lastest measurement, or 
# sample from file with latest date field -- Pete noted this won't always be the case, so will likely
# have to find case-by-case solution. 

# 1) get rid of rows that are exactly the same across all columns
doc.unique <- unique(doc.cleaned)

# 2) identify field replicates and drop them
reps <- grep('rep|-R', doc.unique$ProjectID, ignore.case = TRUE)
doc.unique2 <- doc.unique[-reps, ]


# 3) now find duplicated ProjectIDs that have different date,
# use latest date

# to avoid processing samples that we won't keep,
# get rid of rows that do not have a project ID that is shared by absorbance data
#doc.unique2$ProjectID <- gsub("-", ".", doc.unique2$ProjectID)
#dat.keep <- which(doc.unique2$ProjectID %in% sample.ids)
#doc.unique2 <- doc.unique2[dat.keep, ]
# first, have to format dates properly
new.dates <- c()
for (i in 1:length(doc.unique2$Date)){
  if (nchar(as.character(doc.unique2$Date[i])) == 8 &
      length(grep('^201', doc.unique2$Date[i])) == 1){
    new.dates[i] <- as.character(doc.unique2$Date[i])
  } else if (nchar(as.character(doc.unique2$Date[i])) == 8 &
             length(grep('^201', doc.unique2$Date[i])) == 0) {
    new.dates[i] <- paste(substr(as.character(doc.unique2$Date[i]), 5, 8), 
                          substr(as.character(doc.unique2$Date[i]), 0, 2), 
                          substr(as.character(doc.unique2$Date[i]), 3, 4), sep = "")
    
  } else if (nchar(as.character(doc.unique2$Date[i])) == 6) {
    new.dates[i] <- paste('20', substr(as.character(doc.unique2$Date[i]), 5, 6), 
                          substr(as.character(doc.unique2$Date[i]), 0, 2), 
                          substr(as.character(doc.unique2$Date[i]), 3, 4), sep = "")
  } else if (nchar(as.character(doc.unique2$Date[i])) == 9) {
    new.dates[i] <- gsub('(\\d{8})(.)', '\\1', doc.unique2$Date[i])
  }
} 

doc.unique2$Date_formatted <- new.dates
doc.unique2 <- doc.unique2[,-4]
doc.unique2 <- unique(doc.unique2)

sample.log <- read_xlsx('M:/NonPoint Evaluation/gmia/SLOH labforms and budget/optics.sample.log.ALL.xlsx')
sample.log <- as.data.frame(sample.log[,c(2,6)])
names(sample.log) <- c('ProjectID', 'KeepDate')
#sample.log$ProjectID <- gsub("-", ".", sample.log$ProjectID)

doc.unique3 <- merge(doc.unique2, sample.log, by = 'ProjectID', all.x = TRUE)
doc.unique3 <- filter(doc.unique3, !is.na(KeepDate))

for (i in 1:nrow(doc.unique3)){
  if (is.na(doc.unique3$Keep[i])){
    if (doc.unique3$Date_formatted[i] %in% doc.unique3$KeepDate[i]){
      doc.unique3$Keep[i] = 1
    } else {
      doc.unique3$Keep[i] = 0
    }
  }
}

doc.unique3 <- subset(doc.unique3, doc.unique3$Keep == 1)

doc.unique3 <- doc.unique3[,c('ProjectID', 'DOC', 'Date_formatted')]
doc.unique3$ProjectID <- gsub("-", ".", doc.unique3$ProjectID)
 
write.csv(doc.unique3, 'cached_data/cleanedDOCdata.csv', row.names = FALSE)
