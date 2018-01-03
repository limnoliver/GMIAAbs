library(readxl)
library(dplyr)
# this script cleans up DOC data

doc.raw <- read.csv('raw_data/rawDOCdata.csv', stringsAsFactors = F)

# get sample ids from absorbance data to ID samples we want to keep
sample.ids <- read.csv('cached_data/tcorrectedAbsData.csv')
sample.ids <- sample.ids$ProjectID

# identify QA rows from 'ProjectID' column
# these include blanks and standards
rows.exclude <- grep('blank*|ppm|unknown|standard*|std|dilution', doc.raw$ProjectID, ignore.case = TRUE)
doc.cleaned <- doc.raw[-rows.exclude, ]

# get field blanks, which are denoted with "Q"
field.blanks <- grep("q", doc.raw$ProjectID, ignore.case = T, value = T)
field.blanks <- field.blanks[-grep("WQ", field.blanks)] # get rid of random water quality samples labeled "WQ"
field.blanks <- field.blanks[-grep("-R", field.blanks)] # get rid of field blank replicate
field.blanks <- field.blanks[-grep("PQ", field.blanks)] # get rid of OUT storm sample that is labeled "PG"
field.blanks <- doc.raw[doc.raw$ProjectID %in% field.blanks, ] # 


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
new.dates.fun <- function(date.vec){
  new.dates <- c()
for (i in 1:length(date.vec)){
  if (nchar(as.character(date.vec[i])) == 8 &
      length(grep('^201', date.vec[i])) == 1){
    new.dates[i] <- as.character(date.vec[i])
  } else if (nchar(as.character(date.vec[i])) == 8 &
             length(grep('^201', date.vec[i])) == 0) {
    new.dates[i] <- paste(substr(as.character(date.vec[i]), 5, 8), 
                          substr(as.character(date.vec[i]), 0, 2), 
                          substr(as.character(date.vec[i]), 3, 4), sep = "")
    
  } else if (nchar(as.character(date.vec[i])) == 6) {
    new.dates[i] <- paste('20', substr(as.character(date.vec[i]), 5, 6), 
                          substr(as.character(date.vec[i]), 0, 2), 
                          substr(as.character(date.vec[i]), 3, 4), sep = "")
  } else if (nchar(as.character(date.vec[i])) == 9) {
    new.dates[i] <- gsub('(\\d{8})(.)', '\\1', date.vec[i])
  }
} 
  return(new.dates)
}

new.dates.doc <- new.dates.fun(doc.unique2$Date)
doc.unique2$Date_formatted <- new.dates.doc
doc.unique2 <- doc.unique2[,-4]
doc.unique2 <- unique(doc.unique2)

# get new dates for doc qa data as well
new.dates.field.blanks <- new.dates.fun(field.blanks$Date)
field.blanks$Date_formatted <- new.dates.field.blanks

sample.log <- read_xlsx('M:/NonPoint Evaluation/gmia/SLOH labforms and budget/optics/optics.sample.log.ALL.xlsx')
sample.log <- as.data.frame(sample.log[,c(2,6)])
names(sample.log) <- c('ProjectID', 'KeepDate')
#sample.log$ProjectID <- gsub("-", ".", sample.log$ProjectID)

doc.unique3 <- merge(doc.unique2, sample.log, by = 'ProjectID', all.x = TRUE)
doc.unique3 <- filter(doc.unique3, !is.na(KeepDate))

for (i in 1:nrow(doc.unique3)){
  if (is.na(doc.unique3$Keep[i])){
    if (length(which(doc.unique3$ProjectID == doc.unique3$ProjectID[i])) == 1) {
      doc.unique3$Keep[i] = 1
    } else {
      if (doc.unique3$Date_formatted[i] %in% doc.unique3$KeepDate[i]){
        doc.unique3$Keep[i] = 1
      } else {
        doc.unique3$Keep[i] = 0
    }
  }
  }
}

doc.unique3 <- subset(doc.unique3, doc.unique3$Keep == 1)

doc.unique3 <- doc.unique3[,c('ProjectID', 'DOC', 'Date_formatted')]
doc.unique3$ProjectID <- gsub("-", ".", doc.unique3$ProjectID)

# manually get rid of US.S111G where value = 65
# this is a duplicate value that was not filtered out of 
# 20141202 file

doc.unique3 <- subset(doc.unique3, !(doc.unique3$ProjectID == "US.S111G" & doc.unique3$DOC == 65))

# create QA dataset as well
# keep only qa values from the same date as samples
sample.dates <- unique(doc.unique3$Date_formatted)
field.blanks <- filter(field.blanks, Date_formatted %in% sample.dates)
field.blanks <- select(field.blanks, ProjectID, DOC, Date_formatted)
field.blanks <- unique(field.blanks)

# summarize QA data
field.blanks.summary <- group_by(field.blanks, ProjectID) %>%
  summarize_at(vars(DOC), funs(mean, median, sd))

# write a qa dataset
write.csv(field.blanks, "cached_data/QA_DOCdata.csv", row.names = F)

# write a dataset of both qa and samples
field.blanks <- bind_rows(field.blanks, doc.unique3)
write.csv(field.blanks, 'cached_data/cleanedDOC_withQA.csv', row.names = FALSE)
write.csv(doc.unique3, 'cached_data/cleanedDOCdata.csv', row.names = FALSE)
