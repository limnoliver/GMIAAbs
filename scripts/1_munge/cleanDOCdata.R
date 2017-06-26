# this script cleans up DOC data

doc.raw <- read.csv('raw_data/rawDOCdata.csv')

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
doc.unique2$ProjectID <- gsub("-", ".", doc.unique2$ProjectID)
dat.keep <- which(doc.unique2$ProjectID %in% sample.ids)
doc.unique2 <- doc.unique2[dat.keep, ]
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

# now find which date should be used for each sample
counts <- table(doc.unique2$ProjectID)
doc.unique3 <- data.frame(ProjectID = row.names(counts),
                          nsamples = as.numeric(counts),
                          DOC = NA,
                          DOC_dilution_corrected = NA, 
                          Date = NA, 
                          Date_formatted = NA)
doc.unique3 <- doc.unique3[doc.unique3$nsamples != 0, ]

# if there are multiple observations in the same date, take the mean
# if there are multiple observations across multiple dates, take the latest observation
# this may not be completely correct - Pete will verify based on file list I gave him. 
for (i in 1:nrow(doc.unique3)) {
 if (doc.unique3$nsamples[i] > 1) {
   temp <- doc.unique2[doc.unique2$ProjectID == doc.unique3$ProjectID[i], ]
   temp <- temp[which(temp$Date_formatted %in% max(as.numeric(as.character(temp$Date_formatted)))), ]
   if (nrow(temp) > 1){
     doc.unique3$ProjectID[i] <- temp$ProjectID[1]
     doc.unique3$DOC[i] <- mean(temp$DOC)
     doc.unique3$Date[i] <- paste('multiple readings for this sample in file', temp$Date[1], sep = " ")
     doc.unique3$Date_formatted[i] <- paste('multiple readings for this sample on date', temp$Date_formatted[1], sep  = " ")
     doc.unique3$DOC_dilution_corrected[i] <- temp$DOC_dilution_corrected[1]

     } else {
       doc.unique3[i,c(1,3,4,5,6)] <- temp[1,]
     }
 } else {
   temp <- doc.unique2[doc.unique2$ProjectID == doc.unique3$ProjectID[i], ]
   doc.unique3[i,c(1,3,4,5,6)] <- temp[1,]
 }
}
 
# export sample IDS and file name of places where there are duplicate samples for each ProjectID

multiple.entries <- doc.unique3[doc.unique3$nsamples > 1, ] 
multiple.entries <- multiple.entries[,c(1,2,5)]
multiple.entries$Date <- as.character(multiple.entries$Date)
for (i in 1:nrow(multiple.entries)) {
    temp <- doc.unique2[doc.unique2$ProjectID == multiple.entries$ProjectID[i], ]
    multiple.entries$Date[i] <-  paste(temp$Date, collapse = ", ")
}

write.csv(multiple.entries, "DOC_duplicate_entries.csv", row.names = FALSE)
  
