# this script cleans up DOC data

doc.raw <- read.csv('raw_data/rawDOCdata.csv')

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
# 2) field replicates where two aliquots were collected at same site/time, 
# these have "-rep" or "-REP" added to the end. SOLUTION: take mean
# 3) sample reruns - where same water was run on a later date due to measurement errors/issues,
# these are likely to have same sample id but different date. SOLUTION: take lastest measurement, or 
# sample from file with latest date field

# 1) get rid of rows that are exactly the same across all columns
doc.unique <- unique(doc.cleaned)

# 2) identify field replicates and take mean of sample + rep
reps <- grep('rep|-R', doc.unique$ProjectID, ignore.case = TRUE)
rep.names <- doc.unique$ProjectID[grep('rep|-R', doc.unique$ProjectID, ignore.case = TRUE)]
new.names <- gsub('\\(rep\\)|-REP|-R', "", rep.names, ignore.case = FALSE)

doc.reps <- doc.unique[reps,]
doc.reps$NewNames <- new.names
doc.unique2 <- doc.unique[-reps, ]

for (i in 1:nrow(doc.unique2)){
  if (doc.unique2$ProjectID[i] %in% new.names) {
    doc.unique2$DOC[i] <- mean(doc.unique2$DOC[i], doc.reps$DOC[which(doc.reps$NewNames %in% doc.unique2$ProjectID[i])])
  }
}

# 3) now find duplicated ProjectIDs that have different date,
# use latest date

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
                          DOC = "",
                          DOC_dilution_corrected = "", 
                          Date = "", 
                          Date_formatted = "")
doc.unique3 <- doc.unique3[doc.unique3$nsamples != 0, ]
                          
for (i in 1:nrow(doc.unique3)) {
  
}
