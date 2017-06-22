# this script cleans up DOC data

doc.raw <- read.csv('raw_data/rawDOCdata.csv')

# identify QA rows from 'ProjectID' column
# these include blanks and standards
rows.exclude <- grep('blank*|ppm|unknown|standard*|std|dilution', doc.raw$ProjectID, ignore.case = TRUE)
doc.cleaned <- doc.raw[-rows.exclude, ]

# get rows with no info in them, either ProjectID or DOC
doc.cleaned <- doc.cleaned[!is.na(doc.cleaned$DOC), ]
doc.cleaned <- doc.cleaned[!is.na(doc.cleaned$ProjectID), ]

