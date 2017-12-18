# script to merge WQ, DOC, and absorbance data

WQ <- read.csv('cached_data/cleanedWQdata.csv', stringsAsFactors = FALSE)
WQ$ProjectID <- toupper(WQ$ProjectID)
# change Project ID OUT.S115ST to OUT.S115T - this is an error in the QW data file
WQ$ProjectID[WQ$ProjectID == "OUT.S115ST"] <- "OUT.S115T"
DOC <- read.csv('cached_data/cleanedDOCdata.csv')
DOC$ProjectID <- toupper(DOC$ProjectID)
Abs <- read.csv('cached_data/absSlopesResiduals.csv')
Abs2 <- read.csv('cached_data/tcorrectedAbsData.csv')
Abs <- merge(Abs, Abs2, 'GRnumber', all.x = TRUE)  
Abs$ProjectID <- toupper(Abs$ProjectID)
all.dat <- merge(Abs, DOC, 'ProjectID', all.x = TRUE)
all.dat <- merge(all.dat, WQ, 'ProjectID', all.x = TRUE)

write.csv(all.dat, "cached_data/mergedWQ_DOC_ABS.csv", row.names = FALSE)
