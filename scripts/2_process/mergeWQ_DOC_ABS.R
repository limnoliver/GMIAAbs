# script to merge WQ, DOC, and absorbance data

WQ <- read.csv('cached_data/cleanedWQdata.csv')
WQ$ProjectID <- toupper(WQ$ProjectID)
DOC <- read.csv('cached_data/cleanedDOCdata.csv')
DOC$ProjectID <- toupper(DOC$ProjectID)
Abs <- read.csv('cached_data/absSlopesResiduals.csv')
Abs2 <- read.csv('cached_data/tcorrectedAbsData.csv')
Abs <- merge(Abs, Abs2, 'GRnumber', all.x = TRUE)  
Abs$ProjectID <- toupper(Abs$ProjectID)
all.dat <- merge(Abs, DOC, 'ProjectID', all.x = TRUE)
all.dat <- merge(all.dat, WQ, 'ProjectID', all.x = TRUE)

write.csv(all.dat, "cached_data/mergedWQ_DOC_ABS.csv", row.names = FALSE)
