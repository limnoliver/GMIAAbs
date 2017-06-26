# script to merge WQ, DOC, and absorbance data

WQ <- read.csv('cached_data/cleanedWQdata.csv')
DOC <- read.csv('cached_data/cleanedDOCdata.csv')
Abs <- read.csv('cached_data/absSlopesResiduals.csv')
  
all.dat <- merge(Abs, DOC, 'ProjectID', all.x = TRUE)
all.dat <- merge(all.dat, WQ, 'ProjectID', all.x = TRUE)

write.csv(all.dat, "cached_data/mergedWQ_DOC_ABS.csv", row.names = FALSE)
