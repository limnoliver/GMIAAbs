# script to merge WQ, DOC, and absorbance data

WQ <- read.csv('cached_data/cleanedWQdata.csv', stringsAsFactors = FALSE)
WQ$ProjectID <- toupper(WQ$ProjectID)
# change Project ID OUT.S115ST to OUT.S115T - this is an error in the QW data file
WQ$ProjectID[WQ$ProjectID == "OUT.S115ST"] <- "OUT.S115T"
DOC <- read.csv('cached_data/cleanedDOCdata.csv')
DOC$ProjectID <- toupper(DOC$ProjectID)
Abs <- read.csv('cached_data/absSlopesResiduals.csv', stringsAsFactors = F)

Abs2 <- read.csv('cached_data/tcorrectedAbsData.csv')
Abs <- merge(Abs, Abs2, 'GRnumber', all.x = TRUE)  
Abs$ProjectID <- toupper(Abs$ProjectID)

# pull out rep data from absorbances
abs_reps <- grep('\\.R', Abs$ProjectID, value = TRUE)
abs_reps_matching <- gsub('\\.R', '', abs_reps)

abs_reps_dat <- dplyr::filter(Abs, ProjectID %in% c(abs_reps, abs_reps_matching))
# one rep is repeated, only keep rep with sample on same date
abs_reps_dat <- dplyr::filter(abs_reps_dat, !(ProjectID == 'OUT.S107.R' & datetime %in% '2014-02-25'))
# write lab reps and matching data
write.csv(abs_reps_dat,'cached_data/abs_labreps_matchingsamples.csv', row.names = FALSE)

Abs <- dplyr::filter(Abs, !grepl('\\.R', GRnumber))
all.dat <- merge(Abs, DOC, 'ProjectID', all.x = TRUE)
all.dat <- merge(all.dat, WQ, 'ProjectID', all.x = TRUE)

write.csv(all.dat, "cached_data/mergedWQ_DOC_ABS.csv", row.names = FALSE)
