# read in transposed absorbance data where each wavelength has own column
abscoef <- read.csv('cached_data/transposedAbsCoef.csv', header = TRUE)

# read in raw data to see imported data
abs.raw <- read.csv('raw_data/rawCompiledAbs.csv')

temp <- as.character(abscoef$GRnumber)

# extract date from GRnumber
abscoef$date <- gsub('(^.+)(_[[:digit:]]{4}\\.)([[:digit:]]{8})', '\\3', temp)
abscoef$datetime <- strptime(abscoef$date, format = "%Y%m%d")

# extract site or sample id (e.g., blanks, standards, OUT.W103)
abscoef$ProjectID <- gsub('(^.+)([[:punct:]]+Group.+)', '\\1', temp)


# reduce to sites of interest for GMIA
samples.keep <- grep("out\\.|cg\\.|lk\\.|us\\.|oak\\.", abscoef$ProjectID, ignore.case = TRUE, value = TRUE)

# get rid of replicates/redos
samples.keep <- samples.keep[-grep('\\.R|redo', samples.keep, ignore.case = TRUE)]

# get rid of quality control samples
samples.keep <- samples.keep[-grep('^Q', samples.keep)]

# get rid of duplicated or diluted samples
samples.keep <- samples.keep[-grep('dil|dup',samples.keep, ignore.case = TRUE)]

# get rid of random samples that made it through that have long names - think these were runs of the deicers themselves
samples.keep <- samples.keep[-which(nchar(samples.keep)>12)]

# some duplicate Project IDs left - pull in optics sample log to verify we are using the correct date for each sample
samples.compare <- abscoef[which(abscoef$ProjectID %in% samples.keep), ]
sample.log <- read_xlsx('M:/NonPoint Evaluation/gmia/SLOH labforms and budget/optics.sample.log.ALL.xlsx')
samples.compare <- samples.compare[,c('date', 'ProjectID')]
names(sample.log)[2] <- 'ProjectID'
sample.log$ProjectID <- gsub('-', '.', sample.log$ProjectID)

comparison <- merge(sample.log[,c(2,7)], samples.compare, all.x = TRUE)

for (i in 1:nrow(comparison)){
  if (comparison$`Optics processed date`[i] %in% comparison$date[i]) {
    comparison$test[i] <- 'correct'
  } else if (comparison$`Optics processed date`[i] == 'No Sample' & is.na(comparison$date[i])) {
    comparison$test[i] <- 'correct'
  } else if (nchar(comparison$`Optics processed date`[i]) == 8 & is.na(comparison$date[i])) {
    comparison$test[i] <- 'missing'
  } else {
    comparison$test[i] <- 'wrong date'
  }
}
# find site-specific duplicates
abscoefOAK <- abscoef[grep("OAK",abscoef$ProjectID),]
# remove duplicate sample for OAK site
abscoefOAK <- abscoefOAK[which(paste(abscoefOAK$ProjectID,abscoefOAK$date,sep="")!="OAK.S10720140225"),]

abscoefOUT <- abscoef[substr(abscoef$ProjectID,1,2) %in% c("OU","Ou"),]
abscoefOUT <- abscoefOUT[-grep(".R",abscoefOUT$ProjectID),]
# remove duplicate samples for OUT site
abscoefOUT <- abscoefOUT[which(paste(abscoefOUT$ProjectID,abscoefOUT$date,sep="")!="OUT.S10720140225"),]
abscoefOUT <- abscoefOUT[which(paste(abscoefOUT$ProjectID,abscoefOUT$date,sep="")!="OUT.S107G20140225"),]
abscoefOUT <- abscoefOUT[which(substr(abscoefOUT$GRnumber,1,18)!="OUT.S110G_Group003"),]
abscoefOUT <- abscoefOUT[which(abscoefOUT$ProjectID!="OUT.S118-D"),]
abscoefOUT <- abscoefOUT[which(substr(abscoefOUT$GRnumber,1,10)!="OUT.S114D_"),]
abscoefOUT <- abscoefOUT[which(substr(abscoefOUT$GRnumber,1,10)!="OUT.S114E_"),]
abscoefOUT <- abscoefOUT[which(substr(abscoefOUT$GRnumber,1,10)!="OUT.S114F_"),]
abscoefOUT <- abscoefOUT[which(substr(abscoefOUT$GRnumber,1,10)!="OUT.S114G_"),]
abscoefOUT <- abscoefOUT[which(substr(abscoefOUT$GRnumber,1,10)!="OUT.S114H_"),]
abscoefOUT <- abscoefOUT[which(substr(abscoefOUT$GRnumber,1,10)!="OUT.S114J_"),]
abscoefOUT <- abscoefOUT[which(substr(abscoefOUT$GRnumber,1,10)!="OUT.S114K_"),]
abscoefOUT$ProjectID <- gsub('Out','OUT',abscoefOUT$ProjectID)

abscoefCG <- abscoef[grep("CG.",abscoef$ProjectID),]
# remove duplicate and QC samples for CG site
abscoefCG <- abscoefCG[which(paste(abscoefCG$ProjectID,abscoefCG$date,sep="")!="CG.S10720140225"),]
abscoefCG <- abscoefCG[which(abscoefCG$ProjectID!="CG.S116B"),]
abscoefCG <- abscoefCG[which(abscoefCG$ProjectID!="CG.Q23C"),]

abscoefLK <- abscoef[grep("LK.",abscoef$ProjectID),]
# remove duplicate and QC samples for LK site

#############
# Fix? Not sure this part is doing what it's supposed to
############
abscoefLK <- abscoefLK[which(abscoefLK$ProjectID!="LK.Q23C"),]
abscoefLK <- abscoefLK[-grep(".R",abscoefLK$ProjectID),]
abscoefLK <- abscoefLK[which(paste(abscoefLK$ProjectID,abscoefLK$date,sep="")!="LK.S10720140225"),]
abscoefLK <- abscoefLK[which(paste(abscoefLK$ProjectID,abscoefLK$date,sep="")!="LK.S107G20140225"),]

abscoefWorking <- rbind(abscoefOUT,abscoefCG)
abscoefWorking <- rbind(abscoefWorking,abscoefLK)
abscoefWorking <- rbind(abscoefWorking,abscoefOAK)
# after merging sites of interest, remove any remaining QC samples
abscoefWorking <- abscoefWorking[-which(substr(abscoefWorking$GRnumber,1,1)=='Q'),]
# limit samples to Nov-April deicing period
abscoefWorking <- abscoefWorking[-which(abscoefWorking$ProjectID %in% c("OUT.S110","OUT.S110G","OUT.S117","OUT.S118","CG.S110","CG.S117","CG.S118","LK.S110","LK.S110G","LK.S117","LK.S118","OAK.S110","OAK.S118")),]

write.csv(abscoefWorking,'cached_data/AbsData.csv', row.names = FALSE)