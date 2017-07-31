library(USGSHydroOpt)
source('/scripts/1_munge/transposeAbsData.R')
FinalAbsDf <- read.csv("raw_data/rawCompiledAbs.csv", header = TRUE)

# use the filtered abs data to define which columsn to keep
cleanAbs <- read.csv('cached_data/cleanedAbsData.csv')
col.keep <- unique(cleanAbs$GRnumber)
col.keep <- as.character(col.keep)
col.keep <- c(col.keep, 'Wavelength')

# change GRnumbers to match col.keep
testnames <- colnames(FinalAbsDf)
testnames <- gsub("USGS","Group",testnames)
colnames(FinalAbsDf) <- testnames
rawAbs.f <- FinalAbsDf[,col.keep]

# define groups by site
finalcolsOUT <- grep('out\\.', names(rawAbs.f), ignore.case = TRUE)
finalcolsCG <- grep('cg\\.', names(rawAbs.f), ignore.case = TRUE)
finalcolsLK <- grep('lk\\.', names(rawAbs.f), ignore.case = TRUE)
finalcolsOA <- grep('oak\\.', names(rawAbs.f), ignore.case = TRUE)
#finalcolsUS <- grep('us\\.', names(rawAbs.f), ignore.case = TRUE)
finalcolsALL <- c(finalcolsOUT, finalcolsCG, finalcolsLK, finalcolsOA)

FinalAbsDf$meanAbs <- rowMeans(FinalAbsDf[,finalcolsALL])
FinalAbsDf$meanAbsOUT <- rowMeans(FinalAbsDf[,finalcolsOUT])
FinalAbsDf$meanAbsCG <- rowMeans(FinalAbsDf[,finalcolsCG])
FinalAbsDf$meanAbsLK <- rowMeans(FinalAbsDf[,finalcolsLK])
FinalAbsDf$meanAbsOA <- rowMeans(FinalAbsDf[,finalcolsOA])
FinalAbsDf[FinalAbsDf<0] <- NA
FinalAbsDf$minAbs <- do.call(pmin,c(FinalAbsDf[,finalcolsALL],na.rm=TRUE))
FinalAbsDf[is.na(FinalAbsDf)] <- min(FinalAbsDf$minAbs)

write.csv(FinalAbsDf, "cached_data/SummarizedAbsData.csv", row.names = FALSE)

