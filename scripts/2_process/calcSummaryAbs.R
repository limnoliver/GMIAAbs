# this script creates summarizes absorbances by wavelength and site
# across all samples
# input required is each column as sample, with a column for wavelengths

AbsDf <- read.csv("cached_data/correctedAbsData.csv", header = TRUE)

# define groups by site
finalcolsOUT <- grep('out\\.', names(AbsDf), ignore.case = TRUE)
finalcolsCG <- grep('cg\\.', names(AbsDf), ignore.case = TRUE)
finalcolsLK <- grep('lk\\.', names(AbsDf), ignore.case = TRUE)
finalcolsOA <- grep('oak\\.', names(AbsDf), ignore.case = TRUE)
#finalcolsUS <- grep('us\\.', names(AbsDf), ignore.case = TRUE)
finalcolsALL <- c(finalcolsOUT, finalcolsCG, finalcolsLK, finalcolsOA)

AbsDf$meanAbs <- rowMeans(AbsDf[,finalcolsALL])
AbsDf$meanAbsOUT <- rowMeans(AbsDf[,finalcolsOUT])
AbsDf$meanAbsCG <- rowMeans(AbsDf[,finalcolsCG])
AbsDf$meanAbsLK <- rowMeans(AbsDf[,finalcolsLK])
AbsDf$meanAbsOA <- rowMeans(AbsDf[,finalcolsOA])
AbsDf$minAbs <- do.call(pmin,c(AbsDf[,finalcolsALL],na.rm=TRUE))
#AbsDf[is.na(AbsDf)] <- min(AbsDf$minAbs)

write.csv(AbsDf, "cached_data/SummarizedAbsData.csv", row.names = FALSE)

