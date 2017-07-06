# this script cleans data from
# formatAbsSamplesRevised function
# transposes data frame so that each wavelength becomes
# a column and GRnumbers are all in single column (samples by rows)

library(USGSHydroOpt)

source("scripts/0_getdata/getdata_aqualog.R")

rawAbs <- read.csv("raw_data/rawCompiledAbs.csv")

#clean up column names
testnames <- colnames(rawAbs)
testnames <- gsub("USGS","Group",testnames)
colnames(rawAbs) <- testnames

# get rid of last column name that is wavelengths
testnames <- testnames[-length(testnames)]

# make a data frame that will become summary table for getAbs function
test <- data.frame(testnames,stringsAsFactors=FALSE)

colnames(test) <- "GRnumber"
wavs <- unique(rawAbs$Wavelength)
wavs <- wavs[which(wavs<=700)]

testAbs <- getAbs(rawAbs,'Wavelength',wavs,"Group",test,"GRnumber")

# write data to cached data fikder
write.csv(testAbs, "cached_data/transposedAbsCoef.csv", row.names = FALSE)

###################################################


