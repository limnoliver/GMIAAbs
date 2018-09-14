# clean up QW QAQC data from Troy
library(readxl)
library(dplyr)

wq.qa <- read_xlsx("raw_data/airport_qw_qaqc.xlsx")

grep("benzotriazole", wq.qa[[1]], ignore.case = T, value = T)

pcodes <- c("00310", "00335", "91080", "65240", "63120", "61944")
pcodes.r <- paste0("R", pcodes)
pcodes.p <- paste0("P", pcodes)
pcodes.all <- c(pcodes.r, pcodes.p)

chemicals <- c('BOD', "COD", "PG", "Acetate", "4M-1H-bzt", "5M-1H-bzt")

wq.qa <- read_xlsx("raw_data/airport_qw_qaqc.xlsx", skip = 89)
keep.cols <- names(wq.qa) %in% pcodes.all
keep.cols[1:6] <- TRUE
keep.cols[grep('SCMFL', names(wq.qa))] <- TRUE
summary(keep.cols)

wq.qa.f <- wq.qa[, keep.cols]
head(wq.qa.f)
wq.qa.f <- select(wq.qa.f, STAID:SAMPL, SCMFL, R00310:P00335, R91080, P91080, R65240, P65240, R63120:P61944)
names(wq.qa.f)[seq(from = 8, to = 18, by = 2)] <- paste0(chemicals, "_remark")
names(wq.qa.f)[seq(from = 9, to = 19, by = 2)] <- chemicals

# get rid of site COD
wq.qa.f <- wq.qa.f[-grep("COD", wq.qa.f$SCMFL),]

# write data
write.csv(wq.qa.f, "cached_data/QA_QWdata.csv")
