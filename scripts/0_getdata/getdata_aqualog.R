# source revised formatAbsSamples function
source("scripts/0_getdata/formatAbsSamplesRevised.R")

# set home wd
wd.home <- getwd()
# collate data
FinalAbsDf <- formatAbsSamplesRevised(dateLower='20130930',dateUpper='20170610',Type='All',Project='GMIA')

# remove columns with NA names
FinalAbsDf <- FinalAbsDf[,names(FinalAbsDf) != "NA"]

setwd(wd.home)
write.csv(FinalAbsDf, "raw_data/rawCompiledAbs.csv", row.names = FALSE)
