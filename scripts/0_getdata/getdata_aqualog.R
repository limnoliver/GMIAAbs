# source revised formatAbsSamples function
source("scripts/0_getdata/formatAbsSamplesRevised.R")

# collate data
FinalAbsDf <- formatAbsSamplesRevised(dateLower='20130930',dateUpper='20170113',Type='All',Project='GMIA')

# remove columns with NA names
FinalAbsDf2 <- FinalAbsDf[,names(FinalAbsDf) != "NA"]

write.csv(FinalAbsDf2, "raw_data/rawCompiledAbs.csv", row.names = FALSE)
