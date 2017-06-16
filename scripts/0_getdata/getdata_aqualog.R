# source revised formatAbsSamples function
source("scripts/0_getdata/formatAbsSamplesRevised.R")

# collate data
FinalAbsDf <- formatAbsSamplesRevised(dateLower='20130930',dateUpper='20170113',Type='All',Project='GMIA')

# remove columns with NA names
FinalAbsDf <- FinalAbsDf[,names(FinalAbsDf) != "NA"]

# Filter out specific column names
finalcols <- colnames(FinalAbsDf)
finalcols <- finalcols[which(substr(finalcols,1,2) %in% c("OU","Ou","CG","LK","OA","Wa"))]
FinalAbsDf <- FinalAbsDf[,finalcols]

write.csv(FinalAbsDf2, "raw_data/rawCompiledAbs.csv", row.names = FALSE)
