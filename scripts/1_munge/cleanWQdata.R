# this script filters and cleans wq data
# for eventual merging with absorbance data
library(dplyr)
wq_data <- read.csv('raw_data/rawWQdata.csv', check.names = FALSE)

# project ID is hidden in unnamed column "X__1"
names(wq_data)[151] <- 'ProjectID'

# keep only rows that have a project ID
wq_filtered <- wq_data %>%
  filter(!is.na(ProjectID))

# format ProjectID to look like project ID from absSlope.csv
wq_filtered$ProjectID <- gsub('-', '.', wq_filtered$ProjectID)

# filter which WQ variables are of interest

vars.keep <- grep("cod,|bod|formate|acetate|glycol|potassium|sodium|ethylene|propylene|methyl", names(wq_filtered), ignore.case = TRUE)
vars.keep <- c(vars.keep, vars.keep-1)
vars.keep <- sort(vars.keep)

wq_filtered <- wq_filtered[,c(151, 1:4, vars.keep)]

# clean up names of columns
# including turning "remark" columns into 
# "rVariable" so they are identifiable by name

new.names <- names(wq_filtered)
# remove anything after first comma in wq var names
new.names <- gsub("(\\w),( .*)", "\\1",new.names)
# match remarks to variable names
for (i in 1:length(new.names)){
  if (length(grep("Remark", new.names[i]) == 1)) {
    new.names[i] <- paste("r", new.names[i+1], sep = "")
  }
}
# get rid of all spaces in column names, 
# turn into underscores
new.names <- gsub("\\s", "_", new.names)

# rename columns in wq dataframe
names(wq_filtered) <- new.names

# rename a set of 2015 files that have "COD-" ProjectIDs rather than the storm event/location name
# OUT-S113 through S115 were collected in glass bottles in the COD sampler, not regular sampler

missing.dat <- read.csv('cached_data/missingWQdata.csv')
missing.dat <- missing.dat[missing.dat$Sample.ID..notes == 'COD results exists', ]
sample.no <- missing.dat$record.no
replacement.projectid <- missing.dat$ProjectID
replacement.projectid <- gsub("-", ".", replacement.projectid)

replace.rows <- which(wq_filtered$Record_number %in% sample.no)
wq_filtered$ProjectID[replace.rows] <- as.character(replacement.projectid)

write.csv(wq_filtered, "cached_data/cleanedWQdata.csv", row.names = FALSE)
