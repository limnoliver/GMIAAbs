# this script filters and cleans wq data
# for eventual merging with absorbance data

wq_data <- read.csv('raw_data/rawWQdata.csv')

# keep only rows that have a project ID
wq_filtered <- wq_data %>%
  filter(!is.na(ProjectID))

# format ProjectID to look like project ID from absSlope.csv
wq_filtered$ProjectID <- gsub('-', '.', wq_filtered$ProjectID)

# filter which WQ variables are of interest

vars.keep <- grep("cod,|bod|formate|acetate|glycol|calcium|magnesium|potassium|sodium|NH3|ammonia|NO2|ethylene|propylene|methyl", names(wq_filtered), ignore.case = TRUE)
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

write.csv(wq_filtered, "cached_data/cleanedWQdata.csv", row.names = FALSE)
