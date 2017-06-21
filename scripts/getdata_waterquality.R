library(readxl)

# read in latest wq file -- this reads in 2017 version
wq_data <- read_xlsx('M:/NonPoint Evaluation/gmia/WY2017 QW data/WY 2017 water-quality by site.xlsx', sheet = 'qw', skip = 1)

# write tab "qw" as csv
write.csv(wq_data, "raw_data/rawWQdata.csv", row.names = FALSE)


