# script to read in, adjust (due to dilution), and 
# format dye and deicer data for release
library(tidyr)
library(dplyr)

# source Steve's code to summarize blanks
source('scripts/2_process/fxn_absMRL.R')

# source Steve's code to correct absorbance values
source('scripts/2_process/fxn_absMRLAdjust.R')

# read in data
dyes <- read.csv('raw_data/DyeDilution.csv')
dyes.blanks <- read.csv('raw_data/DyeBlanks.csv')
deicers <- read.csv('raw_data/DeicerAbs.csv')
deicers_ids <- c('Group003GMIA0004', 'Group003GMIA0005')
location <- "//igsarmewvshome/igsarmew-genusr/HG8Data/Aqualog/AquaLog_Data/2017/20170127/"

# correct dyes, which were diluted to 5% dye, so correct by *20
# all dyes were originally dissolved from powder form to liquid to mimick
# a deicer formula of ~30mg/L of dye

# first, correct for exact concentration to get everything to 30 mg/L
# dyes[,'OrangeII5'] <- dyes[,'OrangeII5']*(30/(1.122/0.040))
# dyes[,'SunsetYellow5'] <- dyes[,'SunsetYellow5']*(30/(1.236/0.040))
# dyes[,'Tartrazine5'] <- dyes[,'Tartrazine5']*(30/(1.249/0.040))
# dyes[,'Erioglycine5'] <- dyes[,'Erioglycine5']*(30/(1.104/0.040))

# get type I deicer measured in 2017
all.files <- list.files(path = location)
file1 <- all.files[grep(paste0(deicers_ids[1], 'ABS.dat'), all.files)]
deicer_2017_1 <- read.table(paste(location, file1, sep = '/'), header = FALSE)
names(deicer_2017_1) <- c('Wavelength', 'CPP-I_')

# correct for dilution factor which is 20

deicer_2017_1$`CPP-I_` <- deicer_2017_1$`CPP-I_`*20

# get type IV deicer measured in 2017
file2 <- all.files[grep(paste0(deicers_ids[2], 'ABS.dat'), all.files)]
deicer_2017_2 <- read.table(paste(location, file2, sep = '/'), header = FALSE)
names(deicer_2017_2) <- c('Wavelength', 'CPGA-IV_')

# correct for dilution factor which is 50
deicer_2017_2$`CPGA-IV_` <- deicer_2017_2$`CPGA-IV_`*50

deicers <- merge(deicers, deicer_2017_1, all.x = TRUE)
deicers <- merge(deicers, deicer_2017_2, all.x = TRUE)

# Correct deicers for MDL
# read in raw absorbance data
abs.raw <- read.csv('raw_data/rawCompiledAbs.csv')

# find lab blanks for correction (not field blanks)
blankGRnums.deicers <- grep("20141216|20170127", names(abs.raw), value = T)
blankGRnums.deicers <- grep("blank", blankGRnums.deicers, value = T)

# calculate MRL based on all blank samples
MRL.deicers <- absMRL(abs.raw, "Wavelength", blankGRnums.deicers)

# correct deicer data
deicers.corrected <- absMRLAdjust(dfabs = deicers, dfMRLs = MRL.deicers, Wavelength = 'Wavelength', sampleGRnums = names(deicers)[-(1:2)], multiplier = 0.5)
deicers.corrected <- deicers.corrected[[1]]

# gather columns so this is in long format
deicers_long <- gather(deicers.corrected, key = brand, value = value, -Wavelength)

deicers_long$type <- NA
deicers_long$type[grep('I_', deicers_long$brand)] <- "Deicer - Type I"
deicers_long$type[grep('IV_|IV\\.', deicers_long$brand)] <- "Deicer - Type IV"
deicers_long$type[grep('sodium', deicers_long$brand, ignore.case = T)] <- "Deicer - Pavement (solid)"
deicers_long$type[grep('E36', deicers_long$brand, ignore.case = T)] <- "Deicer - Pavement (liquid)"

# create an id column that uses the brand coding in the figures
deicers_long$manufacturer_id <- NA
deicers_long$manufacturer_id[grep("ucar", deicers_long$brand, ignore.case = T)] <- "A_2014_F1"
deicers_long$manufacturer_id[grep("kilfrost", deicers_long$brand, ignore.case = T)] <- "B_2014_F1"
deicers_long$manufacturer_id[grep("cryotech", deicers_long$brand, ignore.case = T)] <- "C_2014_F1"
deicers_long$manufacturer_id[grep("CPP-I_|CPGA-IV_", deicers_long$brand, ignore.case = T)] <- "C_2017_F1"
deicers_long$manufacturer_id[grep("clair|clar", deicers_long$brand, ignore.case = T)] <- "D_2014_F1"
deicers_long$manufacturer_id[grep("sodium", deicers_long$brand, ignore.case = T)] <- "C_2014_F1"

# check that there is only brand name for each type-manufacturer_id
# get rid of Clairient.Maxflight.04.Type.IV_Group002GMIA0010_2014.20141216
# as this was redone and tagged with a "redo"

deicers_long <- filter(deicers_long, 
                       brand != "Clairient.Maxflight.04.Type.IV_Group002GMIA0010_2014.20141216")

# now distinguish between two different formulas for brand clariant type IV
deicers_long$manufacturer_id[grep("safewing", deicers_long$brand, ignore.case = T)] <- "D_2014_F1"
deicers_long$manufacturer_id[grep("maxflight", deicers_long$brand, ignore.case = T)] <- "D_2014_F2"

head(deicers_long)

test <- group_by(deicers_long, type, manufacturer_id) %>%
  summarize(n())

# Do MDL correction (will get rid of negative values) on Dyes
# in the same way we did for environmental samples
# Dyes were run by Pellerin lab so need to be corrected separately

# Do MDL correction (will get rid of negative values)
# in the same way we did for environmental samples

# use blanks to determine MDL
names(dyes.blanks)[15] <- "Wavelength"
MRL.all <- absMRL(dyes.blanks, "Wavelength", names(dyes.blanks)[-15])

# adjust values from MRL - here setting to 1/2 MRL
dyes.corrected <- absMRLAdjust(dfabs = dyes, dfMRLs = MRL.all, Wavelength = 'Wavelength', sampleGRnums = names(dyes)[-1], multiplier = 0.5)
dyes.corrected <- dyes.corrected[[1]]

# then, correct for dilution
dyes.corrected[,2:5] <- 20*dyes.corrected[,2:5]

# bring in and process dyes to create long data frame to add to deicers_long
names(dyes.corrected) <- c("Wavelength", "Orange II", "Sunset Yellow", "Tartrazine", "Erioglycine")
dyes_long <- gather(dyes.corrected, key = manufacturer_id, value = value, -Wavelength)
dyes_long$type <- "Dye"

dyes_deicers <- select(deicers_long, -brand) %>%
  bind_rows(dyes_long) %>%
  select(type, manufacturer_id, Wavelength, value) %>%
  rename(absorbance = value, id = manufacturer_id, wavelength = Wavelength)

write.csv(dyes_deicers, 'cached_data/dyes_deicers_for_sb.csv', row.names = F)

head(dyes_long)
