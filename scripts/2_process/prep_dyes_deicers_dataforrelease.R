# script to read in, adjust (due to dilution), and 
# format dye and deicer data for release
library(tidyr)
library(dplyr)

# read in data
dyes <- read.csv('raw_data/DyeDilution.csv')
deicers <- read.csv('raw_data/DeicerAbs.csv')
deicers_ids <- c('Group003GMIA0004', 'Group003GMIA0005')
location <- "//igsarmewvshome/igsarmew-genusr/HG8Data/Aqualog/AquaLog_Data/2017/20170127/"

# correct dyes, which were diluted to 5% dye, so correct by *20

dyes[,2:5] <- 20*dyes[,2:5]

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

# gather columns so this is in long format
deicers_long <- select(deicers, -X) %>%
  gather(key = brand, value = value, -Wavelength)

deicers_long$type <- NA
deicers_long$type[grep('I_', deicers_long$brand)] <- "Deicer - TypeI"
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

# bring in and process dyes to create long data frame to add to deicers_long
names(dyes) <- c("Wavelength", "Orange II", "Sunset Yellow", "Tartrazine", "Erioglycine")
dyes_long <- gather(dyes, key = manufacturer_id, value = value, -Wavelength)
dyes_long$type <- "Dye"

dyes_deicers <- select(deicers_long, -brand) %>%
  bind_rows(dyes_long) %>%
  select(type, manufacturer_id, Wavelength, value) %>%
  rename(absorbance = value, id = manufacturer_id, wavelength = Wavelength)

write.csv(dyes_deicers, 'cached_data/dyes_deicers_for_sb.csv', row.names = F)

head(dyes_long)
