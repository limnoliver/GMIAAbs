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
  gather(key = type, value = value, -Wavelength)

deicers_long$id <- "Type I"
deicers_long$id[grep('IV_', deicers_long$type)] <- "Type IV"
deicers_long$id[grep('pavement', deicers_long$type, ignore.case = T)] <- "Pavement"



# plot four panels (type I, type IV, pavement, dyes)
type.iv <- grep('IV_', names(deicers))
type.i <- grep('I_', names(deicers))
pavement <- grep('pavement', names(deicers), ignore.case = TRUE)