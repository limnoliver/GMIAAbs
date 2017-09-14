library(dplyr)
library(tidyr)
# script to get data in format for data release

df <- read.csv('cached_data/filteredWQ_DOC_ABS.csv')
df.abs.r <- read.csv('cached_data/censoredAbs.csv')

# make absorbance remarks data frame into long format
df.abs.r <- df.abs.r %>%
  gather(variable, value, -GRnumber)
names(df.abs.r)[3] <- 'censored'

# change censored = TRUE to '<'
df.abs.r$censored[df.abs.r$censored == TRUE] <- '<'
df.abs.r$censored[df.abs.r$censored == FALSE] <- NA

# find remarks columns for wq data
df.r <- df %>%
  select(GRnumber, rBOD, rCOD, r4.Methyl.1H.Benzotriazole, r5.Methyl.1H.benzotriazole, rAcetate,
         rPropylene_glycol)  %>%
  gather(variable, censored, -GRnumber)

df.r$variable <- gsub('^r', "", df.r$variable)

# pull data columns and make into long format
df.rel <- df %>%
  select(ProjectID, GRnumber, DOC, BOD, COD, X4.Methyl.1H.Benzotriazole, X5.Methyl.1H.benzotriazole, Acetate,
         Propylene_glycol, Resids491:A239, Start_Date, End_Date) 

df.rel <- df.rel %>%
  gather(variable, value, -ProjectID, -GRnumber, -Start_Date, -End_Date)

# merge remarks with values
df.all <- left_join(df.rel, df.abs.r)
df.all <- left_join(df.all, df.r)
head(df.rel)

# get a dataframe with just unique values of variables
# so then can fill in units

vars <- unique(df.all$variable)
units <- c('MilligramsPerLiter', 'MilligramsPerLiter', 'MilligramsPerLiter', 'MicrogramsPerLiter', 
           'MicrogramsPerLiter', 'MilligramsPerLiter', 'MilligramsPerLiter', rep(NA, 98))
units.df <- data.frame(variable = vars,
                    unit = units)

# merge units with data
df.all <- left_join(df.all, units.df)

df.all$site <- substr(df.all$ProjectID, 1, 2)

df.pub <- df.all %>%
  select(-GRnumber, -ProjectID) %>%
  rename(start_date = Start_Date, end_date = End_Date)

df.pub <- select(df.pub, site, start_date, end_date, variable, value, censored, unit)
df.pub$variable <- as.factor(df.pub$variable)
levels(df.pub$variable)[104:105] <- c('4_Methyl_1H_Benzotriazole', '5_Methyl_1H_Benzotriazole')

# drop all rows where value is NA

df.pub <- df.pub %>%
  filter(!is.na(value))

write.csv(df.pub, 'cached_data/airport_data_for_sb.csv', row.names = FALSE)
