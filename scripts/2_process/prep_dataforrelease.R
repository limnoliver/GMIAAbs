# script to get data in format for data release

df <- read.csv('cached_data/filteredWQ_DOC_ABS.csv')

df.rel <- df %>%
  select(ProjectID, DOC, rBOD:COD, r4.Methyl.1H.Benzotriazole:Acetate, rPropylene_glycol, 
         Propylene_glycol, Resids491:A239, Start_Date, End_Date) 

head(df.rel)

# get an output of MDLs for each absorbance metric, so that I can identify 
# censored values in the merged data frame. 

names(df.rel)
