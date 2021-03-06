# this code finds the minimum reporting limit from field blanks
# and corrects absorbances for MRLs

# source Steve's code to summarize blanks
source('scripts/2_process/fxn_absMRL.R')

# source Steve's code to correct absorbance values
source('scripts/2_process/fxn_absMRLAdjust.R')

# read in raw absorbance data
abs.raw <- read.csv('raw_data/rawCompiledAbs.csv')

# calculate MRL based on all blank samples
blankGRnums.all <- grep('^Q[[:alpha:]]', names(abs.raw), value = TRUE)
MRL.all <- absMRL(abs.raw, "Wavelength", blankGRnums.all)

# read in cleaned absorbance data
abs.cleaned <- read.csv("cached_data/cleanedAbsData.csv")
GRnums <- as.character(abs.cleaned$GRnumber)
Wavelength <- grep("A", names(abs.cleaned), value = TRUE)
Wavelength.num <- gsub("A", "", Wavelength)
storms <- abs.cleaned[,c('GRnumber', 'date', 'datetime', 'ProjectID')]

abs.t.cleaned <- as.data.frame(t(abs.cleaned[,2:(ncol(abs.cleaned)-4)]))
names(abs.t.cleaned) <- GRnums
abs.t.cleaned$Wavelength <- Wavelength.num
wl.column <- grep('Wavelength', names(abs.t.cleaned))
abs.t.cleaned <- abs.t.cleaned[,c(wl.column, 1:(wl.column-1))]

# adjust values from MRL - here setting to 1/2 MRL
# function outputs two dataframes - one with adjusted values, the other with "<" for all corrected values
abs.corrected <- absMRLAdjust(dfabs = abs.t.cleaned, dfMRLs = MRL.all, Wavelength = 'Wavelength', sampleGRnums = GRnums, multiplier = 0.5)

abs.censored <- abs.corrected[[2]]
# find out how many censored values we have for each wavelength to determine which wavelengths we should use
# turn "<" into NAs
# maybe add this to function in the future
for (i in 2:length(abs.censored)){
  abs.censored[,i] <- as.numeric(as.character(abs.censored[,i]))
}

# now create dataframe of 'censored' values to release with data
test <- abs.censored[,-1]

test[!is.na(test)] <- 'FALSE'
test[is.na(test)] <- 'TRUE'


test <- as.data.frame(t(test))
names(test) <- paste('A', abs.censored[,1], sep = "")
test$GRnumber <- row.names(test)

names(test)

# count NAs per row (# of samples censored per wavelength)
count_na <- function(x) sum(is.na(x))
abs.censored$n_censored <- apply(abs.censored, 1, count_na)
abs.censored$prop_censored <- abs.censored$n_censored/(length(abs.censored)-1)

abs.corrected.t <- as.data.frame(t(abs.corrected[[1]][,-grep('Wavelength', names(abs.corrected[[1]]))]))
names(abs.corrected.t) <- Wavelength
abs.corrected.t$GRnumber <- row.names(abs.corrected.t)
abs.corrected.t <- merge(abs.corrected.t, storms, by = 'GRnumber', all.x = TRUE)

write.csv(abs.corrected[[1]],'cached_data/correctedAbsData.csv',row.names = FALSE)
write.csv(abs.corrected.t, 'cached_data/tcorrectedAbsData.csv', row.names = FALSE)
write.csv(test, 'cached_data/censoredAbs.csv', row.names = FALSE)

png('figures/Abs_prop_censored.png')
plot(prop_censored~Wavelength, data = abs.censored, ylab = "Proportion of Samples < MDL", cex.lab = 1.3)
dev.off()

#######################################################
# test if blanks by site makes a difference
# that is, is there contamination at any sites?

blankGRnums.OUT <- grep('QOUT', names(abs.raw), value = TRUE)
blankGRnums.LK <- grep('QLK', names(abs.raw), value = TRUE)
blankGRnums.CG <- grep('QCG', names(abs.raw), value = TRUE)
blankGRnums.US <- grep('QUS', names(abs.raw), value = TRUE)

data.list <- list()
data.list[[1]] <- absMRL(abs.raw, "Wavelength", blankGRnums.all)
data.list[[2]] <- absMRL(abs.raw, "Wavelength", blankGRnums.OUT)
data.list[[3]] <- absMRL(abs.raw, "Wavelength", blankGRnums.LK)
data.list[[4]] <- absMRL(abs.raw, "Wavelength", blankGRnums.CG)
data.list[[5]] <- absMRL(abs.raw, "Wavelength", blankGRnums.US)



# visualize data for major differences
absMRL.viz <- function(data.list, var){
plot(data.list[[1]][, var]~data.list[[1]][, 'Wavelength'], type = "l")
points(data.list[[2]][, var]~data.list[[2]][, 'Wavelength'], type = "l", col = "red")
points(data.list[[3]][, var]~data.list[[3]][, 'Wavelength'], type = "l", col = "blue")
points(data.list[[4]][, var]~data.list[[4]][, 'Wavelength'], type = "l", col = "green")
points(data.list[[5]][, var]~data.list[[5]][, 'Wavelength'], type = "l", col = "purple")
legend("topright", legend = c("All", "OUT", "LK", "CG", "US"), 
       col = c('black', 'red', 'blue', 'green', 'purple'), lty = 1, title = paste(var," of Blank", sep = ""))
}
data.list <- list()
data.list[[1]]<- test.all
data.list[[2]] <- test.OUT
data.list[[3]] <- test.LK
data.list[[4]] <- test.CG
data.list[[5]] <- test.US

par(mfrow=c(1,1))

absMRL.viz(data.list = data.list, var = "mean")

