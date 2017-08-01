# script that uses output from absResiduals, and adds spectral slopes
# read in dataframe that has residuals calculated
# import processed data
absResiduals <- read.csv('cached_data/absResiduals.csv')
dataAbs <- read.csv('cached_data/correctedAbsData.csv')

# read in selected absorbence values
sag <- read.csv("raw_data/SagVals.csv",stringsAsFactors=FALSE)

waveCol <- "Wavelength"
colSubsetString <- "Gr"
dataSummary <-  absResiduals
grnum <- "GRnumber"

GMIASag <- getSag(dataAbs,waveCol,sag,colSubsetString,dataSummary,"GRnumber")

write.csv(GMIASag, "cached_data/absSlopesResiduals.csv", row.names = FALSE)
