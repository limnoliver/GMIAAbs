# GMIAAbs
Absorbance data processing from GMIA deicer project. This code imports water quality, DOC, and absorbance data from sites near the MKE airport, merges these data by storm ID, and then performs LASSO modeling for variable selection.

## Cleaning and processing absorbance data
There are several steps required to clean and process the absorbance data. In order, these include: 
1. data import (`getdata_aqualog.R`)
2. transposing data (`transposeAbsData.R`), which takes the raw format (each storm has its own column) and makes it more user friendly (makes storm event or 'ProjectID' into a single column)
3. clean data (`cleanAbsData.R`) which cleans up the storm IDs for later merging with WQ and DOC data. Also determines the correct processing date and removes duplicate sampleids from the wrong date.
4. correct censored data (`correctAbs_MRL.R`) which calculates minimum reporting limits, and sets censored data to half the MRL. 
5. summarize absorbance data by site (`calcSummaryAbs.R`) which does not appear to be used at any point in modeling, so may not be necessary.
6. calculate absorbance residuals from linear regression using first order decay function (`absResiduals.R`)
7. calculate spectral slopes (`absSlopes.R`) 

## Modeling
The goal of the project is to predict the water quality endpoints of airport deicer in streams (high COD, BOD, eythylene, acetate, etc) using optical properties (absorbance data). Because we have a large number of predictors (~100) and relatively few data points (~200), we want to use a modeling routine that can also do variable selection. We have chosen to use a LASSO with k-folds cross validation. The modeling procedure, in the context of this repository, is as follows:
1. 
