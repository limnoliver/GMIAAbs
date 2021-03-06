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
1. filter merged data (`filter_data.R`) to only include relevant sites (cargo and outfall), relevant events (remove summer/non-deicer period events), and relevant predictor variables (remove absorbance at wavelengths > 500 due to high number of censored values)
2. log-transform predictor variables that do not include zeros or negative values
3. get rid of highly correlated indpendent variables using `caret`'s `findCorrelation` function (r = 0.99)
4. run lasso using `glmnet` within `caret`, which repeats the k-folds (5) cross-validation 10 times, and optimizes both alpha and lambda (versus `cvglmnet` which only optimizes lambda). This function also centers and scales predictor variables, but returns coefficients in their original scale.
5. for dependent variables that have censored values, repeat this process setting censored values to DL, 0.5xDL, and 0.5xmin(non-censored values)
