# visualize variability in model results
# when randomly withholding 20% of data

library(Hmisc)
source('scripts/3_analyze/test_holdout.R')
library(devtools)
library(dplyr)

source_url('https://gist.github.com/kdauria/524eade46135f6348140')
#source('scripts/ana')
devtools::source_gist("524eade46135f6348140", filename = "ggplot_smooth_func.R")



# set responses
responses <- c('COD', 'BOD', 'DOC', 'Propylene_glycol', 'Acetate', 'X4.Methyl.1H.Benzotriazole', 'X5.Methyl.1H.benzotriazole')
responses.clean <- c('COD', 'BOD', 'DOC', 'Propylene glycol', 'Acetate', '4-Methyl-1H-benzotriazole', '5-Methyl-1H-benzotriazole')


#vars.keep <- list()
for (group in c("airport", "downstream")) {
  # subset airport or downstream sites
  out_sub <- out[grep(group, names(out))]
  
  # grab all alphas and lambdas from first run
  # create blank output dataframes
  output <- data.frame(matrix(NA, nrow = 7, ncol = 3))
  names(output) <- c('lambda', 'mse', 'r2')
  output2 <- data.frame(matrix(NA, nrow = 7, ncol = 6))
  names(output2) <- c('df_1se', 'df_2se', 'mse_1se', 'mse_2se', 'r2_1se', 'r2_2se')
  coefs_df_stand <- data.frame(matrix(NA, nrow = 15, ncol = 7))
  coefs_df_unstand <- data.frame(matrix(NA, nrow = 15, ncol = 7))
  supp.table <- data.frame(matrix(NA, nrow = 15, ncol = 7))
  
  var.counts <- data.frame(matrix(NA, nrow = 15, ncol = 7))
  pred_obs <- data.frame(matrix(NA, nrow = 1, ncol = 2))
  names(pred_obs) <- c('observed', 's0')
  
  for (i in 1:length(responses)){
    temp <- out_sub[[i]]
    response <- responses[i]
    # extract fit stats from first run
    #output[i, 'alpha'] <- temp[[1]][[1]]$alpha
    output[i, 'lambda'] <- temp[[10]]
    output[i, 'mse'] <- temp[[7]]
    output[i, 'r2'] <- temp[[4]]
    output2[i, ] <- c(mean(temp[[8]]), mean(temp[[9]]), round(mean(temp[[5]]), 2), round(mean(temp[[6]]),2), 
                      round(mean(temp[[2]]), 2), round(mean(temp[[3]]), 2))
    pred_obs <- rbind(pred_obs, temp[[11]])
    coefs_df_stand[,i] <- temp[[1]]$final_stand
    coefs_df_unstand[,i] <- temp[[1]]$final_unstand
    
    
    
    coefs.dot <- temp[[1]][,2:11]
    
    
    # create a column in coefs.dot that sum the number of instances of zero
    zeros <- function(x) {length(which(x == 0))} 
    zeros.holdout <- apply(coefs.dot, 1, zeros)
    var.counts[,i] <- zeros.holdout
    
    # create a supplement table that takes unstandardized coefficients and prints the number of zeros
    supp.table[,i] <- paste0(round(coefs_df_unstand[,i], 1), ' (', zeros.holdout, ')')
    
  }
  supp.table[16:21,] <- t(output2)
  names(supp.table) <- responses.clean 
  supp.table$variable <- c(as.character(temp[[1]]$var.names), names(output2))
  supp.table <- supp.table[,c(8, 1:7)]
  
  coef.table <- round(coefs_df_stand, 2)
  names(coef.table) <- responses.clean
  coef.table[16:18, ] <- t(round(output, 2))
  coef.table$variable <- c(as.character(temp[[1]]$var.names), names(output))
  coef.table <- coef.table[,c(8, 1:7)]
  
  # write table with group name to distinguish between airport and downstream
  write.csv(supp.table, file.path('cached_data', paste(group, 'mod_coefs_supp.csv', sep = "_")), row.names = F)
  write.csv(coef.table, file.path('cached_data', paste(group, 'mod_coefs.csv', sep = "_")), row.names = F)
}





library(Hmisc)


png(paste('figures/holdout_varimp_effect_', response, '.png', sep = ''), height = 500)
par(mar = c(5,5,1,1), oma = c(0,4,0,0))
dotchart2(varimps$imp, labels = coefs$var.names[2:15], dotsize = 2, xlim = c(-1,100), 
          xlab = "Variable Importance", bty= "L",width.factor = .2, col = rgb(100,100,100,alpha = 100, maxColorValue = 255))
for (i in 3:11) {
  dotchart2(varimps[,i], dotsize = 2, add = TRUE, col = rgb(100,100,100,alpha = 100, maxColorValue = 255))
}
dev.off()



