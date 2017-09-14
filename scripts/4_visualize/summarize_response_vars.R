# censored value imputation of the response variables
# preps responses for modeling

source('scripts/2_process/fxn_imputeQRILC.R')
all.dat <- read.csv('cached_data/filteredWQ_DOC_ABS.csv', stringsAsFactors = FALSE)
responses <- c('COD', 'BOD', 'DOC', 'Propylene_glycol', 'Acetate', 'X4.Methyl.1H.Benzotriazole', 'X5.Methyl.1H.benzotriazole')
responses.clean <- c('COD', 'BOD', 'DOC', 'Propylene glycol', 'Acetate', '4-Methyl-1H-benzotriazole', '5-Methyl-1H-benzotriazole')


r.responses <- paste('r', responses, sep = '')
r.responses <- gsub('rX', 'r', r.responses)

do.call(cbind, lapply(all.dat[,responses]))
# add dummy rDOC column
all.dat$rDOC <- NA
n.obs <- do.call(rbind, lapply(all.dat[,responses], function (x) length(which(!is.na(x)))))
n.cens <- do.call(rbind, lapply(all.dat[,r.responses], function (x) length(which(x == '<'))))

y_nodls <- list()
ys <- list()

for (i in 1:length(responses)){
  y <- all.dat[, responses[i]]
  rows.left <- which(all.dat[,r.responses[i]]=="<")
  na.vals <- which(is.na(y))
  discard <- c(rows.left, na.vals)
  if (length(discard)>0){
    y_nodls[[i]] <- y[-discard]
    
  }
  y[rows.left] <- NA
  
  if (length(na.vals)>0){
    y <- y[-na.vals]
    
  }
  
  y <- log10(y)
  # impute values from distribution
  if (length(rows.left)>0){
  y <- impute.QRILC(as.data.frame(y))[[1]]
  y <- as.numeric(y[,1])
  }
  ys[[i]] <- y
}

medians <- do.call(rbind, lapply(ys, function(x) round(median(10^x), 0)))
tenth <- do.call(rbind, lapply(ys, function(x) round(quantile(10^x, 0.1), 0)))
ninetieth <- do.call(rbind, lapply(ys, function(x) round(quantile(10^x, 0.9), 0)))

sum.table <- data.frame(response = responses.clean, 
                        nobs_ncensored = paste(n.obs, ' (', n.cens, ')', sep = ''),
                        summary = paste0(medians, ' (', tenth, ', ', ninetieth, ')'))

write.csv(sum.table, 'figures/response_summary_table.csv', row.names = FALSE)
