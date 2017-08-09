# censored value imputation of the response variables
# preps responses for modeling

all.dat <- read.csv('cached_data/filteredWQ_DOC_ABS.csv', stringsAsFactors = FALSE)
responses <- c('COD', 'BOD', 'DOC', 'Propylene_glycol', 'Acetate', 'Sodium', 'X4.Methyl.1H.Benzotriazole', 'X5.Methyl.1H.benzotriazole', 
           'Ethylene_glycol', 'Formate')


#responses <- all.dat[,c('COD', 'BOD', 'DOC', 'Propylene_glycol', 'Acetate', 'Sodium', 'X4.Methyl.1H.Benzotriazole', 'X5.Methyl.1H.benzotriazole', 
                        'Ethylene_glycol', 'Formate')]
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
  y_nodls[[i]] <- y[c(-rows.left, -na.vals)]
  y[rows.left] <- NA
  y <- y[-na.vals]
  y <- log10(y)
  # impute values from distribution
  if (length(rows.left)>0){
  y <- impute.QRILC(as.data.frame(y))[[1]]
  y <- as.numeric(y[,1])
  }
  ys[[i]] <- y
}

means <- do.call(rbind, lapply(ys, function(x) round(mean(10^x), 0)))
sds <- do.call(rbind, lapply(ys, function(x) round(sd(10^x), 0)))
mins <- do.call(rbind, lapply(y_nodls, function(x) min(x, na.rm = TRUE)))
mins_censored <- do.call(rbind, lapply(ys, function(x) round(min(10^x), 1)))
maxs <- do.call(rbind, lapply(ys, function(x) round(max(10^x), 0)))

sum.table <- data.frame(response = responses, 
                        nobs_ncensored = paste(n.obs, ' (', n.cens, ')', sep = ''),
                        summ = )
