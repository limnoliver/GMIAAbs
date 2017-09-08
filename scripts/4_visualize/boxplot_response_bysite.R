# script to create boxplots of response variables by site
# this is just to show there is not a strong signal in 
# sites outside of outfall and cargo

# get merged data

all.dat <- read.csv('cached_data/mergedWQ_DOC_ABS.csv', stringsAsFactors = FALSE)

# ID summer events
month <- substr(all.dat$date, start = 5, stop = 6)
summer.rows <- grep('05|06|07|08|09|10', month)

# get rid of summer events
filt.dat <- all.dat[-summer.rows,]

# summarize by site
filt.dat$site <- substr(filt.dat$ProjectID, start = 1, stop = 2)
filt.dat$site <- as.factor(filt.dat$site)
levels(filt.dat$site) <- c("CG", "LK", "OAK", "OUT", "UP")
# create boxplot

responses <- c('COD', 'BOD', 'DOC', 'Propylene_glycol', 'Acetate', 'X4.Methyl.1H.Benzotriazole', 'X5.Methyl.1H.benzotriazole')
responses.clean <- c('COD', 'BOD', 'DOC', 'PG', 'Acetate', '4-M-1H-Bzt', '5-M-1H-Bzt')

png('figures/response_by_site.png')
layout(matrix(1:8, nrow = 4, ncol = 2))
par(mar=c(4,5,1,1.5))
for (i in 1:length(responses)){
  
  boxplot(log10(filt.dat[,responses[i]])~filt.dat$site,
          ylab = paste0('log ', responses.clean[i]), cex.lab = 1.4, cex.axis = 1.2)
  
  
}
dev.off()
