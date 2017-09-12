# filter data for modeling
# this includes getting rid of upstream and summer events
# also includes filtering out wavelengths over 500 (not a lot of information, many values < MDL)

all.dat <- read.csv('cached_data/mergedWQ_DOC_ABS.csv', stringsAsFactors = FALSE)

# ID upstream events
sites.remove <- grep('US|LK|OAK', all.dat$ProjectID)

# get rid of US events
filt.dat <- all.dat[-sites.remove, ]

# get rid of data that has no response vars
incomplete <- which(is.na(filt.dat$COD)&is.na(filt.dat$BOD)&is.na(filt.dat$Propylene_glycol)&is.na(filt.dat$DOC))
filt.dat <- filt.dat[-incomplete, ]

# ID summer events
month <- substr(filt.dat$Start_Date, start = 6, stop = 7)
summer.rows <- grep('05|06|07|08|09|10', month)

# get rid of summer events
filt.dat <- filt.dat[-summer.rows,]

# get rid of WL > 500 as WL > 500 have 
# high proportion (>30%) of observations that are censored
# (< MDL) -- see Abs_prop_censored.png

start.abs <- grep('A800', names(filt.dat))
end.abs <- grep('A503', names(filt.dat))
filt.dat <- filt.dat[,-(start.abs:end.abs)]

# get rid of slopes and resideuals that were calculated using
# absorbance at WL > 500

out <- c('Sag506_530', 'Sag530_551', 'Sag551_611', 'Sag611_650', 'Resids629')
out.cols <- which(names(filt.dat) %in% out)

filt.dat <- filt.dat[,-out.cols]

write.csv(filt.dat, 'cached_data/filteredWQ_DOC_ABS.csv', row.names = FALSE)

