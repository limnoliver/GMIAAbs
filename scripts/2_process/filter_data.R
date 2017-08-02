# filter data for modeling
# this includes getting rid of upstream and summer events
# also includes filtering out wavelengths over 500 (not a lot of information, many values < MDL)

all.dat <- read.csv('cached_data/mergedWQ_DOC_ABS.csv', stringsAsFactors = FALSE)

# ID upstream events
sites.remove <- grep('US|LK|OAK', all.dat$ProjectID)

# get rid of US events
filt.dat <- all.dat[-sites.remove, ]

# ID summer events
month <- substr(filt.dat$date, start = 5, stop = 6)
summer.rows <- grep('05|06|07|08|09|10', month)

# get rid of summer events
filt.dat <- filt.dat[-summer.rows,]

# get rid of WL > 500 as WL > 500 have 
# high proportion (>30%) of observations that are censored
# (< MDL) -- see Abs_prop_censored.png

start.abs <- grep('A800', names(filt.dat))
end.abs <- grep('A503', names(filt.dat))
filt.dat <- filt.dat[,-(start.abs:end.abs)]



write.csv(filt.dat, 'cached_data/filteredWQ_DOC_ABS.csv', row.names = FALSE)

