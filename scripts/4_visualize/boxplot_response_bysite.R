# script to create boxplots of response variables by site
# this is just to show there is not a strong signal in 
# sites outside of outfall and cargo

library(dplyr)
library(tidyr)
library(scales)
library(ggplot2)
# get merged data

all.dat <- read.csv('cached_data/filteredWQ_DOC_ABS_allsites.csv', stringsAsFactors = FALSE)
filt.dat <- all.dat

# how many unique storms are represented?
head(all.dat)
all.dat[grep('S115', all.dat$ProjectID), ]
storms <- all.dat$ProjectID
storms2 <- gsub("([[:upper:]]{2,3}\\.)([[:upper:]]{1}\\d{3})(.*)", "\\2", storms)
length(unique(storms2))

# how many surface water samples?
# so sort of storms x site
length(unique(all.dat$ProjectID))


# summarize by site
filt.dat$site <- substr(filt.dat$ProjectID, start = 1, stop = 2)
filt.dat$site <- as.factor(filt.dat$site)
levels(filt.dat$site) <- c("CG", "LK", "OAK", "OUT", "UP")
# create boxplot

responses <- c('COD', 'BOD', 'DOC', 'Propylene_glycol', 'Acetate', 'X4.Methyl.1H.Benzotriazole', 'X5.Methyl.1H.benzotriazole')
responses.clean <- c('COD', 'BOD', 'DOC', 'PG', 'Acetate', '4-M-1H-Bzt', '5-M-1H-Bzt')

# create a long table

filt.dat.long <- filt.dat %>%
  select(responses, site, ProjectID)

names(filt.dat.long) <- c(responses.clean, 'site', 'ProjectID')

filt.dat.long <- filt.dat.long %>%
  gather(variable, value, -site, -ProjectID) %>%
  filter(!is.na(value))

# now do the same thing for censored vals
# function to caculate number of censored values

r.responses <- paste('r', responses, sep = '')
r.responses <- gsub('rX', 'r', r.responses)

# add dummy rDOC column
filt.dat$rDOC <- NA

filt.dat.long.cen <- filt.dat %>%
  select(r.responses[1:2], rDOC, r.responses[3:7], site, ProjectID)

names(filt.dat.long.cen) <- c(responses.clean, 'site', 'ProjectID')

filt.dat.long.cen <- filt.dat.long.cen %>%
  gather(variable, value_cen, -site, -ProjectID)

filt.dat.long <- left_join(filt.dat.long, filt.dat.long.cen, by = c('site', 'ProjectID', 'variable'))

filt.dat.long <- mutate(filt.dat.long, censored = ifelse(value_cen %in% "<", 1, 0))

levels(filt.dat.long$site)
filt.dat.long$site = factor(filt.dat.long$site, levels(filt.dat.long$site)[c(1,4,3,2,5)])
filt.dat.long$variable = as.factor(filt.dat.long$variable)
filt.dat.long$variable = factor(filt.dat.long$variable, levels(filt.dat.long$variable)[c(5,4,6,7,3,1,2)])
# ggplot box plot with numbers

n_fun <- function(x){
  return(data.frame(y = ifelse(max(x) > 2, max(x) + (.08*max(x)), max(x) + (.25*max(x))), label = length(x)))
}

# change site names to include units for facet labels
mu.symbol <- '\U00B5'
levels(filt.dat.long$variable) <- paste(levels(filt.dat.long$variable), c(rep('(mg/L)', 5), rep(paste0("(",  mu.symbol, "g/L)"), 2)))

n.cens <- filt.dat.long %>%
  group_by(variable, site) %>%
  summarize(n.cens = sum(censored), n = length(value), y = ifelse(max(log10(value)) > 2.2, max(log10(value)) + (.08*max(log10(value))), max(log10(value)) + (.25*max(log10(value))))) %>%
  mutate(label = ifelse(n.cens > 0, paste0(n, ' (', n.cens, ')'), paste0(n)))


p <- ggplot(filt.dat.long, aes(site, log10(value))) +
  geom_boxplot(position = position_dodge2(preserve = "total")) + 
  facet_wrap(~variable, scales = 'free_y', ncol = 4) +
  geom_text(data = n.cens, aes(x = site, y = y, label = label)) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        panel.border = element_rect(colour = "black"),
        axis.text = element_text(size = 12), 
        axis.title = element_text(size = 14),
        strip.text = element_text(size = 12)) +
  labs(x = '', y = 'Concentration') +
  scale_y_continuous(labels = scales::math_format(expr = 10^.x, format = force))

ggsave('figures/response_by_site.png', p, height = 5, width = 12)


