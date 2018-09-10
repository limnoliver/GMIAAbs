# transpose absorbance lab replicates to be useful for Pete

dat <- read.csv('cached_data/abs_labreps_matchingsamples.csv', stringsAsFactors = FALSE)

dat_reps <- dplyr::filter(dat, grepl('\\.R', ProjectID)) %>% select(-GRnumber, -date)
dat_samples <- dplyr::filter(dat, !grepl('\\.R', ProjectID)) %>% select(-date)

dat_reps_long <- tidyr::gather(dat_reps, key = 'variable', value = 'rep_value', -ProjectID, -datetime)
dat_reps_long <- dplyr::mutate(dat_reps_long, ProjectID = gsub('\\.R', '', ProjectID))
dat_samples_long <- tidyr::gather(dat_samples, key = 'variable', value = 'sample_value', -GRnumber, -ProjectID, -datetime)

dat_out <- dplyr::left_join(dat_reps_long, dat_samples_long, by = c('ProjectID', 'datetime', 'variable')) %>%
  select(ProjectID, datetime, variable, sample_value, rep_value, GRnumber) %>%
  arrange(ProjectID, variable)

write.csv(dat_out, 'cached_data/abs_labreps_matchingsamples_long.csv', row.names = F)
