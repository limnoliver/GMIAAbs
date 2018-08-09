library(ggplot2)
library(dplyr)
library(stringr)

# get model output
out <- readRDS('cached_data/model_out.rds')

# pred vs obs plots 
responses.clean <- c('COD', 'BOD', 'DOC', 'Propylene glycol', 'Acetate', '4-Methyl-1H-benzotriazole', '5-Methyl-1H-benzotriazole')
main_labels <- rep(responses.clean, 2)
location_labels <- c(rep('Airport sites', 7), rep('Downstream sites', 7))

for (i in 1:length(out)) {
  dat <- out[[i]][[11]]
  
  vars <- as.character(out[[i]][[1]]$var.names)
  stand_coef <- out[[i]][[1]]$final_stand
  
  vars_coefs <- select(out[[i]][[1]], var.names, final_stand) %>%
    filter(!(var.names %in% 'intercept')) %>%
    filter(final_stand >0) %>%
    arrange(-final_stand)
  
  coef_labs <- paste0(vars_coefs$var.names, " (", round(vars_coefs$final_stand, 2), ')')
  
  p <- ggplot(dat, aes(y = observed, x = s0)) +
    geom_point() +
    theme_bw() +
    labs(x = "Predicted", y = "Observed", title = paste0(location_labels[i], ' - ', main_labels[i]), 
         subtitle = str_wrap(paste(coef_labs, collapse = ', '), width = 48)) + 
    geom_abline(intercept = 0, slope = 1)
  
  temp_name <- paste0("figures/Obs_vs_Pred_", names(out)[i], '.png')
  ggsave(temp_name, p, height = 5, width = 5)
  
  
}
