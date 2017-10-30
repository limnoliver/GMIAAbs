# write table for supplement

responses <- c('COD', 'BOD', 'DOC', 'Propylene_glycol', 'Acetate', 'X4.Methyl.1H.Benzotriazole', 'X5.Methyl.1H.benzotriazole')

r2 <- c()
mse <- c()
top.vars <- c()

for (i in 1:length(responses)) {
  indices <- grep(responses[i], names(out))
  for (j in indices) {
    out_temp <- out[[j]]
    r2[1+length(r2)] <- out_temp[[4]]
    mse[1+length(mse)] <- out_temp[[7]]
    vars_in <- out_temp[[1]]$var.names[out_temp[[1]]$final_stand != 0]
    vars_in <- vars_in[-1]
    top.vars[1+length(top.vars)] <- paste(vars_in, collapse = ", ")
  }
}

supp.summary <- data.frame(r2 = round(r2, 2), mse = round(mse, 2), top.vars)
write.csv(supp.summary, 'figures/supplemental_models.csv', row.names = FALSE)
