# visualize variability in model results
# when randomly withholding 20% of data

library(Hmisc)
source('scripts/3_analyze/test_holdout.R')
library(devtools)
source_url('https://gist.github.com/kdauria/524eade46135f6348140')

devtools::source_gist("524eade46135f6348140", filename = "ggplot_smooth_func.R")

# grab all alphas and lambdas from first run
output <- data.frame(matrix(NA, nrow = 8, ncol = 6))
names(output) <- c('alpha', 'lambda', 'rmse', 'r2.cv', 'r2.train', 'r2.test')
coefs <- data.frame(matrix(NA, nrow = 15, ncol = 8))
vars.keep <- list()

for (i in 1:length(responses)){
  temp <- out[[i]]
  response <- responses[i]
  # extract fit stats from first run
  output[i, 'alpha'] <- temp[[1]][[1]]$alpha
  output[i, 'lambda'] <- temp[[1]][[1]]$lambda
  output[i, 'rmse'] <- temp[[5]][1]
  output[i, 'r2.cv'] <- temp[[4]][1]
    
  train.mod <- lm(temp[[7]][[1]]$train.obs ~ temp[[7]][[1]]$train.pred)
  output[i, 'r2.train'] <- round(summary(train.mod)$r.squared, 2)
  
  test.mod <- lm(temp[[6]][[1]]$hold.obs ~ temp[[6]][[1]]$hold.pred)
  output[i, 'r2.test'] <- round(summary(test.mod)$r.squared, 2)
  
  # create output figs
  train.dat <- do.call(rbind, temp[[7]])
  train.dat$iteration <- rep(1:10, each = nrow(temp[[7]][[1]]))
  temp.name <- paste('figures/holdout_train_', responses[i], '.png', sep = '')
  temp.x <- paste('log Predicted ', responses.clean[i], sep = '')
  temp.y <- paste('log Observed ', responses.clean[i], sep = '')
  
  r2 <- function(y,x) {round(summary(lm(y~x))$r.squared,3)}
  
  r2.train <- train.dat %>%
    group_by(iteration) %>%
    summarise(r2.train = r2(train.obs,train.pred))

  train.dat <- merge(train.dat, r2.train, by = 'iteration')
  ggsave(temp.name, 
  ggplot(train.dat, aes(x = train.pred, y = train.obs)) +
    geom_point(size = 2, shape = 16, color = 'darkgray') +
    geom_smooth(method = 'lm', formula = y~x) +
    facet_wrap(~ r2.train, scales = 'fixed', nrow = 2) +
    #geom_text(data = r2.text, aes(label = labels)) +
    theme_bw() +
    theme(strip.background = element_blank(), 
          panel.grid.major = element_blank(), 
          strip.text = element_text(vjust = -10), 
          strip.placement = 'outside') +
    labs(x = temp.x, y = temp.y), 
  width = 8, height = 4, dpi = 1200)
  
  # test dat
  test.dat <- do.call(rbind, temp[[6]])
  test.dat$iteration <- rep(1:10, each = nrow(temp[[6]][[1]]))
  temp.name <- paste('figures/holdout_test_', responses[i], '.png', sep = '')
  r2.test <- test.dat %>%
    group_by(iteration) %>%
    summarise(r2.test = r2(hold.obs,hold.pred))
  
  test.dat <- merge(test.dat, r2.test, by = 'iteration')
  ggsave(temp.name, 
         ggplot(test.dat, aes(x = hold.pred, y = hold.obs)) +
           geom_point(size = 2, shape = 16, color = 'darkgray') +
           geom_smooth(method = 'lm', formula = y~x) +
           facet_wrap(~ r2.test, scales = 'fixed', nrow = 2) +
           #geom_text(data = r2.text, aes(label = labels)) +
           theme_bw() +
           theme(strip.background = element_blank(), 
                 panel.grid.major = element_blank(), 
                 strip.text = element_text(vjust = -10), 
                 strip.placement = 'outside') +
           labs(x = temp.x, y = temp.y), 
         width = 8, height = 4, dpi = 1200)
  
  # create dotplots
  coefs.dot <- temp[[3]]
  varimps <- temp[[2]]
  temp.name <- paste('figures/holdout_coefs.dot_dotplot_', responses[i], '.png', sep = '')
  png(temp.name, height = 500, width = 500)
  par(mar = c(5,5,1,1), oma = c(0,4,0,0), mfrow=c(1,2))
  dotchart2(coefs.dot$coef[2:15], labels = coefs.dot$var.names[2:15], dotsize = 2, xlim = c(min(coefs.dot[2:15, 2:11]),max(coefs.dot[2:15, 2:11])), 
            xlab = "Coefficients", bty= "L",width.factor = .2, col = rgb(100,100,100,alpha = 100, maxColorValue = 255))
  for (j in 3:11) {
    dotchart2(coefs.dot[2:15,j], dotsize = 2, add = TRUE, col = rgb(100,100,100,alpha = 100, maxColorValue = 255))
  }
  dotchart2(varimps$imp, labels = '', dotsize = 2, xlim = c(-3,100), 
            xlab = "Variable Importance", bty= "L",width.factor = .2, col = rgb(100,100,100,alpha = 100, maxColorValue = 255))
  for (j in 3:11) {
    dotchart2(varimps[,j], dotsize = 2, add = TRUE, col = rgb(100,100,100,alpha = 100, maxColorValue = 255))
  }
  dev.off()
  
  # create a column in coefs.dot that sum the number of instances of zero
  zeros <- function(x) {length(which(x == 0))} 
  zeros.holdout <- apply(coefs.dot[,2:9], 1, zeros)
  vars.keep[[i]] <- droplevels(coefs.dot$var.names[zeros.holdout == 0])
  
  # create table of coefficients by
  # extracting coefficient estimates from 
  if (i == 1){
    coefs[,i] <- temp[[3]]$var.names
    names(coefs)[1] <- 'Predictor Variables'
  }
  coefs[,i+1] <- temp[[3]]$coef
  names(coefs)[i+1] <- responses.clean[i]
  
  # now itiratively remove vars and run linear model
  # plot R2, RMSE by # of vars
  zeros.holdout <- zeros.holdout[-1]
  remove <- order(zeros.holdout, decreasing = TRUE) +1
  file.names <- list.files('cached_data')
  
  # read in ready-to-model data
  temp.search <- paste(response, '_model_dat', sep = '')
  temp.files <- file.names[grep(temp.search, file.names)]
  temp.loc <- paste('cached_data/', temp.files, sep = '')
  temp.dat <- read.csv(temp.loc)
  
  mod.full <- summary(lm(y~., temp.dat))
  mod.nvars <- data.frame(rse = mod.full$sigma, 
                          r2 = mod.full$adj.r.squared, 
                          nvars = 14)
  for (j in 1:(length(remove)-1)){
    remove.cols <- remove[1:j]
    temp.dat.r <- temp.dat[, -remove.cols]
    
    mod.part <- summary(lm(y~., temp.dat.r))
    nvar <- 14-j
    rem <- as.character(names(temp.dat[remove[j]]))
    mod.nvars[j+1, 1:3] <- c(mod.part$sigma, mod.part$adj.r.squared, nvar)

  }
  
  mod.nvars$var_drop[2:14] <- names(temp.dat[,remove[1:13]])
  temp.name <- paste('figures/mod_nvar_', response, '.png', sep = '')
  png(temp.name)
  par(mar=c(4,5,1,5))
  plot(rse ~ nvars, mod.nvars, type = 'b', col = 'red', lwd = 2, ylab = 'Residual standard error', xlab = 'n Predictors')
  par(new = TRUE)
  plot(r2 ~ nvars, mod.nvars, type = 'b', col = 'blue', axes = F, bty = 'n', xlab = '', ylab = '')
  axis(side = 4)
  mtext("R2", side=4, line=3)
  legend('right', legend = c('RSE', 'R2'), col = c('red', 'blue'), pch = 1, bty = 'n')
  dev.off()
}


output$responses <- responses.clean
output[,c(2:4)] <- round(output[,c(2:4)], 2)
coefs[,c(2:9)] = round(coefs[,c(2:9)], 2)

write.csv(output, 'cached_data/mod_performance.csv', row.names = F)
write.csv(coefs, 'cached_data/mod_coefs.csv', row.names = F)
library(Hmisc)


png(paste('figures/holdout_varimp_effect_', response, '.png', sep = ''), height = 500)
par(mar = c(5,5,1,1), oma = c(0,4,0,0))
dotchart2(varimps$imp, labels = coefs$var.names[2:15], dotsize = 2, xlim = c(-1,100), 
          xlab = "Variable Importance", bty= "L",width.factor = .2, col = rgb(100,100,100,alpha = 100, maxColorValue = 255))
for (i in 3:11) {
  dotchart2(varimps[,i], dotsize = 2, add = TRUE, col = rgb(100,100,100,alpha = 100, maxColorValue = 255))
}
dev.off()



