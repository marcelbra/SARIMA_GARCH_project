library(tidyverse)
library(fGarch)
library(readxl)

options(warn = -1)
setwd("/Users/marcelbraasch/Desktop/TimeSeriesProject/")
data <- read_excel('Data/EDP RENOVAVEISprice.xlsx', skip = 3)

log_returns <- diff(log(data$Close))
N <- length(log_returns)

# Initial data plot
plot(0,0,xlim = c(0,N), ylim = c(min(log_returns),max(log_returns)), type = "n", main = "Log returns")
lines(1:length(log_returns), log_returns) # Returns
mean_vec <- rep(mean(log_returns), N)
std_vec <- rep(sd(log_returns), N)
lines(1:length(mean_vec), mean_vec, col = "#0000FF") # Mean
lines(1:length(std_vec), std_vec, col = "#FF0000") # Std
lines(1:length(std_vec), std_vec * -1, col = "#FF0000")

# Check various fitted models and compare AICs
summary(garchFit(formula = ~garch(1,1), data = log_returns, trace = FALSE))
# p q     AIC   1 2
# 1 0 -4.649665 X
# 2 0 -4.669183 X
# 3 0 -4.666500 X
# 4 0 -4.664773 X
# 5 0 -4.665047 X
# 1 1 -4.684707 
# 2 1 -4.681642 
# 3 1 -4.677745 X
# 4 1 -4.673867 X
# 5 1 -4.668735 X
# 1 2 -4.682089 
# 2 2 -4.678152 X
# 3 2 -4.674250 X
# 4 2 -4.670373 X
# 5 2 -4.665629 X
# 1 3 -4.680907 
# 2 3 -4.677594 X
# 3 3 -4.673657 X
# 4 3 -4.669795 X
# 5 3 -4.664046 X
# 1 4 -4.679921 X
# 2 4 -4.678822 X
# 3 4 -4.674928 X
# 4 4 -4.670991 X
# 5 4 -4.665979 X
# 1 5 -4.673375 X
# 2 5 -4.673467 X
# 3 5 -4.669530 X
# 4 5 -4.665764 X
# 5 5 -4.661844 X
# Only regard models with top 4 models (AIC < -4.67)
# Keep checking further (1,1), (2,1), (1,2) and (1,3)

summary(garchFit(formula = ~garch(1,1), data = log_returns, trace = FALSE))
#          Estimate   Std. Error  t value Pr(>|t|)    
# mu     -1.809e-03   9.695e-04   -1.866   0.0620 .  
# omega   4.057e-05   2.492e-05    1.628   0.1035    
# alpha1  1.024e-01   4.325e-02    2.367   0.0179 *  
# beta1   8.295e-01   7.284e-02   11.389   <2e-16 ***
# Standardised Residuals Tests: all not significant -> iid noise

summary(garchFit(formula = ~garch(2,1), data = log_returns, trace = FALSE))
#          Estimate  Std. Error  t value Pr(>|t|)    
# mu     -1.802e-03   9.747e-04   -1.848   0.0646 .  
# omega   3.957e-05   2.640e-05    1.499   0.1338    
# alpha1  1.014e-01   6.821e-02    1.486   0.1373    
# alpha2  1.000e-08   6.889e-02    0.000   1.0000    
# beta1   8.319e-01   7.845e-02   10.605   <2e-16 ***
# Standardised Residuals Tests: all not significant -> iid noise

summary(garchFit(formula = ~garch(1,2), data = log_returns, trace = FALSE))
#          Estimate  Std. Error  t value Pr(>|t|)  
# mu     -1.830e-03   9.692e-04   -1.888   0.0590 .
# omega   4.468e-05   2.809e-05    1.591   0.1117  
# alpha1  1.183e-01   5.735e-02    2.063   0.0391 *
# beta1   6.680e-01   3.236e-01    2.064   0.0390 *
# beta2   1.389e-01   2.724e-01    0.510   0.6101  
# Standardised Residuals Tests: all not significant -> iid noise

summary(garchFit(formula = ~garch(1,3), data = log_returns, trace = FALSE))
#          Estimate  Std. Error  t value Pr(>|t|)  
# mu     -1.849e-03   9.644e-04   -1.917   0.0552 .
# omega   5.404e-05   3.126e-05    1.729   0.0838 .
# alpha1  1.593e-01   7.162e-02    2.224   0.0262 *
# beta1   5.267e-01   3.081e-01    1.709   0.0874 .
# beta2   1.000e-08   5.439e-01    0.000   1.0000  
# beta3   2.255e-01   3.173e-01    0.711   0.4772  
# Standardised Residuals Tests: all not significant -> iid noise

# => Seems like GARCH(1,1) is the best fit, so we can proceed with this
# They support this:
# https://onlinelibrary.wiley.com/doi/pdf/10.1002/jae.800
# However, they claim the opposite:
# http://www.unstarched.net/2013/01/07/does-anything-not-beat-the-garch11/
# They question GARCH(1,1)'s existence:
# http://pubs.sciepub.com/jfa/1/2/2/
# From:
# https://stats.stackexchange.com/questions/175400/optimal-lag-order-selection-for-a-garch-model