library(tidyverse)
library(fGarch)
library(readxl)
library(stats)
library(itsmr)

options(warn = -1)
setwd("/Users/marcelbraasch/Desktop/TimeSeriesProject/Data/")
names <- c("NOVABASESGPS", "NOSSGPS", "MOTA ENGIL", "GALP ENERGIA-NOM", "EDP RENOVAVEIS")



get_log_returns <- function(names) {
  N <- length(read_excel(paste(names[[1]],"price.xlsx", sep=""), skip = 3)[["Close"]]) - 1
  log_returns <- data.frame(a = 1:N)
  for (name in names) {
    log_returns[name] <- diff(log(read_excel(paste(name,"price.xlsx", sep=""), skip = 3)[["Close"]]))
  }
  log_returns
}

log_returns <- get_log_returns(names)

plot_data <- function(log_returns, name) {
  # Plots the time series of the given log returns with mean and standard deviation
  plot(0, 0, xlim = c(0,N), ylim = c(min(log_returns[[name]]),max(log_returns[[name]])),
       type = "n",  main = "", xlab="Time in days", ylab="Log returns in percent")
  lines(1:length(log_returns[[name]]), log_returns[[name]]) # Returns
  mean_vec <- rep(mean(log_returns[[name]]), N)
  std_vec <- rep(sd(log_returns[[name]]), N)
  lines(1:length(mean_vec), mean_vec, col = "#0000FF") # Mean
  lines(1:length(std_vec), std_vec, col = "#FF0000") # Std
  lines(1:length(std_vec), std_vec * -1, col = "#FF0000")
}

# Plot ACF of log returns and squared log returns
setwd("/Users/marcelbraasch/Desktop/TimeSeriesProject/Assets")
save_plots <- function() {
  for (name in names) {
    png(sprintf("plot_%s.png", name))
    plot_data(log_returns, name)
    dev.off()
    png(sprintf("acf_return_%s.png", name))
    plot(acf(log_return, 40), main = name)
    dev.off()
    png(sprintf("acf_squared_return_%s.png", name))
    plot(acf(log_return**2, 40), main = "")# sprintf("ACF of %s's squared log-returns", name))
    dev.off()
  }
}


###########
# => Seems like GARCH(1,1) is the best fit, so we can proceed with this
# They support this:
# https://onlinelibrary.wiley.com/doi/pdf/10.1002/jae.800
# However, they claim the opposite:
# http://www.unstarched.net/2013/01/07/does-anything-not-beat-the-garch11/
# They question GARCH(1,1)'s existence:
# http://pubs.sciepub.com/jfa/1/2/2/
###########

# Set parameter for current stock series
run_tests <- function(number) {
  name <- names[[number]]
  log_return <- log_returns[[name]]
  name
  # Fit the final model and do tests
  model <- garchFit(formula = ~garch(1,1), data = log_return, trace = FALSE)
  #summary(model)
  test(residuals(model, standardize=TRUE))
  print(name)
}

run_tests(5)
