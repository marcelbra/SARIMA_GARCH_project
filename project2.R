library(tidyverse)
library(fGarch)

setwd("/Users/marcelbraasch/Desktop/TimeSeriesProject/")
data <- read_excel('Data/EDP RENOVAVEISprice.xlsx', skip = 3)
log_returns <- diff(log(data$Open))
fitted_model <- garchFit(formula = ~garch(1,1), data = log_returns, trace = FALSE)
