
rm(list=ls())


library(foreign)
library(reshape)
library(plyr)
library(matlab)
set.seed(12345)


library(foreach)
library(doMC)
registerDoMC(2)


source("load_monthly_stock_data.R")
load_market_data()
load_fama_french_factors()
load_momentum_factor()

for (rp in seq(2,6)) {
  load_firm_data(ranking_period = rp, holding_period = 1)
}
