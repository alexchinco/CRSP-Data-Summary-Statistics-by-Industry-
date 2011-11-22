
rm(list=ls())



library(foreign)
library(reshape)
library(plyr)
library(matlab)
library(rjson)
set.seed(12345)



source("load_industry_classification_data.R")    
## assign_msci_industries(file_name = "stock_data_rp1_hp1_1988to2010.Rdata") 
assign_ff1988_industries(file_name = "stock_data_rp1_hp1_1988to2010.Rdata") 
## assign_mg1999_industries(file_name = "stock_data_rp1_hp1_1988to2010.Rdata")
