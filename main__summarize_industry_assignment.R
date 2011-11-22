
rm(list=ls())


library(foreign)
library(reshape)
library(plyr)
library(matlab)
library(rjson)


library(RColorBrewer)
library(ggplot2)
library(tikzDevice)
library(classInt)


source("summarize_industry_assignment.R")    
## plot_number_of_firms()
## plot_number_of_firms_per_industry(industry_classification = "mg1999")
## plot_number_of_firms_per_industry(industry_classification = "ff1988")
## plot_number_of_firms_per_sub_industry()
## plot_distribution_of_excess_returns_by_industry(industry_classification = "mg1999")
## plot_distribution_of_excess_returns_by_industry(industry_classification = "ff1988")
## plot_market_cap_by_industry(industry_classification = "mg1999")
plot_market_cap_by_industry(industry_classification = "ff1988")
