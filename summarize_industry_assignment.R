
print_df_summary <- function(data, 
                             text
                             ) 
{

  
  for (i in 1:10) {
    print("                                                  ")
  }
  for (i in 1:3) {
    print("==================================================")
  }


  print(text)
  print(dim(data))
  print(head(data))
  print(summary(data))


  for (i in 1:3) {
    print("==================================================")
  }

  
}

plot_number_of_firms <- function() 
{
  
  
  load("stock_data_rp1_hp1_1988to2010.Rdata")
  load("market_data_1988to2010.Rdata")


  data <- merge(data.r, data.mcap, by = c("n", "a"))
  data <- merge(data, data.is_bal, by = c("n", "a"))
  data <- merge(data, data.rf, by = c("n"))
  data <- data[data$is_bal == 1, ]
  print_df_summary(data, "data")
  print(length(unique(data$a)))


  data.plot           <- ddply(data,
                               c("t"),
                               function(X)dim(X)[1]
                               )
  names(data.plot)    <- c("t", "Number of Firms")
  data.plot           <- melt(data.plot, c("t"))

  
  RAW_FILE <- 'plot_number_of_firms'
  TEX_FILE <- paste(RAW_FILE,'.tex',sep='')
  PDF_FILE <- paste(RAW_FILE,'.pdf',sep='')
  PNG_FILE <- paste(RAW_FILE,'.png',sep='')
  

  tikz(file = TEX_FILE, height = 5, width = 9, standAlone=TRUE)
  p <- ggplot(data.plot)
  p <- p + geom_path(aes(x            = t, 
                         y            = value,
                         colour       = variable
                         ),
                     linetype = 1,
                     size     = 2
                     )
  p <- p + facet_wrap(~variable, ncol = 1, scale = "free_y")
  p <- p + ylab('') + xlab('$t$')
  p <- p + opts(legend.position = "none")
  print(p)
  dev.off()
  
  
  tools::texi2dvi(TEX_FILE, pdf = TRUE)
  OS <- Sys.info()["sysname"]
  if (OS == "Linux") {
    system(paste('convert -density 450 ', file.path(PDF_FILE), ' ', file.path(PNG_FILE)))
  } else if (OS == "Darwin") {
    system(paste('sips -s format png', file.path(PDF_FILE), '--out', file.path(PNG_FILE)))
  }
  
  
}

plot_number_of_firms_per_industry <- function(industry_classification = "mg1999") 
{
  
  
  load("stock_data_rp1_hp1_1988to2010.Rdata")
  load("market_data_1988to2010.Rdata")


  data <- merge(data.r, data.mcap, by = c("n", "a"))
  data <- merge(data, data.is_bal, by = c("n", "a"))
  data <- merge(data, data.rf, by = c("n"))
  data <- data[data$is_bal == 1, ]
  print_df_summary(data, "data")
  print(length(unique(data$a)))


  if (industry_classification == "mg1999") {
    load("mg1999_industry_dict.Rdata")
    data <- merge(data, data.mg1999, by = c("a"))
  } else if (industry_classification == "ff1988") {
    load("ff1988_industry_dict.Rdata")
    data <- merge(data, data.ff1988, by = c("a"))
  } else if (industry_classification == "msci") {
    load("msci_industry_dict.Rdata")
    data <- merge(data, data.msci, by = c("a","t"))
  }
  print_df_summary(data, "data")
  print(length(unique(data$a)))

  
  data.plot           <- ddply(data,
                               c("t", "ind"),
                               function(X)dim(X)[1]
                               )
  names(data.plot)    <- c("t", "ind", "value")
  data.plot           <- data.plot[data.plot$ind != "Other", ]

  
  RAW_FILE <- paste('plot_number_of_firms_per_industry_', industry_classification, sep = '')
  TEX_FILE <- paste(RAW_FILE,'.tex',sep='')
  PDF_FILE <- paste(RAW_FILE,'.pdf',sep='')
  PNG_FILE <- paste(RAW_FILE,'.png',sep='')
  

  if (industry_classification == "mg1999") {
    png(file = PNG_FILE, height = 1200, width = 2500)
  } else {
    png(file = PNG_FILE, height = 1500, width = 2700)
  }
  p <- ggplot(data.plot)
  p <- p + geom_path(aes(x            = t, 
                         y            = value,
                         colour       = ind
                         ),
                     linetype = 1,
                     size     = 1
                     )
  if (industry_classification == "mg1999") {
    p <- p + facet_wrap(~ind, ncol = 5, scale = "free_y")
  } else {
    p <- p + facet_wrap(~ind, ncol = 8, scale = "free_y")
  }
  p <- p + ylab('') + xlab('t')
  p <- p + opts(legend.position = "none")
  print(p)
  dev.off()

  
}

plot_number_of_firms_per_sub_industry <- function() 
{
  
  
  load("stock_data_rp1_hp1_1988to2010.Rdata")
  load("market_data_1988to2010.Rdata")


  data <- merge(data.r, data.mcap, by = c("n", "a"))
  data <- merge(data, data.is_bal, by = c("n", "a"))
  data <- merge(data, data.rf, by = c("n"))
  data <- data[data$is_bal == 1, ]
  print_df_summary(data, "data")
  print(length(unique(data$a)))


  load("ff1988_industry_dict.Rdata")
  data <- merge(data, data.ff1988, by = c("a"))
  print_df_summary(data, "data")
  print(length(unique(data$a)))

  
  data.plot           <- ddply(data,
                               c("t", "ind", "sub_ind"),
                               function(X)dim(X)[1]
                               )
  names(data.plot)    <- c("t", "ind", "sub_ind", "value")
  data.plot           <- data.plot[data.plot$ind != "Other", ]

  
  RAW_FILE <- 'plot_number_of_firms_per_sub_industry_ff1988'
  TEX_FILE <- paste(RAW_FILE,'.tex',sep='')
  PDF_FILE <- paste(RAW_FILE,'.pdf',sep='')
  PNG_FILE <- paste(RAW_FILE,'.png',sep='')
  

  png(file = PNG_FILE, height = 1500, width = 2700)
  p <- ggplot(data.plot)
  p <- p + geom_path(aes(x            = t, 
                         y            = value,
                         colour       = ind,
                         group        = sub_ind
                         ),
                     linetype = 1,
                     size     = 1
                     )
  p <- p + facet_wrap(~ind, ncol = 8, scale = "free_y")
  p <- p + ylab('') + xlab('$t$')
  p <- p + opts(legend.position = "none")
  print(p)
  dev.off()
  
  
}

plot_distribution_of_excess_returns_by_industry <- function(industry_classification = "mg1999") 
{
  
  
  load("stock_data_rp1_hp1_1988to2010.Rdata")
  load("market_data_1988to2010.Rdata")


  data <- merge(data.r, data.mcap, by = c("n", "a"))
  data <- merge(data, data.is_bal, by = c("n", "a"))
  data <- merge(data, data.rf, by = c("n"))
  data <- data[data$is_bal == 1, ]
  print_df_summary(data, "data")
  print(length(unique(data$a)))


  if (industry_classification == "mg1999") {
    load("mg1999_industry_dict.Rdata")
    data <- merge(data, data.mg1999, by = c("a"))
  } else if (industry_classification == "ff1988") {
    load("ff1988_industry_dict.Rdata")
    data <- merge(data, data.ff1988, by = c("a"))
  } else if (industry_classification == "msci") {
    load("msci_industry_dict.Rdata")
    data <- merge(data, data.msci, by = c("a","t"))
  }
  print_df_summary(data, "data")
  print(length(unique(data$a)))

  
  data$y              <- floor(data$t)
  data.plot           <- ddply(data,
                               c("y", "a", "ind"),
                               function(X)mean(X$r, na.rm = TRUE)
                               )
  names(data.plot)    <- c("y", "a", "ind", "r")
  data.plot           <- data.plot[data.plot$ind != "Other", ]
  print_df_summary(data.plot, "data.plot")
  
  
  RAW_FILE <- paste('plot_distribution_of_excess_returns_by_industry_', industry_classification, sep = '')
  TEX_FILE <- paste(RAW_FILE,'.tex',sep='')
  PDF_FILE <- paste(RAW_FILE,'.pdf',sep='')
  PNG_FILE <- paste(RAW_FILE,'.png',sep='')
  

  if (industry_classification == "mg1999") {
    png(file = PNG_FILE, height = 1200, width = 2500)
  } else {
    png(file = PNG_FILE, height = 1500, width = 2700)
  }
  stat_sum_single <- function(fun, geom = "point", ...) {
    stat_summary(fun.y        = fun, 
                 colour       = "red", 
                 geom         = geom, 
                 size         = 3,
                 ...
                 ) 
  } 
  p <- ggplot(data.plot,
              aes(x           = y,
                  y           = r,
                  group       = y,
                  colour      = ind
                  )
              )
  p <- p + geom_boxplot()
  p <- p + stat_sum_single(mean)
  if (industry_classification == "mg1999") {
    p <- p + facet_wrap(~ind, ncol = 5, scale = "free_y")
  } else {
    p <- p + facet_wrap(~ind, ncol = 8, scale = "free_y")
  }
  p <- p + ylab('') + xlab('y')
  p <- p + opts(legend.position = "none")
  print(p)
  dev.off()
  
  
}

plot_market_cap_by_industry <- function(industry_classification = "mg1999") 
{
  
  
  load("stock_data_rp1_hp1_1988to2010.Rdata")
  load("market_data_1988to2010.Rdata")


  data <- merge(data.r, data.mcap, by = c("n", "a"))
  data <- merge(data, data.is_bal, by = c("n", "a"))
  data <- merge(data, data.rf, by = c("n"))
  data <- data[data$is_bal == 1, ]
  print_df_summary(data, "data")
  print(length(unique(data$a)))


  if (industry_classification == "mg1999") {
    load("mg1999_industry_dict.Rdata")
    data <- merge(data, data.mg1999, by = c("a"))
  } else if (industry_classification == "ff1988") {
    load("ff1988_industry_dict.Rdata")
    data <- merge(data, data.ff1988, by = c("a"))
  } else if (industry_classification == "msci") {
    load("msci_industry_dict.Rdata")
    data <- merge(data, data.msci, by = c("a","t"))
  }
  print_df_summary(data, "data")
  print(length(unique(data$a)))

  
  data.plot           <- ddply(data,
                               c("t", "ind"),
                               function(X)sum(X$mcap, na.rm = TRUE)
                               )
  names(data.plot)    <- c("t", "ind", "mcap")
  data.plot           <- data.plot[data.plot$ind != "Other", ]
  print_df_summary(data.plot, "data.plot")
  
  
  RAW_FILE <- paste('plot_market_cap_by_industry_', industry_classification, sep = '')
  TEX_FILE <- paste(RAW_FILE,'.tex',sep='')
  PDF_FILE <- paste(RAW_FILE,'.pdf',sep='')
  PNG_FILE <- paste(RAW_FILE,'.png',sep='')
  

  if (industry_classification == "mg1999") {
    png(file = PNG_FILE, height = 1200, width = 2500)
  } else {
    png(file = PNG_FILE, height = 1500, width = 2700)
  }
  p <- ggplot(data.plot,
              aes(x           = t,
                  y           = mcap,
                  group       = ind,
                  colour      = ind
                  )
              )
  p <- p + geom_path(size = 2)
  if (industry_classification == "mg1999") {
    p <- p + facet_wrap(~ind, ncol = 5, scale = "free_y")
  } else {
    p <- p + facet_wrap(~ind, ncol = 8, scale = "free_y")
  }
  p <- p + ylab('') + xlab('t')
  p <- p + opts(legend.position = "none")
  print(p)
  dev.off()
  
  
}
