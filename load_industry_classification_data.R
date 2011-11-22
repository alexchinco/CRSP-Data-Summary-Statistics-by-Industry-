
format_dates <- function(data, 
                         name, 
                         format = "YYYYMMDD", 
                         out_names = c("t","n")
                         ) 
{
 
  
  data[, name] <- as.numeric(as.character(data[, name]))
  
  
  if (format == "YYYYMMDD") {  
    year  <- floor(data[, name] / 10000)
    month <- floor((data[, name] - year * 10000) / 100)   
  } else if (format == "YYYYMM") {
    year  <- floor(data[, name] / 100)
    month <- floor(data[, name] - year * 100)
  }
  
  
  data[, out_names[1]] <- year + (month - 1) / 12
  data[, out_names[2]] <- (year - 1960) * 12 + month
  
  
  return(data)
  
  
}

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
  print(apply(data,2,sd,na.rm=TRUE))


  for (i in 1:3) {
    print("==================================================")
  }

  
}

load_industry_dictionaries <- function(tMin = 1988, 
                                       tMax = 2010
                                       ) 
{
  

  
  FILE                                        <- "crsp_compustat_link_table.csv"
  data.msci                                   <- read.csv(FILE, stringsAsFactors = FALSE)
  data.msci                                   <- data.msci[,c("lpermno", "linkdt", "linkenddt", "CIK", "SIC", "NAICS", "GSECTOR", "GGROUP", "GIND", "GSUBIND")]
  names(data.msci)                            <- c("a", "t.start", "t.end", "cik", "sic", "naics", "g_sec", "g_group", "g_ind", "g_subind")
  data.msci                                   <- data.msci[order(data.msci$a, data.msci$t.start), ]
  data.msci[data.msci$t.end == "E", ]$t.end   <- "20100101"
  data.msci                                   <- format_dates(data.msci, 
                                                              "t.start", 
                                                              out_names = c("t.start", "n.start")
                                                              )
  data.msci                                   <- format_dates(data.msci, 
                                                              "t.end", 
                                                              out_names = c("t.end", "n.end")
                                                              )
  data.msci                                   <- data.msci[order(data.msci$a, data.msci$n.start), ]
  print_df_summary(data.msci, "data.msci")
  
  
  
  FILE        <- "industries.json"
  dict.temp   <- fromJSON(file = FILE)
  dict.ff1988 <- dict.temp[[1]]  
  dict.mg1999 <- dict.temp[[2]]
  print(head(dict.mg1999))
  


  industry_dictionaries <- list(data.msci, 
                                dict.ff1988, 
                                dict.mg1999
                                )



  return(industry_dictionaries)



}

assign_msci_industries <- function(file_name = "demo") 
{
  
  
  industry_dictionaries       <- load_industry_dictionaries()
  dict                        <- industry_dictionaries[[1]]
  
  
  if (file_name == "demo") {
    FILE      <- "crsp_monthly_data.csv"
    data      <- read.csv(FILE, 
                          nrows = 10000,
                          stringsAsFactors = FALSE
                          )
    print_df_summary(data, "data")
    data              <- data[, c("PERMNO","DATE","SICCD")]
    names(data)       <- c("a", "t", "sic_0")
    data              <- format_dates(data, "t")
    data              <- data[is.na(data$sic_0) == FALSE, ]
    print_df_summary(data, "data")
    data.msci         <- data
  } else {
    load(file_name)
    load("market_data_1988to2010.Rdata")
    data      <- merge(data.r, data.rf, by = c("n"), all.x = TRUE)
    print_df_summary(data, "data")      
    data.msci <- data[,c("a","t")]
  }
  
  
  dict                <- dict[dict$a %in% sort(unique(data.msci$a)), ]
  num_rows            <- dim(dict)[1]
  data.msci$msci      <- NA
  data.msci$sic       <- NA
  print_df_summary(data.msci, "data.msci")      


  for (r in 1:num_rows) {
    a         <- dict$a[r]
    tMin      <- dict$t.start[r]
    tMax      <- dict$t.end[r]
    msci      <- dict$g_subind[r]
    sic       <- dict$sic[r]
    try({ 
      data.msci[(data.msci$a == a) & (data.msci$t >= tMin) & (data.msci$t <= tMax), ]$msci <- msci
      data.msci[(data.msci$a == a) & (data.msci$t >= tMin) & (data.msci$t <= tMax), ]$sic  <- sic
    }, silent = TRUE)      
    if (r %in% round(seq(1, num_rows, length = 101))) {
      print(round(r/num_rows * 100))
    }
  }
  print_df_summary(data.msci, "data.msci")      
  
  
  if (file_name == "demo") {
    print(data.msci[1:250,])
    save(data.msci, 
         file = "msci_industry_dict_demo.Rdata"
         )
  } else {
    print(data.msci[1:250,])
    save(data.msci, 
         file = "msci_industry_dict.Rdata"
         )
  }


}

assign_ff1988_industries <- function(file_name = "demo") 
{
  
  
  industry_dictionaries       <- load_industry_dictionaries()
  dict                        <- industry_dictionaries[[2]]

  
  if (file_name == "demo") {
    FILE      <- "crsp_monthly_data.csv"
    data      <- read.csv(FILE, 
                          nrows = 10000,
                          stringsAsFactors = FALSE
                          )
    data      <- data[, c("PERMNO","SICCD")]    
    data      <- data[is.na(data$SICCD) == FALSE, ]
    print_df_summary(data, "data")
  } else {
    load(file_name)
    FILE              <- "crsp_monthly_data.csv"
    data              <- read.csv(FILE, 
                                  stringsAsFactors = FALSE
                                  )
    data              <- data[, c("PERMNO","SICCD")]    
    data              <- data[is.na(data$SICCD) == FALSE, ]
    data              <- ddply(data, 
                               c("PERMNO"), 
                               function(X)X$SICCD[1]
                               )
    names(data)       <- c("a", "sic")
    data              <- data[data$a %in% unique(data.is_bal$a), ]
    print_df_summary(data, "data")  
  }
  

  data.ff1988         <- data
  data.ff1988$ind     <- "Other"
  data.ff1988$sub_ind <- "Other"
  print_df_summary(data.ff1988, "data.ff1988")
  
  
  for (i in 1:length(dict)) {
    print(names(dict[[i]]))
    for (k in 1:length(dict[[i]])) {
      a       <- dict[[i]][[k]]$start
      b       <- dict[[i]][[k]]$end
      try({ 
        data.ff1988[data.ff1988$sic %in% seq(a,b), ]$ind      <- names(dict)[i] 
        data.ff1988[data.ff1988$sic %in% seq(a,b), ]$sub_ind  <- names(dict[[i]])[k] 
      }, silent = TRUE)
    }
  }
  print_df_summary(data.ff1988, "data.ff1988")
  
  
  if (file_name == "demo") {
    print(data.ff1988[1:250,])
    save(data.ff1988, 
         file = "ff1988_industry_dict_demo.Rdata"
         )
  } else {
    print(data.ff1988[1:250,])
    save(data.ff1988, 
         file = "ff1988_industry_dict.Rdata"
         )
  }
  
  
}

assign_mg1999_industries <- function(file_name = "demo") 
{
  
  
  industry_dictionaries       <- load_industry_dictionaries()
  dict                        <- industry_dictionaries[[3]]
  
  
  if (file_name == "demo") {
    FILE      <- "crsp_monthly_data.csv"
    data      <- read.csv(FILE, 
                            nrows = 10000,
                            stringsAsFactors = FALSE
                            )
    data      <- data[, c("PERMNO","SICCD")]
    data      <- data[is.na(data$SICCD) == FALSE, ]
    print_df_summary(data, "data")
  } else {
    load(file_name)
    FILE              <- "crsp_monthly_data.csv"
    data              <- read.csv(FILE, 
                                  stringsAsFactors = FALSE
                                  )
    data              <- data[, c("PERMNO","SICCD")]    
    data              <- data[is.na(data$SICCD) == FALSE, ]
    data              <- ddply(data, 
                               c("PERMNO"), 
                               function(X)X$SICCD[1]
                               )
    names(data)       <- c("a", "sic")
    data              <- data[data$a %in% unique(data.is_bal$a), ]
    print_df_summary(data, "data")  
  }
  

  data.mg1999         <- data
  data.mg1999$ind     <- "Other"
  print_df_summary(data.mg1999, "data.mg1999")
  
  
  for (i in 1:length(dict)) {
    a <- dict[[i]]$start
    b <- dict[[i]]$end
    try({ 
      data.mg1999[data.mg1999$sic %in% seq(a,b), ]$ind <- names(dict)[i] 
    }, silent = TRUE)
  }
  print_df_summary(data.mg1999, "data.mg1999")
  
  
  if (file_name == "demo") {
    print(data.mg1999[1:250,])
    save(data.mg1999, 
         file = "mg1999_industry_dict_demo.Rdata"
         )
  } else {
    print(data.mg1999[1:250,])
    save(data.mg1999, 
         file = "mg1999_industry_dict.Rdata"
         )
  }
  
  
}
