
format_dates <- function(data, 
                         name, 
                         format       = "YYYYMMDD", 
                         out_names    = c("t","n")
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

load_market_data <- function(tMin = 1988, 
                             tMax = 2010
                             ) 
{
  
  
  
  FILE                <- "F-F_Research_Data_Factors.txt"
  data.raw            <- read.csv(FILE, 
                                  stringsAsFactors    = FALSE, 
                                  sep                 = " ",
                                  header              = FALSE
                                  )
  names(data.raw)     <- c("t", "rme", "smb", "hml", "rf")
  data.raw            <- data.raw[,c("t","rme","rf")]
  data.raw            <- format_dates(data.raw, "t", format = "YYYYMM")
  data.raw            <- data.raw[(data.raw$t >= tMin) & (data.raw$t < tMax), ]
  print_df_summary(data.raw, "data.raw")
  
  
  
  data.raw$rme        <- data.raw$rme / 100
  data.raw$rf         <- data.raw$rf / 100
  print_df_summary(data.raw, "data.raw")
  
  
  
  data.rme    <- data.raw[,names(data.raw) %in% c("t","n","rme")]
  data.rf     <- data.raw[,names(data.raw) %in% c("t","n","rf")]
  

  
  save(data.rme, 
       data.rf, 
       file = paste("market_data_", tMin, "to", tMax, ".Rdata", sep="")
       )  


  
}

load_fama_french_factors <- function(tMin = 1988, 
                                     tMax = 2010
                                     ) 
{
  
  
  
  FILE                <- "F-F_Research_Data_Factors.txt"
  data.raw            <- read.csv(FILE, 
                                  stringsAsFactors    = FALSE, 
                                  sep                 = " ",
                                  header              = FALSE
                                  )
  names(data.raw)     <- c("t", "rme", "smb", "hml", "rf")
  data.raw            <- data.raw[,c("t","smb","hml")]
  data.raw            <- format_dates(data.raw, "t", format = "YYYYMM")
  data.raw            <- data.raw[(data.raw$t >= tMin) & (data.raw$t < tMax), ]
  print_df_summary(data.raw, "data.raw")



  data.raw$smb        <- data.raw$smb/100
  data.raw$hml        <- data.raw$hml/100
  print_df_summary(data.raw, "data.raw")
  
  
  
  data.smb    <- data.raw[,names(data.raw) %in% c("t","n","smb")]
  data.hml    <- data.raw[,names(data.raw) %in% c("t","n","hml")]
  
  
  
  save(data.smb, 
       data.hml, 
       file = paste("fama_french_factors_", tMin, "to", tMax, ".Rdata", sep="")
       )  
  
}

load_momentum_factor <- function(tMin = 1988, 
                                 tMax = 2010
                                 ) 
{
  
  
  
  FILE                <- "F-F_Momentum_Factor.txt"
  data.raw            <- read.csv(FILE, 
                                  sep                 = " ", 
                                  stringsAsFactors    = FALSE, 
                                  header              = TRUE
                                  )
  names(data.raw)     <- c("t", "rmom")
  data.raw            <- format_dates(data.raw, "t", format = "YYYYMM")
  data.raw            <- data.raw[(data.raw$t >= tMin) & (data.raw$t < tMax), ]
  print_df_summary(data.raw, "data.raw")
  
  
  
  data.raw$rmom       <- data.raw$rmom/100
  print_df_summary(data.raw, "data.raw")



  data.rmom   <- data.raw[,names(data.raw) %in% c("t","n","rmom")]


  
  save(data.rmom, 
       file = paste("momentum_factor_", tMin, "to", tMax, ".Rdata", sep="")
       )  
  
  
  
}

clean_firm_data <- function(data.raw) 
{
  
  
  data.raw    <- data.raw[data.raw$exch %in% c(1,2,3), ]    

  
  data.raw$status     <- as.character(data.raw$status)
  data.raw            <- data.raw[data.raw$status == "A",]
  
  
  data.raw                            <- data.raw[is.na(data.raw$p) == FALSE, ]
  data.raw[data.raw$p < 0, ]$p        <- - data.raw[data.raw$p < 0, ]$p
  
  
  data.raw$r  <- as.numeric(as.character(data.raw$r))
  data.raw    <- data.raw[is.na(data.raw$r) == FALSE, ]    
  
  
  data.raw    <- data.raw[is.na(data.raw$shares) == FALSE, ]
  data.raw    <- data.raw[is.na(data.raw$sic) == FALSE, ]
  
  
  data.raw$mcap       <- data.raw$shares * data.raw$p    
  data.clean          <- data.raw[,names(data.raw) %in% c('a','t','n','r','p','mcap','sic')]


  print_df_summary(data.clean, "data.clean")
  return(data.clean)
  
  
}

identify_big_firms <- function(data.mcap) 
{
  
  
  data.q25    <- ddply(data.mcap, 
                       c("n"),
                       function(X)quantile(X$mcap, prob = 0.25)
                       )
  names(data.q25)     <- c("n", "q25")
  
  
  data.mcap           <- merge(data.mcap, data.q25, by = c("n"))    
  data.mcap           <- data.mcap[order(data.mcap$a, data.mcap$n), ]
  
  
  data.mcap$is_big <- NA
  data.mcap[data.mcap$mcap <= data.mcap$q25, ]$is_big       <- 0
  data.mcap[data.mcap$mcap > data.mcap$q25, ]$is_big        <- 1 
  
  
  print_df_summary(data.mcap, "data.mcap")
  return(data.mcap)
  
  
}

balance_panel <- function(data.r,
                          ranking_period, 
                          holding_period
                          ) 
{
  
  
  firms       <- sort(unique(data.r$a))
  nFirms      <- length(firms)
  
  
  data.is_bal <- foreach(j = 1:nFirms, .combine = "rbind") %dopar% {      
    panel <- data.r[data.r$a == firms[j], ]
    panel$is_bal      <- 0
    nLen              <- length(panel$n)
    for (k in 1:nLen) {
      try({
        m                     <- panel$n[k]
        rank_period_full      <- as.logical(length(panel[panel$n %in% seq(m-ranking_period,m-1), ]$r) == ranking_period)
        hold_period_full      <- as.logical(length(panel[panel$n %in% seq(m,m+holding_period-1), ]$r) == holding_period)
        if (rank_period_full == TRUE & hold_period_full == TRUE) {
          panel$is_bal[k] <- 1
        }
      })
    }
    panel <- panel[, names(panel) %in% c("a","n","is_bal")]
    if (j %in% round(seq(1, nFirms, length = 101))) {
      print(signif(j/nFirms * 100, 3))
    }
    return(panel)
  }
  
  
  print_df_summary(data.is_bal, "data.is_bal")
  return(data.is_bal)
  
  
}

load_firm_data <- function(ranking_period, 
                           holding_period, 
                           tMin = 1988, 
                           tMax = 2010
                           ) 
{
  
  
  FILE                <- "crsp_monthly_data.csv"
  data.raw            <- read.csv(FILE, stringsAsFactors = FALSE)
  data.raw            <- data.raw[, c("DATE", "PERMNO", "EXCHCD", "SICCD", "TRDSTAT", "SHROUT", "PRC", "RET")]
  names(data.raw)     <- c("t", "a", "exch", "sic", "status", "shares", "p", "r")
  data.raw            <- format_dates(data.raw, "t")
  data.raw            <- data.raw[(data.raw$t >= tMin) & (data.raw$t < tMax), ]
  print_df_summary(data.raw, "data.raw")
  
      
  data.clean  <- clean_firm_data(data.raw)
  
  
  data.r      <- data.clean[order(data.clean$a, data.clean$n), c("n","a","r")]
  print_df_summary(data.r, "data.r")
  
  
  data.mcap   <- data.clean[order(data.clean$a, data.clean$n), c("n","a","mcap")]
  data.mcap   <- identify_big_firms(data.mcap)

  
  data.is_bal <- balance_panel(data.r, ranking_period, holding_period)
  
  
  save(data.r, 
       data.mcap, 
       data.is_bal,
       file = paste("stock_data_rp", ranking_period, "_hp", holding_period, "_", tMin, "to", tMax, ".Rdata", sep="")
       )
  
  
}
