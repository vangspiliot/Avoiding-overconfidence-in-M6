setwd("C:/Users/vangs/Desktop/m6d")
library(quantmod)
library(purrr)
library(plyr)

#The M6 asset universe, excluding "DRE", "RE", "CARR", "OGN", "XLC"
assets <- c(
  "ABBV","ACN","AEP","AIZ","ALLE","AMAT","AMP","AMZN","AVB","AVY",   
  "AXP","BDX","BF-B","BMY","BR","CDW","CE","CHTR","CNC",   
  "CNP","COP","CTAS","CZR","DG","DPZ","DXC","META","FTV",   
  "GOOG","GPC","HIG","HST","JPM","KR","PG","PPL","PRU",   
  "PYPL","ROL","ROST","UNH","URI","V","VRSK","WRK","XOM",   
  "IVV","IWM","EWU","EWG","EWL","EWQ","IEUS","EWJ","EWT","MCHI",  
  "INDA","EWY","EWA","EWH","EWZ","EWC","IEMG","LQD","HYG","SHY",  
  "IEF","TLT","SEGA.L","IEAA.L","HIGH.L","JPEA.L","IAU","SLV","GSG","REET",  
  "ICLN","IXN","IGF","IUVL.L","IUMO.L","SPMV.L","IEVL.L","IEFM.L","MVEU.L","XLK",   
  "XLF","XLV","XLE","XLY","XLI","XLU","XLP","XLB","VXX") 

#Download historical data (select starting date)
staring_date <- "2018-01-01"
data <- getSymbols(assets, src="yahoo", from = staring_date)

dataset <- NULL
for (aid in assets){
  tmp <- get(aid) ; colnames(tmp)[6] <- "Adjusted"
  symbol <- aid
  price <- as.numeric(tmp$Adjusted)
  date <- index(tmp)
  df <- data.frame(symbol, date, price)
  dataset <- rbind(dataset, df)
}
rm(list=setdiff(ls(), c("assets","dataset")))
unique(dataset$symbol)

dataset <- dataset[dataset$date<="2024-07-12",]

write.csv(dataset, "assets_m6_extended.csv", row.names = FALSE)