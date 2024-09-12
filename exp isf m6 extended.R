setwd("C:/Users/vangs/Desktop/m6d")
library(plyr)
library(forecast)
library(ggplot2)
#Function for computing RPS
Class_calculation <- function(hist_data){
  #hist_data <- hist_data_tmp
  asset_id <- unique(hist_data$symbol)
  hist_data$date <- as.Date(hist_data$date)
  from_date <- min(hist_data$date)
  to_date <- max(hist_data$date)
  eligible_days <- unique(hist_data$date)
  
  #Compute percentage returns
  returns <- data.frame(matrix(NA, nrow = length(asset_id), ncol = 2))
  colnames(returns) <- c("ID", "Return")
  for (i in 1:length(asset_id)){
    temp <- hist_data[hist_data$symbol==asset_id[i],]
    returns$ID[i] <- temp$symbol[1]
    returns$Return[i] <- (temp[temp$date==to_date,]$price - temp[temp$date==from_date,]$price)/temp[temp$date==from_date,]$price
  }
  
  #Define the relevant position of each asset
  ranking <- data.frame(matrix(NA, nrow = length(asset_id), ncol = 2))
  colnames(ranking) <- c("ID", "Position")
  ranking$ID <- asset_id
  ranking <- merge(ranking, returns, by="ID", all.x = T)
  ranking$Position <- rank(ranking$Return, ties.method = "min")
  
  #Handle Ties
  Series_per_position <- table(ranking$Position)
  Series_per_position <- data.frame(Series_per_position,t(rep(NA,6)))
  colnames(Series_per_position) <- c("Position", "Series","Rank", "Rank1", "Rank2", "Rank3", "Rank4","Rank5")
  Series_per_position$Position <- as.numeric(as.character(Series_per_position$Position))
  for (i in 1:nrow(Series_per_position)){
    
    start_p <- Series_per_position$Position[i]
    end_p <- Series_per_position$Position[i] + Series_per_position$Series[i] - 1
    temp <- data.frame(seq(start_p,end_p,1),NA,t(rep(0,5))) 
    colnames(temp) <- c("Position","Rank", "Rank1", "Rank2", "Rank3", "Rank4","Rank5")
    
    if (nrow(temp[temp$Position<=19,])>0){
      temp[temp$Position<=19,]$Rank <- 1
      temp[temp$Position<=19,]$Rank1 <- 1
    }
    if (nrow(temp[(temp$Position>19)&(temp$Position<=38),])>0){
      temp[(temp$Position>19)&(temp$Position<=38),]$Rank <- 2
      temp[(temp$Position>19)&(temp$Position<=38),]$Rank2 <- 1
    }
    if (nrow(temp[(temp$Position>38)&(temp$Position<=57),])>0){
      temp[(temp$Position>38)&(temp$Position<=57),]$Rank <- 3
      temp[(temp$Position>38)&(temp$Position<=57),]$Rank3 <- 1
    }
    if (nrow(temp[(temp$Position>57)&(temp$Position<=76),])>0){
      temp[(temp$Position>57)&(temp$Position<=76),]$Rank <- 4
      temp[(temp$Position>57)&(temp$Position<=76),]$Rank4 <- 1
    }
    if (nrow(temp[temp$Position>76,])>0){
      temp[temp$Position>76,]$Rank <- 5
      temp[temp$Position>76,]$Rank5 <- 1
    }
    Series_per_position[i,c(3:8)] <- as.numeric(colMeans(temp)[2:7])
  }
  Series_per_position$Series <- NULL
  ranking <- merge(ranking, Series_per_position, by="Position", all.x = TRUE)
  ranking <- ranking[,c("ID", "Return", "Position", "Rank", 
                        "Rank1", "Rank2", "Rank3", "Rank4", "Rank5")]
  
  return(ranking)
  
}
#Functions for applying the proposed approach
sym_probs <- function(f){
  p1 <- sum(c(f[1], f[5]))/2
  p2 <- sum(c(f[2], f[4]))/2
  f <- c(p1,p2,f[3],p2,p1)
  return(f)
} #Symmetric
ra_probs <- function(f,p){
  p1 <- sum(c(f[1], f[5]))
  p2 <- sum(c(f[2], f[4]))
  f <- c(p1*(1-p), p2*(1-p),f[3],p2*p, p1*p)
  return(f)
} #Based on probability of positive gains

#Read asset data
asset_data<- read.csv("assets_m6_extended.csv", stringsAsFactors = F)
asset_data$date <- as.Date(asset_data$date)

#Interpolate asset values
template <- seq.Date(min(asset_data$date), max(asset_data$date), 1)
template <- data.frame(template,NA) ; colnames(template) <- c("date","X")
asset_data_new <- NULL
for (i in unique(asset_data$symbol)){
  tmp <- asset_data[asset_data$symbol==i,]
  tmp <- merge(template, tmp, all.x = TRUE)
  tmp$wd <- weekdays(tmp$date)
  tmp <- tmp[!(tmp$wd %in% c("Saturday","Sunday")),]
  tmp$X <- NULL
  tmp$symbol <- i
  for (j in 2:nrow(tmp)){
    if (is.na(tmp$price[j])){tmp$price[j] <- tmp$price[j-1]}
  }
  asset_data_new <- rbind(asset_data_new, tmp)
}
asset_data <- asset_data_new
rm(tmp,i,j,asset_data_new, template)

#Identify Fridays
sub_period_end <- unique(asset_data[asset_data$wd=="Friday",]$date)
sub_period_end <- sub_period_end[-1]
sub_period_start <- sub_period_end-7
sub_period_name <- paste0("p",c(1:length(sub_period_start)))
sub_period_info <- data.frame(sub_period_name, sub_period_start, sub_period_end)
rm(sub_period_end, sub_period_start)

#Extract asset ranks and classes
pid_c = 1 
ranks_of_assets <- NULL
for (pid_c in c(1:nrow(sub_period_info))){
  
  pid = sub_period_info$sub_period_name[pid_c]
  hist_data_tmp <- asset_data[asset_data$date>=sub_period_info[sub_period_info$sub_period_name==pid,]$sub_period_start &
                                asset_data$date<=sub_period_info[sub_period_info$sub_period_name==pid,]$sub_period_end,]
  output <- Class_calculation(hist_data_tmp)
  output$pid <- pid
  ranks_of_assets <- rbind(ranks_of_assets, output)
}
rm(output, pid, pid_c)

#Summary statistics per asset
s_avg <- ddply(ranks_of_assets[,c("ID","Return","Rank")], .(ID),colwise(mean))
colnames(s_avg) <- c("symbol", "AvgReturn", "AvgRank")
s_sd <- ddply(ranks_of_assets[,c("ID","Return","Rank")], .(ID),colwise(sd))
colnames(s_sd) <- c("symbol", "SdReturn", "SdRank")
s_sum <- ddply(ranks_of_assets[,c("ID","Rank1","Rank2","Rank3","Rank4","Rank5")], .(ID),colwise(sum))
colnames(s_sum)[1] <- c("symbol")
stats <- merge(merge(s_avg, s_sd, by="symbol"), s_sum, by="symbol")
rm(s_avg, s_sd, s_sum)
colnames(stats)[1] <- "Asset"


#Forecasting
aid <- unique(asset_data$symbol)[1] ; xstart <- 7
rps_sub <- NULL
mname <- c("frequency", "ses", "arima")
mname <- c("benchmark", mname, paste0(mname,"_s"), paste0(mname,"_ra"))

forecasts <- NULL
aid <- "XLY" ; xstart <- 11
for (aid in unique(asset_data$symbol)){
  
  x <- ranks_of_assets[ranks_of_assets$ID==aid,]
  xrange <- c(10:339) #Repeat evaluation for 48 consecutive weeks
  
  for (xstart in xrange){
    trainset <- x$Rank[1:xstart]
    testset <- x[xstart+1,]
    
    #Benchmark
    frc_b <- rep(0.2, 5)
    #Frequency
    frc_f <- c(sum(trainset[trainset==1]),
               sum(trainset[trainset==2])/2,
               sum(trainset[trainset==3])/3,
               sum(trainset[trainset==4])/4,
               sum(trainset[trainset==5])/5)
    frc_f <- frc_f/sum(frc_f)
    frc_pos <- nrow(x[x$Return>0,])/nrow(x)
    
    frc_f_s <- sym_probs(frc_f)
    frc_f_ra <- ra_probs(frc_f, frc_pos)
    
    #SES
    frc_or <- ses(trainset, h=1)
    frc_ref <- ses(x$Return,h=1)
    frc_pos <- 1 - pnorm(0,mean = frc_ref$mean, sd=frc_ref$model$sigma2^0.5)
    
    set_p <- c(0,
               pnorm(1.5,mean = frc_or$mean, sd=frc_or$model$sigma2^0.5),
               pnorm(2.5,mean = frc_or$mean, sd=frc_or$model$sigma2^0.5),
               pnorm(3.5,mean = frc_or$mean, sd=frc_or$model$sigma2^0.5),
               pnorm(4.5,mean = frc_or$mean, sd=frc_or$model$sigma2^0.5))
    frc_ses <- c(diff(set_p), 1-sum(diff(set_p)))
    frc_ses_s <- sym_probs(frc_ses)
    frc_ses_ra <- ra_probs(frc_ses, frc_pos)
    
    #ARIMA
    frc_or <- forecast(auto.arima(trainset), h=1)
    frc_ref <- forecast(auto.arima(x$Return), h=1)
    frc_pos <- 1 - pnorm(0,mean = frc_ref$mean, sd=frc_ref$model$sigma2^0.5)
    
    set_p <- c(0,
               pnorm(1.5,mean = frc_or$mean, sd=frc_or$model$sigma2^0.5),
               pnorm(2.5,mean = frc_or$mean, sd=frc_or$model$sigma2^0.5),
               pnorm(3.5,mean = frc_or$mean, sd=frc_or$model$sigma2^0.5),
               pnorm(4.5,mean = frc_or$mean, sd=frc_or$model$sigma2^0.5))
    frc_arima <- c(diff(set_p), 1-sum(diff(set_p)))
    frc_arima_s <- sym_probs(frc_arima)
    frc_arima_ra <- ra_probs(frc_arima, frc_pos)
    
    #Save forecasts
    frc_list <- list(frc_b,
                     frc_f,
                     frc_ses, frc_arima,
                     frc_f_s,
                     frc_ses_s, frc_arima_s,
                     frc_f_ra,
                     frc_ses_ra, frc_arima_ra)
    
    #Evaluate forecasts (across all quintiles as per RPS and per quintile as per MSE)
    for (mid in 1:length(frc_list)){
      target_tmp <- as.numeric(testset[,c("Rank1","Rank2","Rank3","Rank4","Rank5")])
      target <- cumsum(target_tmp)
      frc <- cumsum(frc_list[[mid]])
      df <- data.frame(aid, xstart, mname[mid], mean((target-frc)^2), 
                       t((target_tmp-frc_list[[mid]])^2))
      colnames(df) <- c("symbol","round","model","rps",
                        "mse1","mse2","mse3","mse4","mse5")
      rps_sub <- rbind(rps_sub, df)
      
      forecasts_tmp <- data.frame(aid, xstart, mname[mid], t(frc_list[[mid]]))
      colnames(forecasts_tmp) <- c("Asset","Round","Model","Rank1","Rank2","Rank3","Rank4","Rank5")
      forecasts <- rbind(forecasts, forecasts_tmp)
    }
    
  }
}

tmp <- rps_sub ; tmp$symbol <- NULL
tmp <- ddply(tmp[,c("model","rps")], .(model), colwise(mean))
tmp[tmp$model=="frequency_s",]$rps/tmp[tmp$model=="frequency",]$rps
tmp[tmp$model=="ses_s",]$rps/tmp[tmp$model=="ses",]$rps
tmp[tmp$model=="arima_s",]$rps/tmp[tmp$model=="arima",]$rps

tmp <- rps_sub ; tmp$symbol <- NULL
tmp <- ddply(tmp[,c("model","rps","round")], .(model,round), colwise(mean))
plot(tmp[tmp$model=="frequency_s",]$rps/tmp[tmp$model=="frequency",]$rps, type="l")
abline(h=1, col="red")

plot(tmp[tmp$model=="ses_s",]$rps/tmp[tmp$model=="ses",]$rps, type="l")
abline(h=1, col="red")

plot(tmp[tmp$model=="arima_s",]$rps/tmp[tmp$model=="arima",]$rps, type="l")
abline(h=1, col="red")

#save.image("results_fromR_extended.Rdata")
load("results_fromR_extended.Rdata")

#RPS per model (barplot)
res3 <- ddply(rps_sub[,c("rps","model")], .(model), colwise(mean))
res3$model <- c("ARIMA","ARIMA_D","ARIMA_S","benchmark","FRQ","FRQ_D","FRQ_S","SES","SES_D","SES_S")
res3 <- res3[order(res3$rps),]
res3 <- res3[!(res3$model %in% "benchmark"),]
colnames(res3) <- c("Method","RPS")
res3$Method <- factor(res3$Method, levels=res3$Method)
setEPS()
postscript("rps_methods_extended.eps", width=6, height=4.5)
ggplot(res3, aes(x=Method, y=RPS)) +
  geom_bar(stat="identity") +
  theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5)) + 
  geom_hline(yintercept=0.16, linetype="dashed", size=1.2) + 
  coord_cartesian(ylim=c(0.15,max(res3$RPS)))
dev.off()
print(res3)