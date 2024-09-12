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
    
    if (nrow(temp[temp$Position<=20,])>0){
      temp[temp$Position<=20,]$Rank <- 1
      temp[temp$Position<=20,]$Rank1 <- 1
    }
    if (nrow(temp[(temp$Position>20)&(temp$Position<=40),])>0){
      temp[(temp$Position>20)&(temp$Position<=40),]$Rank <- 2
      temp[(temp$Position>20)&(temp$Position<=40),]$Rank2 <- 1
    }
    if (nrow(temp[(temp$Position>40)&(temp$Position<=60),])>0){
      temp[(temp$Position>40)&(temp$Position<=60),]$Rank <- 3
      temp[(temp$Position>40)&(temp$Position<=60),]$Rank3 <- 1
    }
    if (nrow(temp[(temp$Position>60)&(temp$Position<=80),])>0){
      temp[(temp$Position>60)&(temp$Position<=80),]$Rank <- 4
      temp[(temp$Position>60)&(temp$Position<=80),]$Rank4 <- 1
    }
    if (nrow(temp[temp$Position>80,])>0){
      temp[temp$Position>80,]$Rank <- 5
      temp[temp$Position>80,]$Rank5 <- 1
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
asset_data<- read.csv("assets_m6.csv", stringsAsFactors = F)
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

#Present asset ranks in a heatmap
library(heatmaply)
stats2 <- stats[,c("Rank1","Rank2","Rank3","Rank4","Rank5")]
rownames(stats2) <- stats$Asset
stats2 <- stats2[order(stats2$Rank1+stats2$Rank5),]
stats2 <- stats2[seq(1,100,3),] #Keep only 1/3 of the assets to improve readability
p <- heatmaply(stats2,
               dendrogram = "none",
               xlab = "", ylab = "",
               main = "",
               scale = "column",
               margins = c(60,100,40,20),
               grid_color = "white",
               grid_width = 0.00001,
               titleX = FALSE,
               hide_colorbar = TRUE,
               branches_lwd = 0.01,
               heatmap_layers = theme(axis.line=element_blank()),
               labRow = rownames(stats2),
               fontsize_row = 8
)
p

#Forecasting
aid <- unique(asset_data$symbol)[1] ; xstart <- 7
rps_sub <- NULL
mname <- c("frequency", "ses", "arima")
mname <- c("benchmark", mname, paste0(mname,"_s"), paste0(mname,"_ra"))

forecasts <- NULL
aid <- "XLY" ; xstart <- 11
for (aid in unique(asset_data$symbol)){
  
  x <- ranks_of_assets[ranks_of_assets$ID==aid,]
  xrange <- c(4:51) #Repeat evaluation for 48 consecutive weeks
  
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

save.image("results_fromR.Rdata")
#load("results_fromR.Rdata")

#RPS per asset (FRQ as base)
tmp <- rps_sub[,c("rps","symbol","model")]
tmp <- tmp[tmp$model=="frequency",] ; tmp$model <- NULL #Select model
res1 <- ddply(tmp, .(symbol), colwise(mean))
colnames(res1) <- c("Asset","RPS")
res1$col <- c(rep("blue", 50), rep("red", 50))
res1 <- merge(res1, stats[,c("Asset","Rank1","Rank2","Rank3","Rank4","Rank5")], by="Asset")
res1 <- res1[order(res1$Rank1 + res1$Rank5),]
res1$Asset <- factor(res1$Asset, levels=res1$Asset)

tmp2 <- rps_sub[,c("rps","symbol","model")]
tmp2 <- tmp2[tmp2$model=="benchmark",] ; tmp2$model <- NULL #Select model
tmp2 <- ddply(tmp2, .(symbol), colwise(mean)) ; colnames(tmp2) <- c("Asset","benchmark")
res1 <- merge(res1, tmp2, by="Asset")
res1 <- res1[order(res1$Rank1 + res1$Rank5),]
rownames(res1) <- NULL

setEPS()
postscript("redblue.eps", width=7, height=3.5)
resp <- res1[seq(1,100,3),] #Keep only 1/3 of the assets to improve readability
ggplot(data = NULL, aes(group = col)) +
  geom_bar(data=resp, aes(Asset, RPS, fill=col), position="dodge", stat="identity") +
  geom_point(data=resp, aes(Asset, benchmark, group=col), position = position_dodge(width = 0.9), shape=8, size=2, show.legend=FALSE) +
  theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5,), legend.position = "none")
dev.off()

#RPS per approach and round (FRQ as base)
res2 <- ddply(rps_sub[,c("rps","round","model")], .(round,model), colwise(mean))
res2 <- res2[res2$model %in% c("frequency_ra","frequency_s","frequency"),]
colnames(res2) <- c("Round","Model","RPS")
res2$Round = res2$Round-3
res2[res2$Model=="frequency",]$Model <- "Base"
res2[res2$Model=="frequency_ra",]$Model <- "Direction"
res2[res2$Model=="frequency_s",]$Model <- "Symmetric"
setEPS()
postscript("rounds.eps", width=7, height=3.5)
ggplot(data=res2, aes(x=Round, y=RPS, col=Model)) +
  geom_line(aes(col=Model))+
  geom_point()+
  geom_hline(yintercept=0.16, linetype="dashed", size=1.2) 
dev.off()


#RPS per model (barplot)
res3 <- ddply(rps_sub[,c("rps","model")], .(model), colwise(mean))
res3$model <- c("ARIMA","ARIMA_D","ARIMA_S","benchmark","FRQ","FRQ_D","FRQ_S","SES","SES_D","SES_S")
res3 <- res3[order(res3$rps),]
res3 <- res3[!(res3$model %in% "benchmark"),]
colnames(res3) <- c("Method","RPS")
res3$Method <- factor(res3$Method, levels=res3$Method)
setEPS()
postscript("rps_methods.eps", width=6, height=4.5)
ggplot(res3, aes(x=Method, y=RPS)) +
  geom_bar(stat="identity") +
  theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5)) + 
  geom_hline(yintercept=0.16, linetype="dashed", size=1.2) + 
  coord_cartesian(ylim=c(0.15,max(res3$RPS)))
dev.off()
print(res3)


#Frequency of model outperforming the benchmark
frq <- rep(0,length(unique(rps_sub$model)))
names(frq) <- unique(rps_sub$model)
for (i in unique(rps_sub$symbol)){
  for (j in unique(rps_sub$round)){
    tmp <- rps_sub[rps_sub$symbol==i & rps_sub$round==j,]
    for (m in unique(rps_sub$model)){
      if (tmp[tmp$model==m,]$rps<tmp[tmp$model=="benchmark",]$rps){
        frq[m] <- frq[m]+1
      }
    }
  }
}
n <- length(unique(rps_sub$round))*length(unique(rps_sub$symbol))
frq <- frq/n
print(frq)

#Relationship between forecast accuracy and average/deviation values of returns/ranks
setEPS()
postscript("cors.eps", width=8.5, height=7)
k <- merge(res1, stats[,c("Asset","AvgReturn","AvgRank","SdReturn","SdRank")], by="Asset")
par(mfrow=c(2,2))
plot(k$AvgReturn,k$RPS, ylab="RPS", xlab="Average Returns")
abline(lm(RPS ~ AvgReturn, data=k), col="red")
legend("bottomleft", legend=paste("r=",round(cor(k$AvgReturn,k$RPS),2)),
       col=c("red"), cex=1.2)
plot(k$SdReturn,k$RPS, ylab="RPS", xlab="Variation of Returns")
abline(lm(RPS ~ SdReturn, data=k), col="red")
legend("bottomright", legend=paste("r=",round(cor(k$SdReturn,k$RPS),2)),
       col=c("red"), cex=1.2)
plot(k$AvgRank,k$RPS, ylab="RPS", xlab="Average Rank")
abline(lm(RPS ~ AvgRank, data=k), col="red")
legend("bottomleft", legend=paste("r=",round(cor(k$AvgRank,k$RPS),2)),
       col=c("red"), cex=1.2)
plot(k$SdRank,k$RPS, ylab="RPS", xlab="Variation of Rank")
abline(lm(RPS ~ SdRank, data=k), col="red")
legend("topleft", legend=paste("r=",round(cor(k$SdRank,k$RPS),2)),
       col=c("red"), cex=1.2)
dev.off()

#Relationship between forecast accuracy and frequency of symmetric ranks
setEPS()
postscript("relation.eps", width=9, height=5)
k$R15 <- k$Rank1+k$Rank5
k$R24 <- k$Rank2+k$Rank4
k$R234 <- k$Rank2+k$Rank3+k$Rank4
par(mfrow=c(1,2))
plot(k$R15,k$RPS, ylab="RPS", xlab="Rank1 & Rank5 frequency")
abline(lm(RPS ~ R15, data=k), col="red")
legend("topleft", legend=paste("r=",round(cor(k$R15,k$RPS),2)),
       col=c("red"), cex=1.2)
plot(k$R24,k$RPS, ylab="RPS", xlab="Rank2 & Rank4 frequency")
abline(lm(RPS ~ R24, data=k), col="red")
legend("topright", legend=paste("r=",round(cor(k$R24,k$RPS),2)),
       col=c("red"), cex=1.2)
dev.off()


#Investigate MSE improvements per quintile
setEPS()
postscript("mse_quintile.eps", width=8.5, height=4)
par(mfrow=c(1,3))
cname <- c("frequency", "ses", "arima")
for (i in 1:3){
  base <- rps_sub[rps_sub$model==cname[i],c("symbol","mse1","mse2","mse3","mse4","mse5")]
  base <- ddply(base, .(symbol), colwise(mean))
  com1 <- rps_sub[rps_sub$model==paste0(cname[i],"_ra"),c("symbol","mse1","mse2","mse3","mse4","mse5")]
  com1 <- ddply(com1, .(symbol), colwise(mean))
  #Average improvement per quintile 
  print(colMeans(com1[,2:6])/colMeans(base[,2:6]))
  
  #Distribution of improvements per asset
  k_p <- na.omit(data.frame(com1$symbol,
                            com1$mse1/base$mse1,
                            com1$mse2/base$mse2,
                            com1$mse3/base$mse3,
                            com1$mse4/base$mse4,
                            com1$mse5/base$mse5))
  colnames(k_p) <- c("Asset","Q1","Q2","Q3","Q4","Q5")
  if (cname[i]=="frequency"){pname <- "FRQ"} #Rename models
  if (cname[i]=="arima"){pname <- "ARIMA"}
  if (cname[i]=="ses"){pname <- "SES"}
  boxplot(k_p[,c("Q1","Q2","Q3","Q4","Q5")], main=pname,
          ylim=c(0.7,1.15), ylab="RelMSE")
  abline(h=1)
}
dev.off()

#################################################
#Investments - Utility of forecasts
inv_results <- data.frame(matrix(NA, ncol = 4, nrow = length(mname)))
colnames(inv_results) <-c("Model","Return","Risk","IR")
inv_results$Model <- mname
for (mid in mname){
  returns <- c()
  for (xstart in xrange){
    
    pid = sub_period_info$sub_period_name[xstart+1]
    hist_data_tmp <- asset_data[asset_data$date>=sub_period_info[sub_period_info$sub_period_name==pid,]$sub_period_start &
                                  asset_data$date<=sub_period_info[sub_period_info$sub_period_name==pid,]$sub_period_end,]
    
    ret_tot <- NULL
    for (aid in unique(hist_data_tmp$symbol)){
      tr <- hist_data_tmp[hist_data_tmp$symbol==aid,]
      ret <- diff(tr$price)/tr$price[1:(nrow(tr))-1]
      ret <- data.frame(aid,t(ret))
      ret_tot <- rbind(ret_tot, ret)
    }
    colnames(ret_tot)[1] <- "Asset"
    
    frc <- forecasts[forecasts$Round==xstart & forecasts$Model==mid,]
    
    frc$gain <- frc$Rank5
    frc$weight <- frc$gain/sum(frc$gain)
    
    ret_m <- merge(ret_tot, frc, by="Asset")
    ret_m <- c(log(1+sum(ret_m$X1*ret_m$weight)), log(1+sum(ret_m$X2*ret_m$weight)), 
               log(1+sum(ret_m$X3*ret_m$weight)), log(1+sum(ret_m$X4*ret_m$weight)), 
               log(1+sum(ret_m$X5*ret_m$weight)))
    
    returns <- c(returns, ret_m)
  }
  inv_results[inv_results$Model==mid,]$Return <- sum(returns)
  inv_results[inv_results$Model==mid,]$Risk <- sd(returns)
  inv_results[inv_results$Model==mid,]$IR <- sum(returns)/sd(returns)
  
}
colnames(inv_results)[1] <- "Method"
inv_results$Method <- c("Benchmark","FRQ","SES","ARIMA","FRQ_S","SES_S","ARIMA_S","FRQ_D","SES_D","ARIMA_D")
inv_results <- merge(inv_results, res3, by="Method")
inv_results[order(inv_results$Method),]

setEPS()
postscript("IR_RPS.eps", width=9, height=6)
par(mfrow=c(1,1))
plot(inv_results$RPS,inv_results$IR, ylim = c(-4,2.5), ylab = "IR", xlab = "RPS")
abline(h=0, lty=2)
abline(lm(inv_results$IR~inv_results$RPS), col="red")
text(inv_results$RPS, inv_results$IR,inv_results$Method, pos=3)
dev.off()
