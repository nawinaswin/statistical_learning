library(xts)
library(zoo)
library(highfrequency)
library(lubridate)
library(tibble)
library(data.table)

rm(list=ls())

#Check system time
Sys.setenv(TZ='EST')
Sys.timezone()

#Set wd and read file location of Coke Stock trades and quotes
setwd("C:/Users/19089/Documents/Stevens MSF/Classes & Plan/3 - Fall 2022/Market Microstructure and Trading/Project/CodeNData")
getwd()

options(digits.secs=3)
######################################################### Load Data  ##############################################################
#Load and Filter XBTUSD for Quotes data
Raw_quotes <- fread("quote.csv.gz")
head(Raw_quotes)
#Note all currencies shown but we only want BTC USD Perp Swaps
class(Raw_quotes)     
#Raw Quotes are DF need to be changed to xts later. 

#Lets find the unique tag for BTC USD in Symbol
unique(Raw_quotes$symbol)
#Now make Quotes only for XBT USD to represent Bitcoin Perpetual Swaps

Raw_quotes <- subset(Raw_quotes, subset = symbol =='XBTUSD')
head(Raw_quotes)


#Load and Filter XBTUSD for Trades data.
Raw_trades <- fread("trade.csv.gz")
class(Raw_trades)     
Raw_trades <- subset(Raw_trades, subset = symbol =='XBTUSD')
head(Raw_trades)

############################################# Convert Trades & Quotes DF to XTS format ###########################################

#Quotes
Raw_quotes$timestamp <- as.POSIXct(x = Raw_quotes$timestamp, format = '%Y-%m-%dD%H:%M:%OS', tz = 'GMT')
attr(Raw_quotes$timestamp, "tzone") <- 'America/New_York'
attr(Raw_quotes$timestamp, "tzone")
force_tz(Raw_quotes$timestamp, tzone = 'EST')
head(Raw_quotes)

#Trades
Raw_trades$timestamp <- as.POSIXct(x = Raw_trades$timestamp, format = '%Y-%m-%dD%H:%M:%OS', tz = 'GMT')
attr(Raw_trades$timestamp, "tzone") <- 'America/New_York'
attr(Raw_trades$timestamp, "tzone")
force_tz(Raw_trades$timestamp, tzone = 'EST')
head(Raw_trades)
#Since trading is 24x7 and data is from GMT start of day, our data starts at 7pm EST. 

#Make the timestamp the index.
quote_time <- Raw_quotes$timestamp
quote_time_body <- subset(Raw_quotes, select = -c(timestamp))
quote_xts <- xts(x = quote_time_body, order.by = quote_time,tzone = "EST")
head(quote_xts)

trade_time <- Raw_trades$timestamp
trade_time_body <- subset(Raw_trades, select = -c(timestamp, side, tickDirection, trdMatchID,grossValue,homeNotional,foreignNotional,trdType))
trade_xts <- xts(x = trade_time_body, order.by = trade_time,tzone = "EST")
head(trade_xts)

#Only keep desired columns for HighFrequency later.
qdata <- subset(quote_xts, select = c(symbol, bidSize, bidPrice, askPrice, askSize))
head(qdata)
tdata <- subset(trade_xts, select = c(symbol, size, price))
head(tdata)

#Edit Column Names for HF package.
colnames(qdata) <- c("SYMBOL","BIDSIZ","BID","OFR","OFRSIZ")
colnames(tdata) <- c("SYMBOL","SIZE","PRICE")
head(qdata)
head(tdata)

# how many trades?
length(tdata$PRICE)
# how many quotes?
length(qdata$BID)

#Convert to TAQ Format. 
XBTUSD_TAQ <- matchTradesQuotes(tdata, qdata)
#Check first 50 rows
XBTUSD_TAQ[1:50,][,]

#It Works! XBTUSD in TAQ Format 
#Save in RData format.

save(XBTUSD_TAQ, file = "XBTUSD_TAQ.RData")

#Rename XBTUSD_TAQ to tqdata for ease here on out. 
tqdata <- XBTUSD_TAQ
####################################################   Explore Data ######################################################

#Explore data
# Number of trades
n.trades <- length(tqdata$SIZE) 
n.trades
#Total number of trades is 266,104

# plot the trade prices pt and the best-bid bt and best-ask prices at
# for the entire dataset
asks <- as.numeric(tqdata$OFR)
bids <- as.numeric(tqdata$BID)
mids <- 0.5*bids + 0.5*asks
lagmids <- lag.xts(mids, k = 300)

price <- as.numeric(tqdata$PRICE)
pmin <- min(as.numeric(tqdata$PRICE))
pmax <- max(as.numeric(tqdata$PRICE))

tradeSigns <- getTradeDirection(tqdata)

plot(as.numeric(tqdata$PRICE),col="red", type="l", ylab="Trade price", 
     xlab="Trade #", main="Trade price (11/8/22)")
lines(asks, type="l", col="blue")
lines(bids,type='l',col = "black")


# plot First 500 trade prices pt and the best-bid bt and best-ask prices at
plot(as.numeric(tqdata$PRICE[1:500,]),col="red", type="l", ylab="XBT Trade price (USD)", 
     xlab="First 500 Trades", main="XBTUSD Trades (First 500)")
lines(asks[1:500], type="l", col="blue")
lines(bids[1:500],type='l',col = "black")


############################################# Count within or at bid/ask  #################################################

#Count how many trades take place: 
# i) within the spread, 
# inside = 0
# for (i in 1:n.trades){
#   
#   if (XBTUSD_TAQ$PRICE[i] > XBTUSD_TAQ$BID[i]  && XBTUSD_TAQ$PRICE[i] < XBTUSD_TAQ$OFR[i]){
#     inside = inside + 1
#   }
#   
# }
# inside
# XXXXXXXXXXX within spread


# # ii) at bid, 
# atbid = 0
# for (i in 1:n.trades){
#   
#   if (XBTUSD_TAQ$PRICE[i] == XBTUSD_TAQ$BID[i]){
#     atbid = atbid + 1
#   }
#   
# }
# atbid
# # 16,929 trades at the bid 
# 
# #iii) at ask
# atask = 0
# for (i in 1:n.trades){
#   
#   if (XBTUSD_TAQ$PRICE[i] == XBTUSD_TAQ$OFR[i]){
#     atask = atask + 1
#   }
#   
# }
# atask
# XXXXXXXXXXXXX at the Ask


######################################### Spread Measures (Not time bucket) ######################################

#Find Spreads at for row of taq data...
#Computing absolute quoted spread
abspread <- asks - bids
head(abspread)
plot(abspread)

#computing relative spread
relspread <- (asks/bids) -1
head(relspread)
plot(relspread)

#computing log spread
logspread <- log10(asks) - log10(bids)
head(logspread)
plot(logspread)

#computing effective spread
effspread <- 2*tradeSigns*(price - mids)
head(effspread)
plot(effspread)

#computing realized spread
realspread <- 2*tradeSigns*(price-lagmids)            #CHECK lag                                                                    
head(realspread)
plot(realspread)

#Add these spreads to tqdata
tqdata$ABSpread <- abspread
tqdata$Relspread <- relspread
tqdata$LOGspread <- logspread
tqdata$Effspread <- effspread
tqdata$RealizedSpread <- realspread

head(tqdata)
tail(tqdata)



#Computing avg of 3 measures over all transactions
avAbspread <- mean(abspread)
avAbspread
avrelspread <- mean(relspread)
avrelspread
avlogspread <- mean(logspread)
avlogspread
avEffspread <- mean(tradeSigns*(price - mids))
avEffspread
avRealizSpread <- function(lag){
  2*mean(tradeSigns[1:(n.trades-lag)]*(price[1:(n.trades-lag)]-mids[-(1:lag)]))
}
avRealizSpread(300)                         #Confirm 300 for lag or not!
                                                                                  

#From comparison, it is clear that the 
# average spread is 6.913607
# average rel spread is much smaller at 0.0003718155
# average ln spread is even smaller at 0.0001613015
# average effective spread is 2.352589
# average realized spread for a 5 min lag is 2.72



######################################### TIME BUCKET SPREAD ANALYSIS (60 min bucket) ######################################

#Split data into hourly groups from 7pm EST 11/7 to 7pm EST 11/8. 
xbt_hourly <- split(tqdata, f = 'hours',k = 1)
head(xbt_hourly)

#Absolute Quoted Spreads
abs <- c()
for (i in (1:24)){
  abs <- append(abs,mean(xbt_hourly[[i]]$ABSpread))
  i = i+1
}
plot(abs,ylab = 'Avg Absolute Spread',xlab='Trading Hours from 00:00 GMT',main = 'Hourly Average Absolute Spread')


#Relative Spreads
rels <- c()
for (i in (1:24)){
  rels <- append(rels,mean(xbt_hourly[[i]]$Relspread))
  i = i+1
}
plot(rels,ylab = 'Avg Relative Spread',xlab='Trading Hours from 00:00 GMT',main = 'Hourly Average Relative Spread')

#Log Spreads
logs <- c()
for (i in (1:24)){
  logs <- append(logs,mean(xbt_hourly[[i]]$LOGspread))
  i = i+1
}
plot(logs,ylab = 'Avg Log Spread',xlab='Trading Hours from 00:00 GMT',main = 'Hourly Average Log Spread')

#Effective Spreads
effs <- c()
for (i in (1:24)){
  effs <- append(effs,mean(xbt_hourly[[i]]$Effspread))
  i = i+1
}
plot(effs,ylab = 'Avg Effective Spread',xlab='Trading Hours from 00:00 GMT',main = 'Hourly Average Effective Spread')

#Realized Spreads
realized <- c()
for (i in (1:24)){
  realized <- append(realized,mean(xbt_hourly[[i]]$RealizedSpread))
  i = i+1
}
plot(realized,ylab = 'Avg Realized Spread',xlab='Trading Hours from 00:00 GMT',main = 'Hourly Average Realized Spread')

#From these plots, we note that the spreads increased drastically during the volatile period during the price drop 
#from Trading Hour Number 16-20.

############################################## INTRADAY LIQUIDITY DYNAMICS #########################################

#Volume over the day
plot(tqdata$SIZE)

#Plot Realized and Effective Spreads using HF getLiqMeasures
liq_meas <- getLiquidityMeasures(tqdata, win=300)

plot(as.numeric(liq_meas$realizedSpread), type="b",
     main="realized spread")


plot(as.numeric(liq_meas$effectiveSpread), type="b",
     main="effective spread")





############################################# Volatility Estimation ##################################################

#This follows class example VolEstimationHFT.R
px <- as.numeric(tqdata$PRICE)
log.px <- log(px)
dp = diff(px)


realizedVar <- function(q){rCov(diff(px, lag=q, differences=1))/q}

realizedVar(1)
sqrt(realizedVar(1))

# compute the signature plot RV(lag)

rv_data <- NULL

for(q in 1:200){
  
  rv_data <- c(rv_data, realizedVar(q))
  
}

#There are 1440 mins in a trading day here (24x7)
q5min <- n.trades*5/1440

rv5 = realizedVar(q5min)

sqrt(rv5)


covdp <- acf(dp, lag.max=10, 
             type="covariance", plot=TRUE,
             main="Autocovariance of price changes")

gamma0 <- covdp$acf[1]
gamma1 <- covdp$acf[2]

sig2u = gamma0 + 2*gamma1 

rvRoll <- sig2u*n.trades

sigRoll <- sqrt(sig2u*n.trades)

plot(rv_data, type ="l", 
     main="Signature plot for prices + Roll",
     ylim=c(0,25))
abline(h=rv5,col="red")
abline(h=rvRoll,col="blue")



##############################################################
## Compare with the two-scale estimator of ZMA 
# ZMA works with log(price), so first take exp.
class(tqdata$PRICE)

pexp <- xts(exp(as.numeric(tqdata$PRICE)),
            order.by=as.POSIXct(tqdata[,1]))
names(pexp) <- "PRICE"
head(pexp)

#rvts <- rTSCov(pdata = tqdata$PRICE,K=104,J=1)
rvts <- rTSCov(pdata = pexp,K=104,J=1)

rvts

abline(h=rvts, col="green") #overlay on top of the log(price) signature plot
sqrt(rvts)
