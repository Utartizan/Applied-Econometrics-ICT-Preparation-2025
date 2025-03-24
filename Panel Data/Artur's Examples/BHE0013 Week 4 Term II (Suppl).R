# ============================================================================= #
# supplementary R script
#
library(tidyverse)
library(tidyquant)
library(xts)
library(readr)
#
tickers=c("AAPL", "MSFT", "AMZN")
#
getSymbols(tickers, src="yahoo", from="2013-01-01", to="2019-12-31", auto.assign=T, warnings=F)
CAPMfactors=read.csv("CAPMfactors.txt", sep="")
CAPMfactors=CAPMfactors[1040:1122, ]
# 
prices=map(tickers, function(j) Ad(get(j)))
prices=reduce(prices, merge)
colnames(prices)=tickers
#
mprices=apply(prices, 2, function(data) to.monthly(data)[,4])
#
# returns ... % change ...
#
returns=apply(mprices, 2, function(data) diff(data)/data[-length(data)] )*100
head(CAPMfactors)
head(returns)
#
returns=apply(returns, 2, function(data) data-CAPMfactors[,5] )
CAPMdata=cbind(returns, CAPMfactors[,2])
# organising data into a panel; note that this can be done in Excel manually
colnames(CAPMdata)=c("AAPL", "MSFT", "AMZN", "MKT")
#
CAPMpanel=as.matrix(rbind(
  cbind(rownames(CAPMdata[,c(1,4)]), CAPMdata[,c(1,4)], rep("AAPL", nrow(CAPMdata))),
  cbind(rownames(CAPMdata[,c(1,4)]), CAPMdata[,c(2,4)], rep("MSFT", nrow(CAPMdata))),
  cbind(rownames(CAPMdata[,c(1,4)]), CAPMdata[,c(3,4)], rep("AMZN", nrow(CAPMdata)))
))
#
colnames(CAPMpanel)=c("Date", "Ereturns", "MKT", "Ticker")
rownames(CAPMpanel)=NULL
CAPMpanel=as.data.frame(CAPMpanel)
CAPMpanel$Ereturns=as.numeric(CAPMpanel$Ereturns)
CAPMpanel$MKT=as.numeric(CAPMpanel$MKT)
CAPMdata=as.data.frame(CAPMdata)
#
# ============================================================================= #