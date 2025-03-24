# ============================================================================= #
set.seed(1)
# load packages
library("AER")
library("plm")
# load first data set
data("Grunfeld", package="AER")
# explore data set and confirm that this is a balanced panel
View(Grunfeld)
# 
reg1=lm(invest~value+capital, data=Grunfeld)                                    # treat data as a cross section
reg2=lm(invest~value+capital+as.factor(firm), data=Grunfeld)                    # panel regression with Fixed individual effects
reg3=lm(invest~value+capital+as.factor(firm)                                    # panel regression with Fixed individual and time effects
                            +as.factor(year), data=Grunfeld)    
#
reg4=plm(invest~value+capital, model="pooling",                                 # running panel regressions with plm package:
                               data=Grunfeld, index = c("firm", "year"))        # 1. cross-section
reg5=plm(invest~value+capital, model="within",                                  
                               data=Grunfeld, index = c("firm", "year"))        # 2. panel regression with Fixed individual effects
reg6=plm(invest~value+capital, model="within", 
                               data=Grunfeld, index = c("firm", "year"),        # 3. panel regression with Fixed individual and time effects
                               effect = "twoways")
#
summary(reg1)                                                                   # compare output for the manual FE output and "turn key" solution by plm package
summary(reg4)
#
summary(reg2)
summary(reg4)
#
summary(reg3)
summary(reg6)
#
# ============================================================================= #
#
reg7=plm(invest~value+capital, model="random", 
                               data=Grunfeld, index = c("firm", "year"))        # RE model
summary(reg5)                                                                   # compare RE and FE (individual effect) models
summary(reg7)
# Hausman test for endogeneity (testing RE vs FE); RE is more efficient (better)
# note that if control for time effect we obtain the opposite results
phtest(reg5, reg7)
#
# ============================================================================= #
# simplified R code from Week 3 session; running CAPM as a panel 
# part 1. This section will is provided to students prior the session
library(tidyverse)
library(tidyquant)
library(xts)
library(readr)
#
tickers=c("AAPL", "MSFT", "AMZN")
#
getSymbols(tickers, src="yahoo", from="2013-01-01", to="2019-12-31", auto.assign = T, warnings = F)
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
# part 2. This section is performed and analysed by students; students should be 
# aware of the steps needed to implement the analysis
#
reg8=lm(Ereturns~MKT+as.factor(Ticker), data=CAPMpanel)
reg9=lm(Ereturns~MKT+MKT*as.factor(Ticker), data=CAPMpanel)
#
reg10=lm(AAPL~MKT, data =CAPMdata)
reg11=lm(AMZN~MKT, data =CAPMdata)
reg12=lm(MSFT~MKT, data =CAPMdata)
#
summary(reg9)
summary(reg10)
summary(reg11)
#
library(car)
linearHypothesis(reg9, c("as.factor(Ticker)AMZN",
                         "as.factor(Ticker)MSFT",
                         "MKT:as.factor(Ticker)AMZN",
                         "MKT:as.factor(Ticker)MSFT"))
#
# ============================================================================= #
# More guns less crime? 
data("Guns")
?Guns
#
reg11=plm(log(violent)~law, data=Guns, model="pooling", index=c("state", "year"))
summary(reg11)
#
reg12=plm(log(violent)~law, data=Guns, model="within", 
          index=c("state", "year"))
summary(reg12)
#
reg13=plm(log(violent)~law, data=Guns, model="within", 
          index=c("state", "year"), effect="twoways")
summary(reg13)
#
summary(lm(log(violent)~law+as.factor(state), data=Guns ))                      # identical to reg 12 
summary(lm(log(violent)~law+as.factor(state)+as.factor(year), data=Guns ))      # identical to reg 13 
#
# ============================================================================= #
# Less alcohol more fatalities (car accidents)?   
data(Fatalities)
?Fatalities
# Compute Fatalities rate
Fatalities$fatal_rate=Fatalities$fatal / Fatalities$pop * 10000
# Run and compare FE and cross-section
reg14=plm(fatal_rate~beertax, data=Fatalities, index=c("state", "year"),
                              model="pooling")
summary(reg14)
#
reg15=plm(fatal_rate~beertax, data=Fatalities, index=c("state", "year"),
                              model="within")
summary(reg15)
#
reg16=plm(fatal_rate~beertax, data=Fatalities, index=c("state", "year"), 
                              model = "within", effect = "twoways")
summary(reg16)
#
summary(lm(fatal_rate~beertax+as.factor(state), data=Fatalities ))              # identical to reg 15 
summary(lm(fatal_rate~beertax+as.factor(state)+as.factor(year), data=Guns ))    # identical to reg 16
#
# Was there an impact from beer tax law introduction in the U.S.? 
Fatalities1982=subset(Fatalities, year == "1982")
Fatalities1988=subset(Fatalities, year == "1988")
diff_fatal_rate=Fatalities1988$fatal_rate - Fatalities1982$fatal_rate
diff_beertax=Fatalities1988$beertax - Fatalities1982$beertax
#
reg17=lm(diff_fatal_rate~diff_beertax)
summary(reg17)
#
# ============================================================================= #