# ============================================================================= #
#
set.seed(1)
# load packages (install those that were not installed previously)
library(readxl)
library(urca)
library(vars)
library(tseries)
library(xts)
library(forecast)
library(quantmod)
library(tsDyn)
#
# load our UKinflation data set from Week 10
UKInflation=read_excel("UKInflation.xls", col_types=c("date", 
                                                      "numeric", "numeric"))
# Transform inflation data (recall what steps we took to achieve stationarity)
INFR=lm(INF~1, data=UKInflation)
INFN=residuals(INFR)
INFN=INFN[-1]
# Transform oil prices data
BRENTN=(UKInflation$BRENT[-1]-UKInflation$BRENT[-349])/UKInflation$BRENT[-1]*100
#
# ============================================================================= #
#
# Create xts objects for new INFN and BRENTN variables
INFN=as.xts(INFN, order.by=UKInflation$DATE[-1])
BRENTN=as.xts(BRENTN, order.by=UKInflation$DATE[-1])
# ... and combine them into one (joint) xts object
INFOIL=merge(INFN, BRENTN)
#
# ============================================================================= #
# VAR modelling steps:
# Identify appropriate lag order for our VAR model
laglength=VARselect(INFOIL, lag.max = 10, type="const")
# Estimate VAR model as per AIC output
VAR1=VAR(INFOIL, p=4, type="const")
# Obtain summary for our VAR(4) model
summary(VAR1)
#
# Perform GC tests for INFLN and BRENTN 
GC1=causality(VAR1, cause = "BRENTN")
GC2=causality(VAR1, cause = "INFN")
# Construct INFN impulse response function for shock in BRENTN
IRUKINFfromOIL=irf(VAR1, impulse="BRENTN", response="INFN", n.ahead=24, boot=TRUE)
# ... and plot obtained IRF
plot(IRUKINFfromOIL)
# Obtain forecast with our VAR(4) model for several months ahead
FOR1=predict(VAR1, n.ahead = 10)
FOR1
#  ... and plot obtained inflation forecast
plot(FOR1, names="INFN" )
# or
fanchart(FOR1,names="INFN")
#
# ============================================================================= #
# ECM and VECM
# obtain data on 3 months and 6 months interest rates
getSymbols('DTB3', src='FRED')
getSymbols('DTB6', src='FRED')
# subsample interest rates for a more recent period 
DTB3.sub=DTB3['2014-03-31/2021-03-31']
DTB6.sub=DTB6['2014-03-31/2021-03-31']
# plot 3 & 6 months treasuries / interest rates; what do you observe?
plot(DTB6.sub, main="")
lines(DTB3.sub, col='red')
# convert data to numeric and combine them in one data set
DTB3and6=na.omit(merge(DTB3.sub, DTB6.sub))
# cointegration test step 1 (simple procedure)
cregmod1=lm(DTB3.sub~DTB6.sub, data=as.data.frame(DTB3and6))
DTB3and6res=as.numeric(cregmod1$residuals)
plot(DTB3and6res) # optional, but it is always good to have a look into residuals
# cointegration test step 2
po.test(DTB3and6, demean = TRUE, lshort = TRUE)
# cointegration step 3
# prepare a dataset and assign column names
dstep3=cbind(DTB3and6, diff(DTB3and6$DTB3), diff(DTB3and6$DTB6), DTB3and6res)
colnames(dstep3)=c("DTB3", "DTB6", "DTB3D", "DTB6D", "RES")
# run final step regression
DTB3ECM=lm(dstep3$DTB3D~lag(dstep3$DTB3D,1)+lag(dstep3$DTB6D,1)+lag(dstep3$RES,1))
summary(DTB3ECM)
# cointegration Johansen procedure
JoTest1=ca.jo(DTB3and6, type = c("trace"), ecdet = c("none"), K = 2)
summary(JoTest1)
# VECM model for 3 month and 6 month interest rates
DTB3and6VECM=VECM(DTB3and6, 1, r=1, estim="ML")
summary(DTB3and6VECM)
summary(DTB3ECM) # you can compare coefficients to the simple (one dim) model
#
# VECM can be converted to VAR (see lecture slides) and similar measures can be
# constructed as in the first part of the session (see above)
DTB3and6VAR=vec2var(DTB3and6VECM, r=1)
#
# ============================================================================ #