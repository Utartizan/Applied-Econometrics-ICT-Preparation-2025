# ============================================================================= #
#
# set system time to UTC
Sys.setenv(TZ='UTC') 
# load xts library for working with the time series data
library(xts)
# load data
lr_oil=read.delim("lr_oil.txt")
sr_oil=read.delim("sr_oil.txt")
# create xts objects
sr_oil=as.xts(sr_oil$CLOSE, order.by=as.POSIXct(sr_oil$Date))
lr_oil=as.xts(lr_oil$CLOSE, order.by=as.POSIXct(lr_oil$Date))
# do necessary data cleaning
BRENT=merge(sr_oil, lr_oil)
BRENT=na.locf(BRENT)
# plot OIL contracts
plot(BRENT$sr_oil, ylim=c(70, 95), col=2)
lines(BRENT$lr_oil, col=3)
# covert data to 15 minutes frequency
BRENT=to.minutes15(BRENT, OHLC=F)
# obtain high frequency returns
BRENT=log(BRENT)
BRENT=merge(BRENT, diff(BRENT)*100)
# do additional cleaning drop when both contracts have 0 returns
BRENT=BRENT[ BRENT[,3]!=0 | BRENT[,4]!=0,]
# assign column names in the final data set
colnames(BRENT)=c("sr_oil", "lr_oil", "sr_oilr", "lr_oilr")
# obtain residuals and plot them
cregr=lm(lr_oil ~ sr_oil, data=BRENT)
r=cregr$residuals
plot(r)
#
#
# do rolling strategy with ECM (similar to Brooks (2001))
myf=numeric(976)
acv=numeric(976)
for (i in 1:976){
  OILtemp=merge(BRENT[(0+i):(999+i),], 
                residuals(lm(lr_oil~sr_oil, data=BRENT[(0+i):(999+i),])))
  colnames(OILtemp)=c("sr_oil", "lr_oil", "sr_oilr", "lr_oilr", "r")
  ECMtemp=lm(OILtemp$lr_oilr~lag(OILtemp$sr_oilr, 1)+lag(OILtemp$lr_oilr, 1)
                                                    +lag(OILtemp$r, 1))
  myc=coef(ECMtemp)
  myf[i]=as.numeric(myc[1]+myc[2]*OILtemp[1000,3]+myc[3]*OILtemp[1000,4]
                          +myc[4]*OILtemp[1000,5])
  acv[i]=as.numeric(BRENT[(1000+i),4])
}
# forecasting accuracy of the ECM (positive sign detection)
sum(sign(myf)>0 & sign (acv)>0)/sum(sign (acv)>0)
# when did our model stop working?
par(mfrow=c(2,1))
plot(r[1001:1976], pch="*", cex=1.5, ylab="", xlab="", 
     main="Snapshot of Residauls from the Initial Model")
plot(acv, col=ifelse(sign(myf)>0 & sign (acv)>0, "green", "gray"), 
          pch=ifelse(sign(myf)>0 & sign (acv)>0, "+", "x"), cex=1.5,
          ylab="", xlab="",
          main="When Did Our Model Stop Working and Why?")
#
#
# ============================================================================= #