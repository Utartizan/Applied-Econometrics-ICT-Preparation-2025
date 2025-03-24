# ============================================================================= #
set.seed(1)
#
# load packages (note that some packages require installation if not installed
# previously)
library(readr)
library(forecast)
library(rugarch)
library(xts)
# load NASDAQ data
NDX=read_csv("NDX.csv",  col_types = cols(Date  =col_date(format = "%d/%m/%Y"), 
                                          Price =col_number()))
#
# transform loaded NASDAQ data to xts object (often it is convenient data type
# at data processing page, we will use this objects in the next seminar more)
NDXR=as.xts(NDX$Price, order.by=NDX$Date)
NDXR=na.omit(diff(log(NDXR))*100)       # obtain logarithmic returns           
#
# plot raw and squared log-returns, their acfs and pacfs
par(mfrow=c(2,3))
plot(NDXR, main="NDX")
acf(NDXR, main="")
pacf(NDXR, main="")
#
plot(NDXR^2, main="sqrd. NDX")
acf(NDXR^2, main="")
pacf(NDXR^2, main="")
#
# fit ARIMA to log-returns, obtain its residuals and plot squared residuals, its
# acfs and pacfs
auto.arima(NDXR, max.p = 15, max.q = 15)
ar1reg=Arima(NDXR, order=c(1,0,0), include.mean = TRUE)
ar1res=residuals(ar1reg)
#
par(mfrow=c(1,3))
plot(ar1res^2, ylab="NDX sqrd. res.", xlab="", main="")
acf(ar1res^2, main="")
pacf(ar1res^2, main="")
#
# ============================================================================= #
# predefine 3 GARCH specification for estimations
# GARCH 1,1
model1=ugarchspec(variance.model=list(model="sGARCH", garchOrder=c(1, 1)),
                  mean.model=list(armaOrder=c(1, 0),  include.mean=TRUE), 
                  distribution.model="sstd")
# EGARCH 1,1
model2=ugarchspec(variance.model=list(model="eGARCH", garchOrder=c(1, 1)),
                  mean.model=list(armaOrder=c(1, 0), include.mean=TRUE), 
                  distribution.model="sstd")
# GJR GARCH 1,1
model3=ugarchspec(variance.model=list(model = "gjrGARCH", garchOrder=c(1, 1)),
                  mean.model=list(armaOrder = c(1, 0), include.mean=TRUE), 
                  distribution.model="sstd")
#
# fit 3 predefined GARCH models
fit1=ugarchfit(model1, NDXR)
fit2=ugarchfit(model2, NDXR)
fit3=ugarchfit(model3, NDXR)
#
# GARCH output
fit1 # GARCH 
fit2 # EGARCH
fit3 # GJR GARCH
# 
# we can also produce various plots with the GARCH objects; for example,
plot(fit3)
# select the plot that interests you from the console menu (0 to exit menu)
#
# ============================================================================= #
# 
# perform rolling VaR forecasts with 3 GARCH models (*estimations start in 2018)
fitroll1=ugarchroll( # GARCH 
  model1, data=NDXR, n.start = 3272, refit.every = 5, 
  refit.window = c("moving"), window.size = 1000, solver = "hybrid", 
  calculate.VaR = TRUE, VaR.alpha = c(0.01, 0.05), keep.coef = F
)
#
fitroll2=ugarchroll( # EGARCH
  model2, data=NDXR, n.start = 3272, refit.every = 5, 
  refit.window = c("moving"), window.size = 1000, solver = "hybrid", 
  calculate.VaR = TRUE, VaR.alpha = c(0.01, 0.05), keep.coef = F
)
#
fitroll3=ugarchroll( # GJR GARCH
  model3, data=NDXR, n.start = 3272, refit.every = 5, 
  refit.window = c("moving"), window.size = 1000, solver = "hybrid", 
  calculate.VaR = TRUE, VaR.alpha = c(0.01, 0.05), keep.coef = F
)
#
# we can also produce various plots with the rolling GARCH objects; for example,
plot(fitroll3)
# select the plot that interests you from the console menu (0 to exit menu)
#
# backtest VaR forecast appropriateness
# GARCH
VaRTest(alpha = 0.01, actual=fitroll1@forecast$VaR$realized, 
        VaR = fitroll1@forecast$VaR$`alpha(1%)`)
# EGARCH
VaRTest(alpha = 0.01, actual=fitroll2@forecast$VaR$realized, 
        VaR = fitroll2@forecast$VaR$`alpha(1%)`)
# GJR GARCH
VaRTest(alpha = 0.01, actual=fitroll3@forecast$VaR$realized, 
        VaR = fitroll3@forecast$VaR$`alpha(1%)`)
#
# ============================================================================= #