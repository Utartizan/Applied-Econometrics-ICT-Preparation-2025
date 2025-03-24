# ============================================================================= #
set.seed(1)
# Remember to install packages if not installed previously
#
# install.packages("QRM")
# install.packages("mev")
#
library(QRM)                                                                    # load packages
library(mev)
#
data("danish")                                                                  # load Danish fire insurance claims
fireinsclaims=danish$FIRE.LOSSES
#
fit.gpd(fireinsclaims, show="T")                                                # obtain parameters for GPD for raw Danish data
fit.gpd(log(fireinsclaims), show="T")                                           # obtain parameters for GPD for log transformed Danish data
#
est_scale=2.5780                                                                # save estimated parameters for raw data
est_shape=0.1863
est_pareto=dgp(seq(0,300, length.out=1000), loc=0,                              # obtain density for visualisation
                                            scale=est_scale,
                                            shape=est_shape,
                                            log=F)
#
est_scale_log=0.8613                                                            # save estimated parameters for log transformed data
est_shape_log=-0.0897
est_pareto_log=dgp(seq(0, 6, length.out=1000), loc=0,                           # obtain density for visualisation 
               scale=est_scale_log,
               shape=est_shape_log,
               log=F)
#
sample_quantile=quantile(fireinsclaims, 0.99)                                   # obtain a sample quantile (this shall be our first point for evaluation)
#
VaR99=qgp(0.99, loc=0, scale=est_scale, shape=est_shape)                        # Value at Risk for GPD on raw data
ES99=mean(qgp(seq(0.99, 0.99999, length.out=100),                               # Expected Shortfall for GPD on raw data
         loc=0, scale=est_scale, shape=est_shape))
#
VaR99_log=qgp(0.99, loc=0, scale=est_scale_log, shape=est_shape_log)            # Value at Risk for GPD on log transformed data (! need still to reverse log !)
ES99_log=qgp(seq(0.99, 0.99999, length.out=100),                                # Expected Shortfall for GPD on log transformed data
              loc=0, scale=est_scale_log, shape=est_shape_log)
#
exp(VaR99_log)                                                                  # reversing log for Value at Risk
mean(exp(ES99_log))                                                             # reversing log for Expected Shortfall
mean(fireinsclaims[sample_quantile<fireinsclaims])                              # actual average losses above Value at Risk
#
par(mfrow=c(1,2))                                                               # visualising GPD fit to the raw data
hist(fireinsclaims, breaks=50,
                    xlab="", main="Ins. Claims (no log & Pareto)", freq=F, 
                    xlim=c(0,300), ylim=c(0,0.2))
lines(est_pareto, col="red", lty=2, lwd=2)
abline(v=c(sample_quantile,
           VaR99), col=c(2,4))
#
hist(log(fireinsclaims), breaks=50,                                             # visualising GPD fit to the log transformed data
     xlab="", main="Ins. Claims (log & Pareto)", freq=F, 
     xlim=c(0,6), ylim=c(0,1))
lines(est_pareto_log~seq(0, 6, length.out=1000), col="blue", lty=2, lwd=2)
abline(v=c(log(sample_quantile),
           VaR99_log), col=c(2,4))
#
# ============================================================================= #
#
# install.packages("tidyquant")
library(tidyquant)                                                              # download NASDAQ index and BTC price from yahoo finance
#
BTC=getSymbols("BTC-USD", from = '2016-01-01',
               to = "2023-10-30",warnings = FALSE,
               auto.assign = FALSE)
#
NDX=getSymbols("^IXIC", from = '2016-01-01',
               to = "2023-10-30",warnings = FALSE,
               auto.assign = FALSE)
#
BTC=BTC$`BTC-USD.Adjusted`
NDX=NDX$IXIC.Adjusted
colnames(BTC)="BTC"
colnames(NDX)="NDX"
#
BTC=diff(log(BTC))*100                                                          # obtain log returns
NDX=diff(log(NDX))*100
NDXBTC=na.omit(merge(BTC, NDX))                                                 # merge in to one data set (this is needed to align )
#
# ============================================================================= #
#
# install.packages("rugarch")
# install.packages("rmgarch")
#
library(rugarch)
library(rmgarch)
#
ugarch11=ugarchspec(mean.model=list(armaOrder=c(0, 0), include.mean=F),         # specify a simple GARCH(1, 1) model without mean regression
                    variance.model=list(model="sGARCH"), 
                    distribution.model="norm")
#
ugarch11fit=ugarchfit(ugarch11, NDXBTC$NDX)                                     # fit GARCH model to NASDAQ log-returns
#
NDX_stnd=NDXBTC$NDX/sigma(ugarch11fit)                                          # standardise NASDAQ returns
NDX_evt=abs(NDX_stnd[NDX_stnd<0])                                               # obtain absolute of negative returns
#
fit.gpd(NDX_evt, show="T")                                                      # obtain parameters for GPD absolute negative standardised returns
est_scale_NDX=0.82851                                                           # save estimated GPD parameters
est_shape_NDX=-0.04111
#
GARCHEVT99=qgp(0.99, loc=0, scale=est_scale_NDX, shape=est_shape_NDX)           # obtain a desired quantile from the GPD distribution
GARCHEVTVaR99=-sigma(ugarch11fit)[-1968]*GARCHEVT99                             # obtain Value at Risk measure for GARCH-EVT model


plot(NDXBTC$NDX[-1], main="NDX returns")                                        # plot NASDAQ log returns alongside GARCH-EVT output
lines(GARCHEVTVaR99, col=2, lwd=2)
#
sum(GARCHEVTVaR99>NDXBTC$NDX[-1])                                               # actual Value at Risk violations
length(NDXBTC$NDX)*0.01                                                         # expected Value at Risk violations
#
# ============================================================================= #
#
mgarch11=dccspec(uspec=multispec(replicate(2, ugarch11)), dccOrder=c(1,1),      # specify a simple multivariate GARCH model
                 distribution="mvnorm")
mgarchfit=dccfit(mgarch11, data = NDXBTC)                                       # fit multivariate GARCH model
dyncor=rcor(mgarchfit)                                                          # obtain dynamic correlation matrix (components)
#
plot(dyncor[2, 1, ]~index(NDXBTC), type="l",                                    # plot obtained dynamic correlation
                                   main="NDX-BTC dynamic correlation",
                                   ylim=c(-0.2, 0.7))
abline(h=c(0, 0.2, 0.4, 0.6), lty=2, col=8)
#
# ============================================================================= #