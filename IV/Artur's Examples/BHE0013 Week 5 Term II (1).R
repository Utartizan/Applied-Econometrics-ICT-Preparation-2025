# ============================================================================= #
set.seed(1)
options(scipen = 30)                                                            # we would like R to be more descriptive
# load package and data
library(AER)
data("CigarettesSW")
# We will work with demand for cigarettes data from Stock and Watson (2007) Into to Econometrics textbook
?CigarettesSW
# Do necessary data transformations
CigarettesSW$rprice=with(CigarettesSW, price/cpi)
CigarettesSW$salestax=with(CigarettesSW, (taxs-tax)/cpi)
CigarettesSW$cigtax=with(CigarettesSW, tax/cpi)
CigarettesSW$rincome=with(CigarettesSW, income/population/cpi)
# Let's focus on one year for simplicity
cig1985=subset(CigarettesSW, year=="1985")                                       # we need 1985 at a later stage
cig1995=subset(CigarettesSW, year=="1995")
#
# We can ignore the simultaneous causality between supply and demand, 
# obtain the expected sign; however, how accurate is this elasticity estimate?
# E.g. a natural suggestion and main policy tool are taxes ...
# and this may be actually our target for estimations! 
reg1=lm(log(packs)~log(rprice)+as.factor(state), data=cig1995)                  # note that we have only one year
summary(reg1)
#
# Let's ignore state individual effects
reg2=lm(log(packs)~log(rprice), data=cig1995)             
summary(reg2)                                                                   # you can explore why ...
#
# ============================================================================= #
#
# Let's perform TSLS manually first.
# Note that we can first explore correlation of the potentially endogenous var.
# and if the error term is 0 ...
cor(residuals(reg2), log(cig1995$rprice))
mean(residuals(reg2))                                                           # both are according to expectations, but theory and our modelling objectives suggest that we use IV
#
# It is important that the IV is affiliated with the endogenous var.
cor(log(cig1995$rprice), cig1995$salestax)                                      # this is a good level of corr.
# Perform first stage regression
s1reg=lm(log(rprice)~salestax, data=cig1995)
summary(s1reg)                                                                  # note that our IV is statistically significant, but we need:
# Obtain predicted values conditional on the sales tax
pfitted=fitted(s1reg)
# Perform second stage regression
s2reg=lm(log(packs)~pfitted, data=cig1995)
summary(s2reg)                                                                  # while coefficients are correct we cannot use these st. errors; hence, are unsure about st. significance
#
# Perform IV regression with correct st. errors
ivreg1=ivreg(log(packs)~log(rprice) | salestax, data = cig1995)
summary(ivreg1)                                                                 # we can perform comparisons to the original regession
#
# ============================================================================= #
#
# Let's look into IV in the MLR setting, above setting misses other important
# factors, such as income and still does not provide a comprehensive outlook
#
ivreg2=ivreg(log(packs)~log(rprice)+log(rincome) | log(rincome)+salestax, 
                        data = cig1995)                                         # how does R knows which we want as endogenous variable? 
summary(ivreg2)
# More instruments, is this correct? 
ivreg3=ivreg(log(packs)~log(rprice)+log(rincome) | log(rincome) + salestax 
                        + cigtax, data = cig1995)
summary(ivreg3)
#
# The output above is not very attractive; but importance of using instruments
# for policy evaluation remains valid as we were not looking into policy evaluation
# accordingly so far ... 
#
# We require the following data transformation steps for DD model (recall FE)
# Essentially we will look into the LR elasticity of demand for cigarettes 
dpacks=log(cig1995$packs)-log(cig1985$packs)
drprice=log(cig1995$rprice)-log(cig1985$rprice)
drincome=log(cig1995$rincome)-log(cig1985$rincome)
dcigtax=cig1995$cigtax-cig1985$cigtax
dsalestax=cig1995$salestax-cig1985$salestax
#
ivreg4=ivreg(dpacks~drprice+drincome | drincome + dsalestax + dcigtax)
summary(ivreg4)                                                                 # Sargan points on two many instruments (overidentified)
#
ivreg5=ivreg(dpacks~drprice+drincome | drincome + dcigtax)                      
summary(ivreg5)                                                                 # good diagnostic output
#
# ============================================================================= #