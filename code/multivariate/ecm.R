rm(list=ls())
require(data.table)
require(ggplot2)
require(dyn)
require(dynlm)
require(zoo)
require(urca)
require(uroot)
require(fpp2)
require(tseries)
require(lmtest)
require(sandwich)
require(eurostat)
require(lubridate)
require(aTSA)
require(ecm)
library(collapse) 

# load data
data(USeconomic)

#rs - discount rate on 91-day TBill
#rl - yield on long-term treasury bonds

dataR <- cbind(rl,rs)
autoplot(dataR)

N <- length(rl)

#what's going on here?
par(mfrow=c(2,2))

# stationary or not?
acf(rl,lag.max=12) 
acf(rs,lag.max=12) 
pacf(rl,lag.max=12) 
pacf(rs,lag.max=12) 

# test for unit root
testrl <-(ur.df(rl, type="drift",  lags=6, selectlags = "AIC"))
testrs <-(ur.df(rs, type="drift",  lags=6, selectlags = "AIC"))
summary(testrl)
summary(testrs)

res_adf_rl <- ts(testrl@res)
res_adf_rs <- ts(testrs@res)

checkresiduals(ts(testrl@res))
checkresiduals(ts(testrs@res))


test_drl <-(ur.df(diff(rl), type="drift", lags=6, selectlags = "AIC"))
test_drs <-(ur.df(diff(rs), type="drift", lags=6, selectlags = "AIC"))
summary(test_drl)
summary(test_drs)

# conclusion?


# cointegration?
uhat <- ts(resid(lm(rs ~ rl)))
autoplot(uhat)

coint.test(rl,rs,nlag=4)
po.test(dataR)
summary(ca.jo(dataR))
test_uhat <-(ur.df(uhat, lags=6, selectlags = "AIC"))
summary(test_uhat)



# conclusion?
date = time(dataR)
reg_longrun <- dynlm(window(rs, c(1954, 1), c(1987, 4)) ~ window(rl, c(1954,1), c(1987, 4)))
summary(reg_longrun)
deviation <-resid(reg_longrun)

# let's estimate ecm
ecm<-dynlm(d(rs) ~ L(d(rs), 1:2) + L(d(rl), 0:1) + L(deviation))
summary(ecm)

bgtest(ecm,order=8)
coeftest(ecm, vcov = vcovHAC)
ecm_fit <- fitted(ecm)

autoplot(ecm_fit) + autolayer(diff(rs))

halflife <- -log(2)/log(1+ecm$coefficients[6])




#### MONEY DEMAND
rm(list=ls())
data(finland)

finland <- ts(finland,start = c(1958, 1), frequency =4)
dataFIN <- cbind(finland[,"lrm1"],finland[,"lny"])
colnames(dataFIN) <- c("m","y")
autoplot(dataFIN)
#what's going on here?
par(mfrow=c(2,2))

# stationary or not?
acf(dataFIN[,"m"],lag.max=12) 
acf(dataFIN[,"y"],lag.max=12) 
pacf(dataFIN[,"m"],lag.max=12) 
pacf(dataFIN[,"y"],lag.max=12) 

# test for unit root
testm <-(ur.df(dataFIN[,"m"], type="drift",  lags=6, selectlags = "AIC"))
testy <-(ur.df(dataFIN[,"y"], type="drift",  lags=6, selectlags = "AIC"))
summary(testm)
summary(testy)

res_adf_m <- ts(testm@res)
res_adf_y <- ts(testy@res)

checkresiduals(ts(testm@res))
checkresiduals(ts(testy@res))


test_dm <-(ur.df(diff(dataFIN[,"m"]), type="drift", lags=6, selectlags = "AIC"))
test_dy <-(ur.df(diff(dataFIN[,"y"]), type="drift", lags=6, selectlags = "AIC"))
summary(test_dm)
summary(test_dy)
# cointegration?

uhat <- ts(resid(lm(m ~ y,data=dataFIN)))
autoplot(uhat)

coint.test(dataFIN[,"m"],dataFIN[,"y"],nlag=4)
po.test(dataFIN)
summary(ca.jo(dataFIN,season = 4))
test_uhat <-(ur.df(uhat, lags=6, selectlags = "AIC"))
summary(test_uhat)

# conclusion?
date = time(dataFIN)
reg_longrun <- dynlm(window(m, c(1958, 1)) ~ window(y, c(1958, 1)),data=dataFIN)
summary(reg_longrun)
deviation <-resid(reg_longrun)

# let's estimate ecm
ecm<-dynlm(d(m) ~ L(d(m), 1:2) + L(d(y), 0:2) + L(deviation) + season(m),data=dataFIN)
summary(ecm)
bgtest(ecm,order=8)
coeftest(ecm, vcov = vcovHAC)
ecm_fit <- fitted(ecm)

autoplot(ecm_fit) + autolayer(diff(dataFIN[,"m"]))

halflife <- -log(2)/log(1+ecm$coefficients[7])

