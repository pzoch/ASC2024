rm(list=ls())
require(data.table)
require(ggplot2)
require(dyn)
require(zoo)
require(urca)
require(uroot)
require(fpp2)
require(tseries)
require(lmtest)
require(eurostat)
require(lubridate)
require(aTSA)



# simulate random walks

rho    <- 1
sigma2 <- 1
no_rws <- 2

set.seed(111)
nobs    = 200
RWsim    = matrix(0,nobs,no_rws)
for(k in 1:no_rws){
  for(t in 2:nobs){
    RWsim[t,k]=rho*RWsim[t-1,k]+rnorm(1,mean = 0, sd = sqrt(sigma2))
  }
}
RWsim <- ts(RWsim)
autoplot(RWsim)

# spurious regression
X <- RWsim[,1]
Y <- RWsim[,2]

summary(lm(Y ~ X))
summary(lm(diff(Y) ~ diff(X)))

# create cointegrated variables
sigma2_eps <- 0.25
sigma2_nu  <- 0.75

epsilon <- rnorm(nobs,mean = 0, sd = sqrt(sigma2_eps))
nu      <- rnorm(nobs,mean = 0, sd = sqrt(sigma2_nu))

Z <- RWsim[,1]
X <- Z  + epsilon
Y <- Z  + nu

RWsim <- cbind(X,Y,Z)
autoplot(RWsim)

summary(lm(Y ~ X))

# conclusion?

uhat <- resid(lm(Y ~ X))
autoplot(ts(uhat))

# testing unit root
testX <-(ur.df(X, type="none", lags=0))
testY <-(ur.df(Y, type="none", lags=4))
summary(testX)
summary(testY)


#what's going on here?
par(mfrow=c(2,2))

#a hint is here
acf(testX@res,lag.max=24) 
acf(testY@res,lag.max=24) 
pacf(testX@res,lag.max=24) 
pacf(testY@res,lag.max=24) 


par(mfrow=c(1,2))
acf(uhat,lag.max=24) 
pacf(uhat,lag.max=24) 
coint.test(Y,X)

# please disregard t-statistics here
summary(lm(Y ~ X - 1))

# what happens with regression on differences
summary(lm(diff(Y) ~ diff(X)-1))


### REAL DATA NOW
# Nelson-Plosser Macro Time Series
data(NelPlo)
m <- window(log(money.stock),start=1900,end=1988)
p <- window(log(gnp.def),start=1900,end=1988)
dataNP <- cbind(m,p)
autoplot(dataNP)

# test for unit root
testM <-(ur.df(m, type="drift", lags=1))
testP <-(ur.df(p, type="drift", lags=1))
summary(testM)
summary(testP)

uhat <- resid(lm( m ~ p))
autoplot(ts(uhat))
par(mfrow=c(1,2))
acf(uhat,lag.max=6) 
pacf(uhat,lag.max=6) 

summary(ca.jo(dataNP))
coint.test(m,p)

# conclusion?

dataNP_short = window(dataNP,start=1951,end=1988)
m <- window((m),start=1951,end=1988)
p <-window((p),start=1951,end=1988)
dataNP_short <- cbind(m,p)
autoplot(dataNP_short)

# test for unit root
testM <-(ur.df(m, type="drift", lags=1))
testP <-(ur.df(p, type="drift", lags=1))
summary(testM)
summary(testP)

uhat <- resid(lm( p ~ m))
autoplot(ts(uhat))

par(mfrow=c(1,2))
acf(uhat,lag.max=6) 
pacf(uhat,lag.max=6) 


coint.test(p,m)
summary(ca.jo(dataNP_short))


### REAL DATA NOW - v2

# A data frame of quarterly data ranging from 1955:Q1 until 1984:Q4. The data is expressed in natural logarithms. 
data(UKconinc)
UKconinc <- ts(UKconinc,start = c(1955, 1), frequency =4)
autoplot(UKconinc)
dec <- decompose(UKconinc)

UKdata <- UKconinc - dec$seasonal
autoplot(UKdata)
C <- UKdata[,1]
Y <- UKdata[,2]

testC <-(ur.df(C, type="drift", lags=1))
testY <-(ur.df(Y, type="drift", lags=1))
summary(testC)
summary(testY)
coint.test(C,Y,nlag=4)
summary(ca.jo(UKdata))

lm(C ~ Y)
summary(lm(diff(C) ~ diff(Y)-1))
