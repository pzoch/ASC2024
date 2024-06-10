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
library(vars)
library(collapse) 

# load data

datasetY<- get_eurostat("sts_inpr_m")
datasetP<- get_eurostat("prc_hicp_midx")

tempY <- subset(datasetY, geo == c("PL") & s_adj == "SCA" & unit == "I15" & nace_r2 == "B-D") 
tempY <- na.omit(ts(rev(tempY$values),start = c(2000, 1), frequency =12))

tempP <- subset(datasetP, geo == c("PL") & coicop=="CP00" & unit=="I15") 
tempP <- na.omit(ts(rev(tempP$values),start = c(1997, 1), frequency =12))

Y    <- window(100*diff(log(tempY)),start = c(2002, 1), end = c(2019, 12))
P    <- window(100*diff(log(tempP)),start = c(2002, 1), end = c(2019, 12))


data <- (cbind(Y,P))
autoplot(data)

VAR <- VAR(data, p=12, type="both")
summary(VAR)

Bcoef(VAR)
Acoef(VAR)


# Select lags
VARselect(data, lag.max=24, type="both")

# Check restrictions - we create our own function
LLtest <- function(varS, varL, df){
  d  <-  -2*(logLik(varS) - logLik(varL))
  p  <- 1 - pchisq(d, df)
  return(list(stat=as.numeric(d), prob = as.numeric(p))) 
}

VAR_11  <- VAR(data, p=11, type="trend")
VAR_12  <- VAR(data,  p=12, type="trend")


LLtest(VAR_12, VAR_13, 12)

VAR_IC = VAR_12
# residuals
par(mfrow=c(2,2), cex = 0.7, bty="l", mar=c(3,3,3,3))
Acf(residuals(VAR_IC)[,1], xlab="",main="e1 vs lags of e1")
Ccf(residuals(VAR_IC)[,1],residuals(VAR_IC)[,2],xlab="",main="e1 vs lags of e2", xlim=c(0,24))
Acf(residuals(VAR_IC)[,2],xlab="",main="e2 vs lags of e2")
Ccf(residuals(VAR_IC)[,2],residuals(VAR_IC)[,1],xlab="",main="e2 vs lags of e1", xlim=c(0,24))




#Autocorrelation - Breusch-Godfrey
BG <- matrix(NA, 24, 3)
for(h in 2:24){
  x <- serial.test(VAR_IC, lags.bg = h, type="BG")
  BG[h,] <- as.numeric(x[["serial"]][1:3])
}
rownames(BG) <- paste('h=',1:24, sep="")
colnames(BG) <- c("stat.", " df ", "p")
options(digits=3)
BG

# stability 
roots(VAR_IC, modulus = TRUE)


# Forecast
H = 12
T   <- dim(data)[1]
fct <- predict(VAR_IC, n.ahead=H)
fct
fanchart(fct, nc=1,bty="l",cis=c(0.68,0.95),mar=c(3,3,1,1))



