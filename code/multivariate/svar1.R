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
require(tidyr)
require(sandwich)
require(eurostat)
require(lubridate)
require(aTSA)
library(vars)
library(collapse) 


data(USeconomic)

# together

d_lgdp = 100*diff(log(GNP))
r      = 100*rs
data <- na.remove(cbind(d_lgdp,r))
autoplot(data)

VAR <- VAR(data, p=4, type="both")
summary(VAR)

Bcoef(VAR)
Acoef(VAR)


# Select lags
VARselect(data, lag.max=4, type="both")
VAR_IC <- VAR(data, p=4, type="both")



# recursive structuralization 

bmat   <- matrix(0,2,2)
bmat[lower.tri(bmat, diag=T)] <- NA

SVARE <- SVAR(VAR_IC, Amat = NULL, Bmat = bmat)

SVARE
B <- SVARE$B

K    <- 12
SVMA <- Phi(SVARE, nstep=K)

# short run-impact matrix
SVMA[,,1]

# Plot 
SIRF <- irf(SVARE, n.ahead=K)
plot(SIRF)

# FEVD
fevds <- fevd(SVARE,n.ahead=40)
fevds$d_lgdp
plot(fevds)

i      = 1            # which variables
# a. Structural shocks
e <- residuals(VAR_IC)
B <- SVARE$B
u <- t(solve(B)%*%t(e))
T <- dim(u)[1]
 cor(u) 

# b. IRFs
SVMA <- Phi(SVARE, nstep=T)
SIRF  = t(SVMA[i,,])    # IRF for variable i

# c. Historical decomposition

HistDec             <- matrix(NA,T,2)
colnames(HistDec)   <- c("uy","ur")

for(t in 1:T){
  junk1 <- as.matrix(u[1:t,])
  junk2 <- as.matrix(SIRF[t:1,])
  HistDec[t,] <- colSums(junk1*junk2)
}

# the impact of initial conditions
InitCond <- tail(coredata(data[,i]),T) - colSums(t(HistDec)) 
mu       <- as.numeric(tail(InitCond,1))
InitCond <- InitCond - mu
HistDec  <- cbind(HistDec, InitCond)


qtrseq<-seq(as.Date("1954-01-04"), by="quarter", length.out = length(d_lgdp))

HD1   <- as.data.frame(HistDec);
HD1$t <- tail(qtrseq,T)
HD2   <- gather(data = HD1, key = shocks, value = value, -c(t))
Y     <- data.frame(y = tail(coredata(data[,i]),T)-mu,t=tail(qtrseq,T))

ggplot() + 
  geom_bar(data=HD2, aes(fill=shocks, y=value, x=t), stat="identity",width=50) + # 
  scale_fill_manual(values=c("grey80", "red", "green"))+
  geom_line(data=Y, aes(y=y, x=t), size=0.2)+
  theme_light()+
  labs(title="Historical decomposition for GDP growth in US", y="", x="", caption="")


