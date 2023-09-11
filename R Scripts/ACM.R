library(psych)
library(pracma)
library(zoo)
library(lubridate)
library(dplyr)
#library(MASS)
library(highcharter)
library(matrixcalc)
library(matlib)
library(dplyr)
library(readxl)

#Zero Coupon Yields
N = 120
source("NSS.R")
L <- nss_yields("Data_Files/gnss.csv",N)
rawYields <- L$rawYields/100
plot_dates <- L$plot_dates
Yields <- as.matrix(rawYields)
Yields <- zoo(Yields,plot_dates)
colnames(Yields) <- paste("n",seq(1,N),sep = "_")
T = nrow(Yields)

# Use excess returns at these maturities to estimate the model.
rx_maturities = c(6,18,24,36,48,60,84,120) 
ttm = c(1:N)*(1/12)
logPrices = t(t(-rawYields)*ttm)
rf =  -1*logPrices[1:T,1]
rx =  logPrices[2:T, 1:(N-1)] - logPrices[1:(T-1), 2:N] - rf[-T]

# K Principal Components
K=5
demeanedyield = scale(Yields,scale = FALSE)
k = prcomp((demeanedyield))
yieldPCs = demeanedyield %*% (k$rotation)

#Step 1 - estimate VAR(1) for the time series of pricing factors
X <- zoo(yieldPCs[,1:K],plot_dates)
X_t1 <-  X[2:T,] 
X_t <-  X[1:(T-1),]
mod1 <- lm(X_t1 ~ X_t)
mu <- coef(mod1)[1,]
phi <- t(coef(mod1)[-1,])
v <- mod1$residuals
Sigma <-  t(v)%*%v/nrow(v)

#Step 2 - regress log excess returns
rx_maturities <- c(6, 18, 24, 36, 48, 60, 84, 120)
selected_rx <- t(rx[,rx_maturities-1])
VX <- as.matrix(bind_cols(v,X_t))
mod2 <- lm(t(selected_rx) ~ VX)
coefficients <- coef(mod2)

a <- coefficients[1,]
beta <- coefficients[2:(K+1), ]
c <- coefficients[(K+2):nrow(coefficients),]

E <- mod2$residuals
sigmasq_ret = tr(t(E) %*% E)/(nrow(E)*ncol(E))

#Step 3 - Run cross-sectional regressions
quad_form <- function(b){
  c(outer(b,b))
}
BStar <- t(apply(t(beta),1,quad_form))
lambda0 <- Ginv(t(beta)) %*% (a + 0.5*((BStar %*% c(Sigma)) + sigmasq_ret))
lambda1 <- Ginv(t(beta)) %*% t(c)

#Recursions for bond pricing
A <- rep(0,N)
B <- matrix(0,K,N)
mod3 <- lm(rf ~ X)
delta0 <- coef(mod3)[1]
delta1 <- coef(mod3)[-1]
A[1] = -delta0 + 0.5*(sigmasq_ret)
B[, 1] = - delta1
for (i in 2:N){
  A[i]  = A[i-1] + t(B[, i-1]) %*% (mu-lambda0) + 0.5 * (t(B[, i-1]) %*% Sigma %*% B[, i-1] + sigmasq_ret) - delta0
  B[,i] = t(B[, i-1]) %*% (phi - lambda1) - delta1
}


#Recursions for Risk Free Yields
A_rf <- rep(0,N)
B_rf <- matrix(0,K,N)
A_rf[1] = -delta0 + 0.5*(sigmasq_ret)
B_rf[, 1] = - delta1
lambda0 = matrix(0,K,1)
lambda1 = matrix(0,K,K)
for (i in 2:N){
  A_rf[i]  = A_rf[i-1] + t(B_rf[, i-1]) %*% (mu-lambda0) + 0.5 * (t(B_rf[, i-1]) %*% Sigma %*% B_rf[, i-1] + sigmasq_ret) - delta0
  B_rf[,i] = t(B_rf[, i-1]) %*% (phi - lambda1) - delta1
}

#FITTED YIELDS
fittedLogPrices = t(as.vector(t(A)) + t(B) %*% t(X))
fittedYields = - t(t(fittedLogPrices) / ttm)

#RISK NEUTRAL YIELDS
RiskFreeLogPrices = t(as.vector(t(A_rf)) + t(B_rf) %*% t(X))
RiskFreeYields = - t(t(RiskFreeLogPrices) / ttm)

#TERM PREMIA
termpremia = fittedYields - RiskFreeYields

#Computing model expected short rates
#We make a Txn matrix with each row containing 1 to "n" months ahead model implied rates
n <- 120 #Time till which forecasts are made
ESTR <- matrix(0,T,n)
for (i in 1:n){
  ESTR[,i] <- (delta0 + t(delta1 %*% matrix.power(phi,i) %*% t(X)))/ttm[1]
  #Correction term - adding it does not make a difference - accounts for mu
  correction <- 0
  for (j in 1:(i-1)){
    correction <- correction + delta1 %*% matrix.power(phi,j) %*% mu
  }
  ESTR[,i] <- ESTR[,i] + matrix(correction,1,T)
}

#Monetary Policy Component from ACM model as per our methodology 
MP_ACM <- ESTR[,1] + (1-1/n) * (ESTR[,1] - fittedYields[,1])
for (i in 2:n){
  MP_ACM <- MP_ACM + (1-i/n)*(ESTR[,i] - ESTR[,i-1])
}

#Plotting Interest Rate Projections
#Start date - 1 month ahead of the last observation
start <- ymd(plot_dates[T]) %m+% period("1 month")
#End date - "Y" years ahead
Y <- 3
end <- ymd(plot_dates[T]) %m+% period(paste(Y,"years"))
plot_dates_proj <- seq(as.Date(start),as.Date(end), length.out = Y*12)

plot(plot_dates_proj,ESTR[T,1:(Y*12)]*100,type = "l",
     ylab = "Expected Rate",xlab = "Date", ylim = c(1.5,3.5),
     main = "ACM Interest Rate Projection (3Y)")
