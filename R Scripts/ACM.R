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

#Zero Coupon Yields
N = 120
source("NSS.R")
L <- nss_yields("Data_Files/gnss.xlsx",N)
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

#The monetary policy component so derived has peaks at two places due to the
#peaks in the 1 month yields in NSS (and subsequently ACM fitted 1 month yields) 

#PLOT - Historical
s1 <- as.data.frame(cbind(plot_dates,fittedYields[,120]*100))
names(s1) <- c("x","y")
s1$x <- as.Date(s1$x)
s2 <- as.data.frame(cbind(as.Date(plot_dates),RiskFreeYields[,120]*100))
names(s2) <- c("x","y")
s2$x <- as.Date(s2$x)
s3 <- as.data.frame(cbind(as.Date(L$plot_dates),termpremia[,120]*100))
names(s3) <- c("x","y")
s3$x <- as.Date(s3$x)
s4 <- as.data.frame(cbind(as.Date(L$plot_dates),rep(0,length(L$plot_dates))))
names(s4) <- c("x","y")
s4$x <- as.Date(s4$x)
s5 <- as.data.frame(cbind(plot_dates,MP_ACM*100))
names(s5) <- c("x","y")
s5$x <- as.Date(s5$x)

Germany_Term_Premia_ACM <- highchart() %>%
  hc_add_series(s1, "line", hcaes(x, y), name = "Model Implied Yield", color = "black") %>%
  hc_add_series(s2, "line", hcaes(x, y), name = "Risk Neutral Yield", color = "red") %>%
  hc_add_series(s3, "line", hcaes(x, y), name = "Term Premia", color = "blue") %>%
  hc_add_series(s4, "line", hcaes(x, y), name = "", dashStyle = "dot", color = "black",
                showInLegend = FALSE) %>%
  hc_add_series(s5, "line", hcaes(x, y), name = "Monetary Policy", color = "grey",
                dashStyle = "dash") %>%
  hc_title(text = "ACM based 10Y Term Premia - Germany") %>%
  hc_xAxis(type = "datetime", title = list(text = "Date")) %>%
  hc_yAxis(title = list(text = "Yield & Term Premia"), min = -2, max = 6) %>%
  hc_legend(enabled = TRUE) %>%
  hc_boost(enabled = TRUE) %>%
  hc_exporting(enabled = TRUE)

Germany_Term_Premia_ACM


#Plotting Interest Rate Projections
#Start date - 1 month ahead of the last observation
start <- plot_dates[T] + month(1)

#End date - "Y" years ahead
Y <- 10
curr <- as.POSIXlt(plot_dates[T])
end <- curr
end$year <- end$year + Y
plot_dates_proj <- seq(as.Date(start),as.Date(end), length.out = Y*12)
plot(plot_dates_proj,ESTR[T,1:(Y*12)]*100,type = "l",
     ylab = "Expected Rate",xlab = "Date", ylim = c(1.5,4),
     main = "ACM Interest Rate Projection (10Y)")
