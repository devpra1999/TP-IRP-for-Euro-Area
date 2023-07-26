library(psych)
library(pracma)
library(zoo)
library(lubridate)
library(dplyr)
library(MASS)

n_maturities = 120
K=5

#Zero Coupon Yields
source("NSS.R")
L <- nss_yields("Data_Files/gnss.xlsx",120)
rawYields <- L$rawYields/100
plot_dates <- L$plot_dates
Yields <- as.matrix(rawYields)
Yields <- zoo(Yields,plot_dates)
colnames(Yields) <- paste("n",seq(1,120),sep = "_")

# Use excess returns at these maturities to estimate the model.
rx_maturities = c(6,18,24,36,48,60,84,120) 
ttm = c(1:120)*(1/12)
logPrices = t(t(-rawYields)*ttm)
rf =  -1*logPrices[(1:nrow(logPrices)-1),1]
rx =  logPrices[2:nrow(logPrices), 1:(ncol(logPrices)-1)] - logPrices[1:(nrow(logPrices)-1), 2:(ncol(logPrices))] -rf

# K Principal Components
K=5
demeanedyield = scale(Yields,scale = FALSE)
k = prcomp((demeanedyield))
yieldPCs = demeanedyield %*% (k$rotation)

#Step 1 - estimate VAR(1) for the time series of pricing factors
X <- zoo(yieldPCs[,1:K],plot_dates)
X_t1 <-  X[(2:nrow(X)),] 
X_t <-  X[1:(nrow(X)-1),]
mod1 <- lm(X_t1 ~ X_t)
mu <- coef(mod1)[1,]
phi <- t(coef(mod1)[-1,])
v <- X_t1 - X_t%*%t(phi)
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
lambda0 <- ginv(t(beta)) %*% (a + 0.5*((BStar %*% c(Sigma)) + sigmasq_ret))
lambda1 <- ginv(t(beta)) %*% t(c)

#Recursions for bond pricing
A <- rep(0,120)
B <- matrix(0,K,120)
mod3 <- lm(rf ~ X_t1)
delta0 <- coef(mod3)[1]
delta1 <- coef(mod3)[-1]
A[1] = -delta0 + 0.5*(sigmasq_ret)
B[, 1] = - delta1
for (i in 2:120){
  A[i]  = A[i-1] + t(B[, i-1]) %*% (-lambda0) + 0.5 * (t(B[, i-1]) %*% Sigma %*% B[, i-1] + sigmasq_ret) - delta0
  B[,i] = t(B[, i-1]) %*% (phi - lambda1) - delta1
}


#Recursions for Risk Free Yields
A_rf <- rep(0,120)
B_rf <- matrix(0,K,120)
A_rf[1] = -delta0 + 0.5*(sigmasq_ret)
B_rf[, 1] = - delta1
lambda0 = matrix(0,5,1)
lambda1 = matrix(0,5,5)
for (i in 2:120){
  A_rf[i]  = A_rf[i-1] + t(B_rf[, i-1]) %*% (-lambda0) + 0.5 * (t(B_rf[, i-1]) %*% Sigma %*% B_rf[, i-1] + sigmasq_ret) - delta0
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

#PLOT
s1 <- as.data.frame(cbind(as.Date(L$plot_dates),fittedYields[,120]*100))
names(s1) <- c("x","y")
s2 <- as.data.frame(cbind(as.Date(L$plot_dates),RiskFreeYields[,120]*100))
names(s2) <- c("x","y")
s3 <- as.data.frame(cbind(as.Date(L$plot_dates),termpremia[,120]*100))
names(s3) <- c("x","y")
s4 <- as.data.frame(cbind(as.Date(L$plot_dates),rep(0,length(L$plot_dates))))
names(s4) <- c("x","y")

Germany_Term_Premia_ACM <- highchart() %>%
  hc_add_series(s1, "line", hcaes(x, y), name = "Yield", color = "black") %>%
  hc_add_series(s2, "line", hcaes(x, y), name = "Risk Neutral Yield", color = "red") %>%
  hc_add_series(s3, "line", hcaes(x, y), name = "Term Premia", color = "blue") %>%
  hc_add_series(s4, "line", hcaes(x, y), name = "", dashStyle = "dot", color = "black",
                showInLegend = FALSE) %>%
  hc_title(text = "ACM based 10Y Term Premia - Germany") %>%
  hc_xAxis(type = "datetime", title = list(text = "Date")) %>%
  hc_yAxis(title = list(text = "Yield & Term Premia"), min = -2, max = 6) %>%
  hc_legend(enabled = TRUE) %>%
  hc_boost(enabled = TRUE) %>%
  hc_exporting(enabled = TRUE)

Germany_Term_Premia_ACM
