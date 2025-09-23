### ACM MODEL---------------------------------------------------
## -----------------------------------------------------------------------------
current_time_window <- "2000-03-01/2025-03-01"
time_window <- "2000-03-01/2034-12-01"

## -----------------------------------------------------------------------------
## Reload the data with the pure names (no EA_ prefix)
#load("atsm_eu/data/rda/EA_yieldCurve.rda")
source("NSS.R")
N = 60
L <- nss_yields("Data_Files/gnss.csv",N)
EA_yieldCurve.ts <- xts(L$rawYields, order.by = L$plot_dates)/100
colnames(EA_yieldCurve.ts) <- paste0("n",seq(1,60))
dates         <- index(EA_yieldCurve.ts)
current_dates <- index(EA_yieldCurve.ts[current_time_window])

T <- length(current_dates)
yieldCurve.ts <- EA_yieldCurve.ts[current_time_window, c(1:N)]

# Take the maturities that are in the dataset
maturities.cols <- grep("^(n)([1-9][0-9]{0,2}|1000|0)$", colnames(yieldCurve.ts))
maturities.all  <- colnames(yieldCurve.ts)[maturities.cols]
maturities.list <- as.numeric(gsub("n", "", maturities.all))

rawYields <- yieldCurve.ts
plot_dates.ACM <- current_dates[seq(1,T,by = 3)]
Yields <- as.matrix(rawYields)[seq(1,T,by = 3),]
Yields <- zoo(Yields,plot_dates.ACM)
colnames(Yields) <- paste("n",seq(1,N),sep = "_")
T = nrow(Yields)

# Use excess returns at these maturities to estimate the model.
rx_maturities = c(6,18,24,36,48,60,84,120)/3
ttm = c(1:N)*(1/4)
logPrices = t(t(-Yields)*ttm)
rf =  -1*logPrices[1:T,1]
rx =  logPrices[2:T, 1:(N-1)] - logPrices[1:(T-1), 2:N] - rf[-T]

# K Principal Components
K=5
demeanedyield = scale(Yields,scale = FALSE)
k = prcomp((demeanedyield))
yieldPCs = demeanedyield %*% (k$rotation)

#Step 1 - estimate VAR(1) for the time series of pricing factors
X <- zoo(yieldPCs[,1:K],plot_dates.ACM)
X_t1 <-  X[2:T,]
X_t <-  X[1:(T-1),]
mod1 <- lm(X_t1 ~ X_t)
mu <- coef(mod1)[1,]
phi <- t(coef(mod1)[-1,])
v <- mod1$residuals
Sigma <-  t(v)%*%v/nrow(v)

#Step 2 - regress log excess returns
rx_maturities <- c(6, 18, 24, 36, 48, 60, 84, 120)/3
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
B_rf[,1] = - delta1
lambda0 = matrix(0,K,1)
lambda1 = matrix(0,K,K)
for (i in 2:N){
  A_rf[i]  = A_rf[i-1] + t(B_rf[, i-1]) %*% (mu-lambda0) + 0.5 * (t(B_rf[, i-1]) %*% Sigma %*% B_rf[, i-1] + sigmasq_ret) - delta0
  B_rf[,i] = t(B_rf[, i-1]) %*% (phi - lambda1) - delta1
}

#FITTED YIELDS
fittedLogPrices.ACM = t(as.vector(t(A)) + t(B) %*% t(X))
fittedYields.ACM = - t(t(fittedLogPrices.ACM) / ttm)

#RISK NEUTRAL YIELDS
RiskFreeLogPrices.ACM = t(as.vector(t(A_rf)) + t(B_rf) %*% t(X))
RiskFreeYields.ACM = - t(t(RiskFreeLogPrices.ACM) / ttm)
RiskFreeYields.ACM.ts = xts(RiskFreeYields.ACM,order.by = index(X))


#TERM PREMIA
# big4_y.10Y.ts <- xts(master_df %>% select(Yield_Germany,Yield_France,Yield_Spain,Yield_Italy), order.by = master_df$Date)
# colnames(big4_y.10Y.ts) <- c("DE_n10","FR_n10","ES_n10","IT_n10")
TP.ACM.ts = na.omit(sweep(big4_y.10Y.ts[plot_dates.ACM], 1, RiskFreeYields.ACM.ts[,40]*100, FUN = "-"))
colnames(TP.ACM.ts) <- gsub("_n10","", colnames(TP.ACM.ts))
data_frame.ACM <- data.frame(Date=index(TP.ACM.ts), as.data.frame(coredata(TP.ACM.ts)) %>% select(DE,FR,ES,IT))

# Now you can melt your data frame
data_long.ACM <- melt(data_frame.ACM, id.vars = "Date")
termpremia_Germany.ACM = filter(data_long.ACM,variable == "DE")$value
termpremia_France.ACM = filter(data_long.ACM,variable == "FR")$value
termpremia_Spain.ACM = filter(data_long.ACM,variable == "ES")$value
termpremia_Italy.ACM = filter(data_long.ACM,variable == "IT")$value

#Computing model expected short rates
#We make a Txn matrix with each row containing 1 to "n" months ahead model implied rates
n <- 40 #Time till which forecasts are made
T <- nrow(X)
ESTR.ACM <- matrix(0,T,n)
for (i in 1:n){
  ESTR.ACM[,i] <- (delta0 + t(delta1 %*% matrix.power(phi,i) %*% t(X)))/ttm[1]
  #Correction term - adding it does not make a difference - accounts for mu
  correction <- delta1 %*% mu
  for (j in 1:(i-1)){
    correction <- correction + delta1 %*% matrix.power(phi,j) %*% mu
  }
  ESTR.ACM[,i] <- ESTR.ACM[,i] + matrix(correction,1,T)/ttm[1]
}

#Monetary Policy Component from ACM model as per our methodology 
MP.ACM <- fittedYields.ACM[,1] + (1-1/n) * (ESTR.ACM[,1] - fittedYields.ACM[,1])
for (i in 2:n){
  MP.ACM <- MP.ACM + (1-i/n)*(ESTR.ACM[,i] - ESTR.ACM[,i-1])
}
MP.ACM.ts <- xts(MP.ACM, order.by=as.Date(plot_dates.ACM))

X.ACM.ts <- xts(X,plot_dates.ACM)


#Plotting Interest Rate Projections
#Start date - 1 month ahead of the last observation
start <- ymd(plot_dates.ACM[T])
start_3 <- ymd(plot_dates.ACM[T])

#End date - "Y" years ahead
Y <- 3
end <- ymd(plot_dates.ACM[T]) %m+% period(paste(Y,"years"))

plot_dates_proj <- seq(as.Date(start),as.Date(end), length.out = Y*12 + 1)
plot_dates_proj_3 <- seq(as.Date(start_3),as.Date(end), length.out = Y*4 + 1)

train_period <- "2004-06-01/2025-03-01"
forecast_window.length <- 40
forecast_window.dates <-seq(as.Date("2025-06-01"),length=forecast_window.length, by="quarters")

X_train.ts  <- X.ACM.ts[train_period]
X_train.var <- VAR(X_train.ts, p = 1, type = "const")
X_train.for <- predict(X_train.var, n.ahead = forecast_window.length)
X_in.mat    <- fitted(X_train.var)
X_for.mat   <- cbind(X_train.for$fcst$PC1[,1],X_train.for$fcst$PC2[,1],X_train.for$fcst$PC3[,1],
                     X_train.for$fcst$PC4[,1],X_train.for$fcst$PC5[,1])
dates <- c(index(X_train.ts),forecast_window.dates)
pred_list <- list()
n = 1
y_OoS <- t(-(A[n]+t(B[, n])%*%t(X_for.mat))/ttm[n])
y_IN  <- t(-(A[n]+t(B[, n])%*%t(X_in.mat))/ttm[n])
y_OoS.ts <- xts((y_OoS), order.by = forecast_window.dates) # assuming dates_for remains constant
y_IN.ts <- xts(y_IN, order.by = index(X_train.ts)[-1])
pred.ts <- xts(merge(yieldCurve.ts[dates, paste0("n", n)], na.omit(y_OoS.ts), join = 'outer'), dates)
pred.ts <- xts(merge(pred.ts, na.omit(y_IN.ts), join = 'outer'), dates)
pred.ts <- pred.ts["2001-03-01/2034-12-01"]

# Set column names
colnames(pred.ts) <- c(paste0("n", n), paste0("n", n, "_OoS"), paste0("n", n, "_IN"))
pred.ts.ACM <- pred.ts

