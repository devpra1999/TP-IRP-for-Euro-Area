#Prepare Macrodataset
load('data/raw/rawdata1.rda')

# Inflation: π (pi)
rawdata.ts$LP     <- log(rawdata.ts$PCECTPI)
rawdata.ts$pi     <- (rawdata.ts$LP - lag.xts(rawdata.ts$LP, 4))
rawdata.ts$pi_gap <- rawdata.ts$pi - rawdata.ts$PTR/100

# Output gap: y_gap 
rawdata.ts$y      <- log(rawdata.ts$GDPC1)
rawdata.ts$y_pot  <- log(rawdata.ts$GDPPOT)
rawdata.ts$y_gap  <- (rawdata.ts$y - rawdata.ts$y_pot)
rawdata.ts$dy_pot <- (rawdata.ts$y_pot - lag.xts(rawdata.ts$y_pot, 4))
#plot(rawdata.ts$dy_pot)
#summary(dynlm(y_gap ~ L(y_gap)))

# Interest rates
rawdata.ts$y3m_cc<-log(1+0.01*rawdata.ts$DGS3MO)
rawdata.ts$US_RFcc<-log(1+0.01*rawdata.ts$US_RF)
#rawdata.ts$y3m_ccq<-0.25*log(1+0.01*rawdata.ts$DGS3MO)*100

# rescale inflation target 
rawdata.ts$PTR<-0.01*rawdata.ts$PTR


#Prepare yield curve dataset
load('data/raw/yields1.rda')
nss_yields_US.ts <- nss_yields
rm(nss_yields)
# maturities<-c(1:N)

#Add n1 yield to macrodataset for the regression
data_US.ts <- merge(rawdata.ts,nss_yields_US.ts$n_1)
rm(rawdata.ts)

#Extend data and subset to available dates
data_US.ts$dy_pot["2026-03-01/2038-06-01"] <- 0.0175
data_US.ts$PTR["2023-06-01/2038-06-01"] <- 0.02
time_window_US <- "1980-03-01/2038-12-01"
current_time_window_US <- "1980-03-01/2023-06-01"
data_US.ts <- data_US.ts[time_window_US]
nss_yields_US.ts <- nss_yields_US.ts[current_time_window_US]
dates_US <- index(data_US.ts)
current_dates_US <- index(data_US.ts[current_time_window_US])
dates_lag_US <-seq(as.Date("1980-06-01"),length=nrow(nss_yields_US.ts)-1, by="quarters")

#Run trend regression
longrun_US<-lm(n_1~MY+dy_pot+PTR-1,data=data_US.ts[current_dates_US])

#Check stationarity of residuals
u_US <- resid(longrun_US)
u_US.ts <- xts(u_US, order.by = index(data_US.ts[current_dates_US]))
tseries::adf.test(u_US.ts,k=0)

r_US <- fitted(longrun_US)
r_US.ts <- xts(r_US, order.by = index(data_US.ts[current_dates_US]))
colnames(r_US.ts) <- "trend"
data_US.ts <- merge(data_US.ts,r_US.ts)
colnames(u_US.ts) <- "cycle"
data_US.ts <- merge(data_US.ts,u_US.ts)
rm(r_US,r_US.ts,u_US,u_US.ts)

data_US.ts$rf <- 0.25*data_US.ts$n_1


#Make trend matrix (OPTIONAL)-----------------------

#Extract cycle
N = dim(nss_yields_US.ts)[2]
ttm = c(1:N)*(1/4)
nss_cycle_US <- sweep(nss_yields_US.ts,1,data_US.ts$trend[current_dates_US],FUN = "-")
K=5
demeanedcyle = scale(nss_cycle_US, scale = FALSE)
k = prcomp((demeanedcyle))
summary(k)
cyclePCs = demeanedcyle %*% (k$rotation)

X.ts <- xts(cyclePCs[,1:K], order.by=current_dates_US)
window.X.var <- index(X.ts)[c(2:length(index(X.ts)))]
X.var <- lm(lag.xts(X.ts,-1)[-nrow(X.ts)]~X.ts[-nrow(X.ts)])  
summary(X.var)
mu <-  as.matrix(coef(X.var)[1,])
Phi <- as.matrix(t(coef(X.var)[-1,]))
colnames(Phi) = rownames(Phi)
v.ts           <- xts(residuals(X.var), order.by=index(X.ts[window.X.var]) )
colnames(v.ts) <- paste("v",colnames(v.ts), sep = "_")
Sigma          <- t(v.ts)%*%v.ts/nrow(v.ts)                                     # Get the variance-covariance matrix

# Save everything
params.X          <- list(Phi=Phi, mu=mu, Sigma=Sigma)

ttm   <- c(1:N)*(1/4)
p.curve.ts  <- -t(t(nss_yields_US.ts)*ttm)
p.curve.ts <- xts(p.curve.ts, order.by=index(nss_yields_US.ts))
colnames(p.curve.ts) = colnames(nss_yields_US.ts)

hpr.ts <- lag.xts(p.curve.ts[,c(1:N-1)],-1) - p.curve.ts[,c(2:N)]
xr.ts <- sweep(hpr.ts, 1, nss_yields_US.ts[index(hpr.ts),1]/4,FUN='-')
xr.ts <- xr.ts[-nrow(xr.ts)]
index(xr.ts) <- index(v.ts)
bias <- lag.xts(data_US.ts$trend[current_dates_US],-1) - data_US.ts$trend[current_dates_US]
bias <- bias[current_time_window_US]
bias <- bias[-nrow(bias)]
index(bias) <- index(v.ts)
xr.adjusted.ts <- xr.ts + (bias %*% t(ttm-0.25))[,-1]

xr_maturities  <- c(3, 7, 11, 15, 19, 27, 39, 59)
xr.sub.ts <- (xr.ts[,xr_maturities])
xr.adjusted.sub.ts <- xr.adjusted.ts[,xr_maturities]

colnames(xr.sub.ts) <- paste0("xr", colnames(xr.sub.ts))
VX  <- cbind(as.matrix(v.ts),as.matrix(X.ts[-nrow(X.ts)]))
VX.ts <- xts(VX, order.by = index(v.ts))
xr.X.reg <- lm(xr.sub.ts ~ X.ts[-nrow(X.ts)])
summary(xr.X.reg)
#xr.XandV.reg <- lm(xr.sub.ts ~ VX.ts)
xr.XandV.reg = lm(xr.adjusted.sub.ts ~ VX.ts)
#summary(xr.XandV.reg)
coefficients <- coef(xr.XandV.reg)


a  <- coefficients[1,]
b <- coefficients[2:(K+1), ]
c <- coefficients[(K+2):nrow(coefficients),]

E <- coredata(xr.XandV.reg$residuals)
sigma2 = tr(t(E) %*% E)/(nrow(E)*ncol(E))


## -----------------------------------------------------------------------------
## STEP 4 - Get the coefficients of the Affine Term Structure Model
quad_form <- function(b){
  c(outer(b,b))
}
BStar   <- t(apply(t(b),1,quad_form))
lambda0 <- ginv(t(b)) %*% (a + 0.5*((BStar %*% c(Sigma)) + sigma2))
lambda1 <- ginv(t(b)) %*% t(c)
lambda  <- list(lambda0=lambda0, lambda1=lambda1)

mod_r      <- lm(nss_cycle_US[,1]/4 ~ X.ts)
delta0     <- coef(mod_r)[1]
delta1     <- coef(mod_r)[-1]
delta      <- list(delta0=delta0, delta1=delta1)
summary(mod_r)

phi = Phi
#Recursions for bond pricing
A <- rep(0,N)
B <- matrix(0,K,N)
A[1] = -delta0 #+ 0.5*(sigmasq_ret)
B[, 1] = - delta1
#mod3_US_3 <- lm(rf_3~X)
#delta0_3 <- coef(mod3_US_3)[1]
#delta1_3 <- coef(mod3_US_3)[-1]
for (i in 2:N){
  #cat(t(B[, i-1]) %*% Sigma %*% B[, i-1],sigmasq_ret)
  A[i]  = A[i-1] + t(B[, i-1]) %*% (mu-lambda0) + 0.5 * (t(B[, i-1]) %*% Sigma %*% B[, i-1] + sigma2) - delta0
  B[,i] = t(B[, i-1]) %*% (phi - lambda1) - delta1
}

#Recursions for Risk Free Yields
A_rf <- rep(0,N)
B_rf <- matrix(0,K,N)
A_rf[1] = -delta0 #+ 0.5*(sigmasq_ret)
B_rf[, 1] = - delta1
lambda0 = matrix(0,K,1)
lambda1 = matrix(0,K,K)
for (i in 2:N){
  A_rf[i]  = A_rf[i-1] + t(B_rf[, i-1]) %*% (mu-lambda0) + 0.5 * (t(B_rf[, i-1]) %*% Sigma %*% B_rf[, i-1] + sigma2) - delta0
  B_rf[,i] = t(B_rf[, i-1]) %*% (phi - lambda1) - delta1
}

## -----------------------------------------------------------------------------
## STEP 5 - Get the fitted values

fittedLogPrices_cy = t(as.vector(t(A)) + t(B) %*% t(X.ts))
fittedYields_cy = - t(t(fittedLogPrices_cy) / ttm)
fittedYields_cy_US.ts <- xts(fittedYields_cy, order.by=current_dates_US)
rm(fittedLogPrices_cy,fittedYields_cy,A,B)
