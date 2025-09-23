#Prepare Macrodataset
data_EA <- read_csv("atsm_eu/data/data_gdp_and_inflation_US_EA.csv") %>% filter(country == "EA")
data_EA.ts <- xts(data_EA[,3:length(data_EA)],order.by = data_EA$date)
LR_inf <- read_csv("atsm_eu/data/data_ecb_inflation_LR.csv")
LR_inf.ts <- xts(LR_inf$inflation_LR,order.by = LR_inf$date)
MY_df <- read_csv("atsm_eu/data/data_euro_demographic_structure.csv")
MY_df.ts <- xts(MY_df$MY,order.by = MY_df$date)
data_EA.ts <- merge(data_EA.ts,LR_inf.ts,MY_df.ts)
rm(data_EA,LR_inf,LR_inf.ts,MY_df,MY_df.ts)

US_names <- c("GDPC1","PCECTPI","GDPPOT","gdp_growth","pi","dy_pot","PTR","MY")
colnames(data_EA.ts) <- US_names
data_EA.ts$PTR <- data_EA.ts$PTR/100

data_EA.ts$y      <- log(data_EA.ts$GDPC1)
data_EA.ts$y_pot  <- log(data_EA.ts$GDPPOT)
data_EA.ts$y_gap  <- (data_EA.ts$y - data_EA.ts$y_pot)
data_EA.ts$dy_pot <- (data_EA.ts$y_pot - lag.xts(data_EA.ts$y_pot, 4))


time_window_EA <- "2001-03-01/2038-06-01"
current_time_window_EA <- "2001-03-01/2023-06-01"
#Prepare Yield Curve dataset
yc <- read_csv("atsm_eu/data/data_ger_yield_curve.csv")
nss_yields_EA.ts <- xts(yc[2:61], order.by = yc$date)
colnames(nss_yields_EA.ts) <- paste0("n_",names(nss_yields_EA.ts))
rm(yc)
#nss_yields_EA.ts <- Yields[current_time_window_EA]

#Add n1 yield to macrodataset for the regression
data_EA.ts <- merge(nss_yields_EA.ts$n_1,data_EA.ts)

#Subset to available dates
#Extend data
data_EA.ts$dy_pot["2025-06-01/2038-06-01"] <- 0.0130
data_EA.ts$PTR["2024-03-01/2038-06-01"] <- 0.021
data_EA.ts <- data_EA.ts[time_window_EA]
nss_yields_EA.ts <- nss_yields_EA.ts[current_time_window_EA]
dates_EA <- index(data_EA.ts)
current_dates_EA <- index(data_EA.ts[current_time_window_EA])
dates_lag_EA <-seq(as.Date("2001-06-01"),length=nrow(nss_yields_EA.ts)-1, by="quarters")

#Run trend regression
longrun_EA<-lm(n_1~MY+dy_pot+PTR-1,data=data_EA.ts[current_dates_EA])

#Check stationarity of residuals

#Using EA trend
r_EA.ts <- xts(fitted(longrun_EA), order.by = current_dates_EA)
u_EA.ts <- xts(resid(longrun_EA), order.by = current_dates_EA)
tseries::adf.test(u_EA.ts,k=0)
tseries::adf.test(u_EA.ts)
colnames(r_EA.ts) <- "trend"
colnames(u_EA.ts) <- "cycle"

#Add EA trend
data_EA.ts <- merge(data_EA.ts,r_EA.ts)
data_EA.ts <- merge(data_EA.ts,u_EA.ts)

data_EA.ts$rf <- 0.25*data_EA.ts$n_1

#Make trend matrix (OPTIONAL)-----------------------

#Extract cycle
N = dim(nss_yields_EA.ts)[2]
ttm = c(1:N)*(1/4)
nss_cycle_EA <- sweep(nss_yields_EA.ts,1,data_EA.ts$trend[current_dates_EA],FUN = "-")
K=5
demeanedcyle = scale(nss_cycle_EA, scale = FALSE)
k = prcomp((demeanedcyle))
summary(k)
cyclePCs = demeanedcyle %*% (k$rotation)

X.ts <- xts(cyclePCs[,1:K], order.by=current_dates_EA)
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
p.curve.ts  <- -t(t(nss_yields_EA.ts)*ttm)
p.curve.ts <- xts(p.curve.ts, order.by=index(nss_yields_EA.ts))
colnames(p.curve.ts) = colnames(nss_yields_EA.ts)

hpr.ts <- lag.xts(p.curve.ts[,c(1:N-1)],-1) - p.curve.ts[,c(2:N)]
xr.ts <- sweep(hpr.ts, 1, nss_yields_EA.ts[index(hpr.ts),1]/4,FUN='-')
xr.ts <- xr.ts[-nrow(xr.ts)]
index(xr.ts) <- index(v.ts)
bias <- lag.xts(data_EA.ts$trend[current_dates_EA],-1) - data_EA.ts$trend[current_dates_EA]
bias <- bias[current_time_window_EA]
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
xr.XandV.reg <- lm(xr.sub.ts ~ VX.ts)
#xr.XandV.reg = lm(xr.adjusted.sub.ts ~ VX.ts)
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

mod_r      <- lm(nss_cycle_EA[,1]/4 ~ X.ts)
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
#mod3_EA_3 <- lm(rf_3~X)
#delta0_3 <- coef(mod3_EA_3)[1]
#delta1_3 <- coef(mod3_EA_3)[-1]
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
fittedYields_cy_EA.ts <- xts(fittedYields_cy, order.by=current_dates_EA)
rm(fittedLogPrices_cy,fittedYields_cy,A,B)
