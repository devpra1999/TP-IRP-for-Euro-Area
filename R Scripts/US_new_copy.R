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
current_time_window_US <- "1980-03-01/2023-06-01"
time_window_US <- "1980-03-01/2050-12-01"
nss_yields_US.ts <- nss_yields[current_time_window_US]
rawdata.ts<-rawdata.ts[time_window_US]
rm(nss_yields)
current_dates_US <- index(nss_yields_US.ts)
dates_lag_US <-seq(as.Date("1980-06-01"),length=nrow(nss_yields_US.ts)-1, by="quarters")
# maturities<-c(1:N)

#Add n1 yield to macrodataset for the regression
data_US.ts <- merge(rawdata.ts,nss_yields_US.ts$n_1)
data_US.ts$rf<-0.25*data_US.ts$n_1
longrun_US <- lm(rf~MY+dy_pot+PTR-1,data=data_US.ts[current_dates_US])
longrun_a_US<-lm(n_1~MY+dy_pot+PTR-1,data=data_US.ts[current_dates_US])
rm(rawdata.ts)

#Extend data and subset to available dates
data_US.ts$dy_pot["2026-03-01/2038-06-01"] <- 0.0175
data_US.ts$PTR["2023-06-01/2038-06-01"] <- 0.02
dates_US <- index(data_US.ts)

data_US.ts$trend<-longrun_US$coefficients[1]*data_US.ts$MY + 
  longrun_US$coefficients[2]*data_US.ts$dy_pot + longrun_US$coefficients[3]*data_US.ts$PTR
data_US.ts$cycle<-data_US.ts$rf-data_US.ts$trend
#Check stationarity of residuals
# u_US <- resid(longrun_US)
# u_US.ts <- xts(u_US, order.by = index(data_US.ts[current_dates_US]))
# tseries::adf.test(u_US.ts,k=0)
# 
# r_US <- fitted(longrun_US)
# r_US.ts <- xts(r_US, order.by = index(data_US.ts[current_dates_US]))
# colnames(r_US.ts) <- "trend"
# data_US.ts <- merge(data_US.ts,r_US.ts)
# colnames(u_US.ts) <- "cycle"
# data_US.ts <- merge(data_US.ts,u_US.ts)
# rm(r_US,r_US.ts,u_US,u_US.ts)

#Make trend matrix (OPTIONAL)-----------------------
time_window_US <- "1980-03-01/2038-12-01"
data.df <- data_US.ts$trend[time_window_US]
T = nrow(data.df)
N=60
ttm = c(1:N)*(1/4)
Y_trend <- matrix(0,T,N)
Y_trend[,1]<-4*data.df[,1]

for (i in 2: N){
  for (j in 1:(T - N)) {  
    Y_trend[j, i] <- (Y_trend[j, (i-1)] + Y_trend[(j +i- 1), 1])
    
  }}

for (i in 1:N){Y_trend[, i]<-Y_trend[, i]/i}
Y_trend_df<- data.frame(Y_trend[1:174,])
Y_trend.ts<-xts(Y_trend_df, order.by=current_dates_US)
colnames(Y_trend_df) <- paste("n",seq(1,N),sep = "_")

#Extract cycle
nss_yields_df<-data.frame(dates = index(nss_yields_US.ts), Data=coredata(nss_yields_US.ts))
nss_cycle<-nss_yields_df[,2:61]-Y_trend_df[,1]
colnames(nss_cycle) <- paste("n",seq(1,N),sep = "_")
nss_cycle_US <- xts(nss_cycle, order.by=current_dates_US)
#nss_cycle_US <- sweep(nss_yields_US.ts,1,data_US.ts$trend[current_dates_US],FUN = "-")

#K Principal Components
K=5
demeanedcyle = scale(nss_cycle_US, scale = FALSE)
k = prcomp((demeanedcyle))
summary(k)
cyclePCs = demeanedcyle %*% (k$rotation)

# Get the principal components in .xts format and add them to yields.ts
X_US.ts <- xts(cyclePCs[,1:K], order.by=current_dates_US)
nss_yields_US.ts <- merge(nss_yields_US.ts, X_US.ts, join = 'outer')
rm(demeanedcyle,k,cyclePCs)


## Step 1 - estimate VAR(1) for the time series of pricing factors
my_var_model_US <- VAR(X_US.ts, p = 1, type = "const")
summary(my_var_model_US)
#cointegration analysis
result <- ca.jo(X_US.ts, type = "eigen", ecdet = "const",K=2)
summary(result)
rm(result)

coeff_matrices <- coef(my_var_model_US)
phi <- rbind(coeff_matrices$PC1[1:5,1],coeff_matrices$PC2[1:5,1],
             coeff_matrices$PC3[1:5,1],coeff_matrices$PC4[1:5,1],
             coeff_matrices$PC5[1:5,1])
mu<- rbind(coeff_matrices$PC1[6,1],coeff_matrices$PC2[6,1],
           coeff_matrices$PC3[6,1],coeff_matrices$PC4[6,1],
           coeff_matrices$PC5[6,1])
rm(coeff_matrices)
resmat <- residuals(my_var_model_US)
Sigma <-  t(resmat)%*%resmat/nrow(resmat)

#Merge residuals into yield curve database
colnames(resmat) <- paste("v",colnames(resmat),sep = "_")
resmat.ts <- xts(resmat, order.by=dates_lag_US)
nss_yields_US.ts <- merge(nss_yields_US.ts, resmat.ts, join = 'outer')
rm(resmat,resmat.ts)

## Step 2 - Regress log excess returns
#generate excess returns
#generate hpr and excess hpr with an approximation error
nss_yields_US.ts$hpr4 <- (1/4)*lag.xts(nss_yields_US.ts$n_4)- (4/4-1/4)*(nss_yields_US.ts$n_3-lag.xts(nss_yields_US.ts$n_4))
nss_yields_US.ts$hpr8<- (1/4)*lag.xts(nss_yields_US.ts$n_8)- (8/4-1/4)*(nss_yields_US.ts$n_7-lag.xts(nss_yields_US.ts$n_8))
nss_yields_US.ts$hpr12<- (1/4)*lag.xts(nss_yields_US.ts$n_12)- (12/4-1/4)*(nss_yields_US.ts$n_11-lag.xts(nss_yields_US.ts$n_12))
nss_yields_US.ts$hpr16<- (1/4)*lag.xts(nss_yields_US.ts$n_16)- (16/4-1/4)*(nss_yields_US.ts$n_15-lag.xts(nss_yields_US.ts$n_16))
nss_yields_US.ts$hpr20<- (1/4)*lag.xts(nss_yields_US.ts$n_20)- (20/4-1/4)*(nss_yields_US.ts$n_19-lag.xts(nss_yields_US.ts$n_20))
nss_yields_US.ts$hpr28<- (1/4)*lag.xts(nss_yields_US.ts$n_28)- (28/4-1/4)*(nss_yields_US.ts$n_27-lag.xts(nss_yields_US.ts$n_28))
nss_yields_US.ts$hpr40<- (1/4)*lag.xts(nss_yields_US.ts$n_40)- (40/4-1/4)*(nss_yields_US.ts$n_39-lag.xts(nss_yields_US.ts$n_40))
nss_yields_US.ts$hpr60<- (1/4)*lag.xts(nss_yields_US.ts$n_60)- (60/4-1/4)*(nss_yields_US.ts$n_59-lag.xts(nss_yields_US.ts$n_60))

bias = -(data_US.ts$trend[current_dates_US]-lag.xts(data_US.ts$trend[current_dates_US]))
nss_yields_US.ts$hper4=nss_yields_US.ts$hpr4-(1/4)*lag.xts(nss_yields_US.ts$n_1)- (4/4-1/4)*bias
nss_yields_US.ts$hper8=nss_yields_US.ts$hpr8-(1/4)*lag.xts(nss_yields_US.ts$n_1)- (8/4-1/4)*bias
nss_yields_US.ts$hper12=nss_yields_US.ts$hpr12-(1/4)*lag.xts(nss_yields_US.ts$n_1)- (12/4-1/4)*bias
nss_yields_US.ts$hper16=nss_yields_US.ts$hpr16-(1/4)*lag.xts(nss_yields_US.ts$n_1)- (16/4-1/4)*bias
nss_yields_US.ts$hper20=nss_yields_US.ts$hpr20-(1/4)*lag.xts(nss_yields_US.ts$n_1)- (20/4-1/4)*bias
nss_yields_US.ts$hper28=nss_yields_US.ts$hpr28-(1/4)*lag.xts(nss_yields_US.ts$n_1)- (28/4-1/4)*bias
nss_yields_US.ts$hper40=nss_yields_US.ts$hpr40-(1/4)*lag.xts(nss_yields_US.ts$n_1)- (40/4-1/4)*bias
nss_yields_US.ts$hper60=nss_yields_US.ts$hpr60-(1/4)*lag.xts(nss_yields_US.ts$n_1)- (60/4-1/4)*bias

# Lags of factors
nss_yields_US.ts$PC1_lag=lag.xts(nss_yields_US.ts$PC1)
nss_yields_US.ts$PC2_lag=lag.xts(nss_yields_US.ts$PC2)
nss_yields_US.ts$PC3_lag=lag.xts(nss_yields_US.ts$PC3)
nss_yields_US.ts$PC4_lag=lag.xts(nss_yields_US.ts$PC4)
nss_yields_US.ts$PC5_lag=lag.xts(nss_yields_US.ts$PC5)

###----------------------
###System estimation for excess returns
###----------------------

eqhper_4r <- hper4 ~  PC1_lag+ PC2_lag+PC3_lag+PC4_lag+PC5_lag
eqhper_8r <- hper8 ~  PC1_lag+ PC2_lag+PC3_lag+PC4_lag+PC5_lag
eqhper_12r <- hper12 ~  PC1_lag+ PC2_lag+PC3_lag+PC4_lag+PC5_lag
eqhper_16r <- hper16 ~  PC1_lag+ PC2_lag+PC3_lag+PC4_lag+PC5_lag
eqhper_20r <- hper20 ~  PC1_lag+ PC2_lag+PC3_lag+PC4_lag+PC5_lag
eqhper_28r <- hper28 ~  PC1_lag+ PC2_lag+PC3_lag+PC4_lag+PC5_lag
eqhper_40r <- hper40 ~  PC1_lag+ PC2_lag+PC3_lag+PC4_lag+PC5_lag
eqhper_60r <- hper60 ~  PC1_lag+ PC2_lag+PC3_lag+PC4_lag+PC5_lag


system_hper_r <- list(eqhper_4r,eqhper_8r,eqhper_12r,eqhper_16r,eqhper_20r,eqhper_28r,
                      eqhper_40r,eqhper_60r)
rm(eqhper_4r,eqhper_8r,eqhper_12r,eqhper_16r,eqhper_20r,eqhper_28r,
   eqhper_40r,eqhper_60r)

eqhper_4 <- hper4 ~  v_PC1+ v_PC2+v_PC3+v_PC4+v_PC5+
  +PC1_lag+ PC2_lag+PC3_lag+PC4_lag+PC5_lag
eqhper_8 <- hper8 ~  v_PC1+ v_PC2+v_PC3+v_PC4+v_PC5+
  +PC1_lag+ PC2_lag+PC3_lag+PC4_lag+PC5_lag
eqhper_12 <- hper12 ~  v_PC1+ v_PC2+v_PC3+v_PC4+v_PC5+
  +PC1_lag+ PC2_lag+PC3_lag+PC4_lag+PC5_lag
eqhper_16 <- hper16 ~  v_PC1+ v_PC2+v_PC3+v_PC4+v_PC5+
  +PC1_lag+ PC2_lag+PC3_lag+PC4_lag+PC5_lag
eqhper_20 <- hper20 ~  v_PC1+ v_PC2+v_PC3+v_PC4+v_PC5+
  +PC1_lag+ PC2_lag+PC3_lag+PC4_lag+PC5_lag
eqhper_28 <- hper28 ~  v_PC1+ v_PC2+v_PC3+v_PC4+v_PC5+
  +PC1_lag+ PC2_lag+PC3_lag+PC4_lag+PC5_lag
eqhper_40 <- hper40 ~  v_PC1+ v_PC2+v_PC3+v_PC4+v_PC5+
  +PC1_lag+ PC2_lag+PC3_lag+PC4_lag+PC5_lag
eqhper_60 <- hper60 ~ v_PC1+ v_PC2+v_PC3+v_PC4+v_PC5+
  +PC1_lag+ PC2_lag+PC3_lag+PC4_lag+PC5_lag


system_hper_unr <- list(eqhper_4,eqhper_8,eqhper_12,eqhper_16,eqhper_20,eqhper_28,
                        eqhper_40,eqhper_60)
rm(eqhper_4,eqhper_8,eqhper_12,eqhper_16,eqhper_20,eqhper_28,
   eqhper_40,eqhper_60)

#estimation
system_hper_r_fit <- systemfit( system_hper_r, "OLS", data = nss_yields_US.ts )
summary(system_hper_r_fit)
rm(system_hper_r,system_hper_r_fit)
system_hper_unr_fit <- systemfit( system_hper_unr, "OLS", data = nss_yields_US.ts )
summary(system_hper_unr_fit)
coeffmat_sys <- coef(system_hper_unr_fit)
res_sys<- residuals(system_hper_unr_fit)
rm(system_hper_unr,system_hper_unr_fit)
res_sys<-res_sys[-1,] #first row contains NAs because of the lag
E<-as.matrix(res_sys)

sigmasq_ret = tr(t(E) %*% E)/(nrow(E)*ncol(E))
#Sigma_v1<-  (t(E1)%*%E1)/(nrow(E1))
#sigmasq_ret1 <- as.matrix(diag(Sigma_v1))

m1=matrix(coeffmat_sys,nrow=8,ncol=11,byrow=TRUE)
#m1 <- matrix(coeff_matsys, nrow = 11, ncol = 8)
a <- m1[,1]
beta <- t(m1[,2:(K+1)])
c <- t(m1[,(K+2):ncol(m1)])
rm(m1,coeffmat_sys,E,res_sys)


#Step 3 - Run cross-sectional regressions
quad_form <- function(b){
  c(outer(b,b))
}
BStar <- t(apply(t(beta),1,quad_form))
lambda0 <- Ginv(t(beta)) %*% (a + 0.5*((BStar %*% c(Sigma)) + sigmasq_ret))
lambda1 <- Ginv(t(beta)) %*% t(c)
rm(a,beta,c)

#Recursions for bond pricing
A <- rep(0,N)
B <- matrix(0,K,N)
nss_cycle_US$rf<-0.25*nss_cycle_US$n_1
mod3_US <- lm(nss_cycle_US$rf ~ X_US.ts)
delta0 <- coef(mod3_US)[1]
delta1 <- coef(mod3_US)[-1]

A[1] = -delta0 #+ 0.5*(sigmasq_ret)
B[, 1] = - delta1
#mod3_US_3 <- lm(rf_3~X)
#delta0_3 <- coef(mod3_US_3)[1]
#delta1_3 <- coef(mod3_US_3)[-1]
for (i in 2:N){
  #cat(t(B[, i-1]) %*% Sigma %*% B[, i-1],sigmasq_ret)
  A[i]  = A[i-1] + t(B[, i-1]) %*% (mu-lambda0) + 0.5 * (t(B[, i-1]) %*% Sigma %*% B[, i-1] + sigmasq_ret) - delta0
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
  A_rf[i]  = A_rf[i-1] + t(B_rf[, i-1]) %*% (mu-lambda0) + 0.5 * (t(B_rf[, i-1]) %*% Sigma %*% B_rf[, i-1] + sigmasq_ret) - delta0
  B_rf[,i] = t(B_rf[, i-1]) %*% (phi - lambda1) - delta1
}
rm(lambda0,lambda1,mu,phi,delta0,delta1,Sigma,sigmasq_ret,BStar)
###############################
#FITTED YIELDS cycle
fittedLogPrices_cy = t(as.vector(t(A)) + t(B) %*% t(X_US.ts))
fittedYields_cy = - t(t(fittedLogPrices_cy) / ttm)
fittedYields_cy_US.ts <- xts(fittedYields_cy, order.by=current_dates_US)
rm(fittedLogPrices_cy,fittedYields_cy,A,B)

#RISK NEUTRAL YIELDS cycle
RiskFreeLogPrices_cy = t(as.vector(t(A_rf)) + t(B_rf) %*% t(X_US.ts))
RiskFreeYields_cy = - t(t(RiskFreeLogPrices_cy) / ttm)
RiskFreeYields_cy_US.ts <- xts(RiskFreeYields_cy, order.by=current_dates_US)
rm(RiskFreeLogPrices_cy,RiskFreeYields_cy,A_rf,B_rf)

#######################
#TERM PREMIA
########################
termpremia_US.ts = fittedYields_cy_US.ts - RiskFreeYields_cy_US.ts
#rm(nss_cycle_US,RiskFreeYields_cy_US.ts)

# tseries::adf.test(data_US.ts$cycle[current_dates_US])
# 
# plot(X_US.ts)
# 
plot(nss_cycle_US[,40],main = "Cycle Fit")
lines(fittedYields_cy_US.ts[,40], col = "blue")
# xts::addLegend("topright", legend.names = c("Acutal","Fit"),
#           col = c("black","blue"), lty = c(1,1), lwd = c(2,1))






