
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
# yc <- read_csv("atsm_eu/data/data_ger_yield_curve.csv")
# nss_yields_EA.ts <- xts(yc[2:61], order.by = yc$date)
# colnames(nss_yields_EA.ts) <- paste0("n_",names(nss_yields_EA.ts))
# rm(yc)
nss_yields_EA.ts <- Yields[current_time_window_EA]

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

# #Using US trend
r_US.ts <- xts(fitted(longrun_a_US),order.by = current_dates_US)
r_US_lag.ts <- lag.xts(r_US.ts,3)[current_dates_EA]
u_EA_USt.ts <- data_EA.ts$n_1[current_dates_EA] - r_US.ts
u_EA_USt_lag.ts <- data_EA.ts$n_1[current_dates_EA] - r_US_lag.ts
tseries::adf.test(u_EA_USt.ts,k=0)
tseries::adf.test(u_EA_USt_lag.ts,k=0)
tseries::adf.test(u_EA_USt.ts)
tseries::adf.test(u_EA_USt_lag.ts)
# colnames(r_US.ts) <- "trend"
# colnames(u_EA_USt.ts) <- "cycle"
# 
# #Using pooled trend
# load("atsm_eu/data/rda/EA_macroData.rda")
# r_pool.ts <- EA_macroData.ts$r_star[current_dates_EA]*4
# u_EA_poolt.ts <- data_EA.ts$n_1[current_dates_EA] - r_pool.ts
# tseries::adf.test(u_EA_poolt.ts,k=0)
# tseries::adf.test(u_EA_poolt.ts)
# colnames(r_pool.ts) <- "trend"
# colnames(u_EA_poolt.ts) <- "cycle"
# rm(EA_macroData.ts)
# 
# #Using US trend coefficients
# r_EA_USc.ts <- xts(data_EA.ts[current_dates_EA,c("MY","dy_pot","PTR")] %*% as.matrix(coef(longrun_US)),order.by = current_dates_EA)
# u_EA_USc.ts <- data_EA.ts$n_1[current_dates_EA] - r_EA_USc.ts
# tseries::adf.test(u_EA_USc.ts,k=0)
# colnames(r_EA_USc.ts) <- "trend"
# colnames(u_EA_USc.ts) <- "cycle"

##############

#Add the relevant trend and cycle to the main dataset

#Add EA trend
data_EA.ts <- merge(data_EA.ts,r_EA.ts)
data_EA.ts <- merge(data_EA.ts,u_EA.ts)

# #Add US trend
# data_EA.ts <- merge(data_EA.ts,r_US.ts)
# data_EA.ts <- merge(data_EA.ts,u_EA_USt.ts)

# #Add pooled trend
# data_EA.ts <- merge(data_EA.ts,r_pool.ts)
# data_EA.ts <- merge(data_EA.ts,u_EA_poolt.ts)

# #Add trend using US coefficients
# data_EA.ts <- merge(data_EA.ts,r_EA_USc.ts)
# data_EA.ts <- merge(data_EA.ts,u_EA_USc.ts)

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

# Get the principal components in .xts format and add them to yields.ts
X_EA.ts <- xts(cyclePCs[,1:K], order.by=current_dates_EA)
nss_yields_EA.ts <- merge(nss_yields_EA.ts, X_EA.ts, join = 'outer')
rm(demeanedcyle,k,cyclePCs)

##Step 1 - estimate VAR(1) for the time series of pricing factors
my_var_model_EA <- VAR(X_EA.ts, p = 1, type = "const")
summary(my_var_model_EA)
#cointegration analysis
result <- ca.jo(X_EA.ts, type = "eigen", ecdet = "const",K=2)
summary(result)
rm(result)

coeff_matrices <- coef(my_var_model_EA)
phi <- rbind(coeff_matrices$PC1[1:5,1],coeff_matrices$PC2[1:5,1],
             coeff_matrices$PC3[1:5,1],coeff_matrices$PC4[1:5,1],
             coeff_matrices$PC5[1:5,1])
mu<- rbind(coeff_matrices$PC1[6,1],coeff_matrices$PC2[6,1],
           coeff_matrices$PC3[6,1],coeff_matrices$PC4[6,1],
           coeff_matrices$PC5[6,1])
rm(coeff_matrices)
resmat <- residuals(my_var_model_EA)
Sigma <-  t(resmat)%*%resmat/nrow(resmat)

#Merge residuals into yield curve database
colnames(resmat) <- paste("v",colnames(resmat),sep = "_")
resmat.ts <- xts(resmat, order.by=dates_lag_EA)
nss_yields_EA.ts <- merge(nss_yields_EA.ts, resmat.ts, join = 'outer')
rm(resmat,resmat.ts)

## Step 2 - Regress log excess returns
#generate excess returns
#generate hpr and excess hpr with an approximation error
nss_yields_EA.ts$hpr4 <- (1/4)*lag.xts(nss_yields_EA.ts$n_4)- (4/4-1/4)*(nss_yields_EA.ts$n_3-lag.xts(nss_yields_EA.ts$n_4))
nss_yields_EA.ts$hpr8 <- (1/4)*lag.xts(nss_yields_EA.ts$n_8)- (8/4-1/4)*(nss_yields_EA.ts$n_7-lag.xts(nss_yields_EA.ts$n_8))
nss_yields_EA.ts$hpr12<- (1/4)*lag.xts(nss_yields_EA.ts$n_12)- (12/4-1/4)*(nss_yields_EA.ts$n_11-lag.xts(nss_yields_EA.ts$n_12))
nss_yields_EA.ts$hpr16<- (1/4)*lag.xts(nss_yields_EA.ts$n_16)- (16/4-1/4)*(nss_yields_EA.ts$n_15-lag.xts(nss_yields_EA.ts$n_16))
nss_yields_EA.ts$hpr20<- (1/4)*lag.xts(nss_yields_EA.ts$n_20)- (20/4-1/4)*(nss_yields_EA.ts$n_19-lag.xts(nss_yields_EA.ts$n_20))
nss_yields_EA.ts$hpr28<- (1/4)*lag.xts(nss_yields_EA.ts$n_28)- (28/4-1/4)*(nss_yields_EA.ts$n_27-lag.xts(nss_yields_EA.ts$n_28))
nss_yields_EA.ts$hpr40<- (1/4)*lag.xts(nss_yields_EA.ts$n_40)- (40/4-1/4)*(nss_yields_EA.ts$n_39-lag.xts(nss_yields_EA.ts$n_40))
nss_yields_EA.ts$hpr60<- (1/4)*lag.xts(nss_yields_EA.ts$n_60)- (60/4-1/4)*(nss_yields_EA.ts$n_59-lag.xts(nss_yields_EA.ts$n_60))

bias = -(data_EA.ts$trend[current_dates_EA]-lag.xts(data_EA.ts$trend[current_dates_EA]))
nss_yields_EA.ts$hper4=nss_yields_EA.ts$hpr4-(1/4)*lag.xts(nss_yields_EA.ts$n_1)- (4/4-1/4)*bias
nss_yields_EA.ts$hper8=nss_yields_EA.ts$hpr8-(1/4)*lag.xts(nss_yields_EA.ts$n_1)- (8/4-1/4)*bias
nss_yields_EA.ts$hper12=nss_yields_EA.ts$hpr12-(1/4)*lag.xts(nss_yields_EA.ts$n_1)- (12/4-1/4)*bias
nss_yields_EA.ts$hper16=nss_yields_EA.ts$hpr16-(1/4)*lag.xts(nss_yields_EA.ts$n_1)- (16/4-1/4)*bias
nss_yields_EA.ts$hper20=nss_yields_EA.ts$hpr20-(1/4)*lag.xts(nss_yields_EA.ts$n_1)- (20/4-1/4)*bias
nss_yields_EA.ts$hper28=nss_yields_EA.ts$hpr28-(1/4)*lag.xts(nss_yields_EA.ts$n_1)- (28/4-1/4)*bias
nss_yields_EA.ts$hper40=nss_yields_EA.ts$hpr40-(1/4)*lag.xts(nss_yields_EA.ts$n_1)- (40/4-1/4)*bias
nss_yields_EA.ts$hper60=nss_yields_EA.ts$hpr60-(1/4)*lag.xts(nss_yields_EA.ts$n_1)- (60/4-1/4)*bias

# nss_yields_EA.ts$hper4=nss_yields_EA.ts$hpr4-(1/4)*lag.xts(nss_yields_EA.ts$n_1)
# nss_yields_EA.ts$hper8=nss_yields_EA.ts$hpr8-(1/4)*lag.xts(nss_yields_EA.ts$n_1)
# nss_yields_EA.ts$hper12=nss_yields_EA.ts$hpr12-(1/4)*lag.xts(nss_yields_EA.ts$n_1)
# nss_yields_EA.ts$hper16=nss_yields_EA.ts$hpr16-(1/4)*lag.xts(nss_yields_EA.ts$n_1)
# nss_yields_EA.ts$hper20=nss_yields_EA.ts$hpr20-(1/4)*lag.xts(nss_yields_EA.ts$n_1)
# nss_yields_EA.ts$hper28=nss_yields_EA.ts$hpr28-(1/4)*lag.xts(nss_yields_EA.ts$n_1)
# nss_yields_EA.ts$hper40=nss_yields_EA.ts$hpr40-(1/4)*lag.xts(nss_yields_EA.ts$n_1)
# nss_yields_EA.ts$hper60=nss_yields_EA.ts$hpr60-(1/4)*lag.xts(nss_yields_EA.ts$n_1)

# Lags of factors
nss_yields_EA.ts$PC1_lag=lag.xts(nss_yields_EA.ts$PC1)
nss_yields_EA.ts$PC2_lag=lag.xts(nss_yields_EA.ts$PC2)
nss_yields_EA.ts$PC3_lag=lag.xts(nss_yields_EA.ts$PC3)
nss_yields_EA.ts$PC4_lag=lag.xts(nss_yields_EA.ts$PC4)
nss_yields_EA.ts$PC5_lag=lag.xts(nss_yields_EA.ts$PC5)

# nss_yields_EA.ts <- merge(data_US.ts$trend[current_dates_US],nss_yields_EA.ts)
# #Bias Correction
# trend_diff <- (nss_yields_EA.ts$trend - lag.xts(nss_yields_EA.ts$trend))
# nss_yields_EA.ts$hper4 = nss_yields_EA.ts$hper4 - (4/4-1/4)*trend_diff
# nss_yields_EA.ts$hper8 = nss_yields_EA.ts$hper8 - (8/4-1/4)*trend_diff
# nss_yields_EA.ts$hper12 = nss_yields_EA.ts$hper4 - (12/4-1/4)*trend_diff
# nss_yields_EA.ts$hper16 = nss_yields_EA.ts$hper8 - (16/4-1/4)*trend_diff
# nss_yields_EA.ts$hper20 = nss_yields_EA.ts$hper4 - (20/4-1/4)*trend_diff
# nss_yields_EA.ts$hper28 = nss_yields_EA.ts$hper8 - (28/4-1/4)*trend_diff
# nss_yields_EA.ts$hper40 = nss_yields_EA.ts$hper4 - (40/4-1/4)*trend_diff
# nss_yields_EA.ts$hper60 = nss_yields_EA.ts$hper8 - (60/4-1/4)*trend_diff
# rm(trend_diff)

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
system_hper_r_fit <- systemfit( system_hper_r, "OLS", data = nss_yields_EA.ts )
summary(system_hper_r_fit)
rm(system_hper_r,system_hper_r_fit)
system_hper_unr_fit <- systemfit( system_hper_unr, "OLS", data = nss_yields_EA.ts )

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
nss_cycle_EA$rf<-0.25*nss_cycle_EA$n_1
mod3_EA <- lm(nss_cycle_EA$rf ~ X_EA.ts)
delta0 <- coef(mod3_EA)[1]
delta1 <- coef(mod3_EA)[-1]

A[1] = -delta0 #+ 0.5*(sigmasq_ret)
B[, 1] = - delta1
#mod3_EA_3 <- lm(rf_3~X)
#delta0_3 <- coef(mod3_EA_3)[1]
#delta1_3 <- coef(mod3_EA_3)[-1]
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
fittedLogPrices_cy = t(as.vector(t(A)) + t(B) %*% t(X_EA.ts))
fittedYields_cy = - t(t(fittedLogPrices_cy) / ttm)
fittedYields_cy_EA.ts <- xts(fittedYields_cy, order.by=current_dates_EA)
rm(fittedLogPrices_cy,fittedYields_cy,A,B)

#RISK NEUTRAL YIELDS cycle
RiskFreeLogPrices_cy = t(as.vector(t(A_rf)) + t(B_rf) %*% t(X_EA.ts))
RiskFreeYields_cy = - t(t(RiskFreeLogPrices_cy) / ttm)
RiskFreeYields_cy_EA.ts <- xts(RiskFreeYields_cy, order.by=current_dates_EA)
rm(RiskFreeLogPrices_cy,RiskFreeYields_cy,A_rf,B_rf)

#######################
#TERM PREMIA
########################
termpremia_EA.ts = fittedYields_cy_EA.ts - RiskFreeYields_cy_EA.ts
#rm(nss_cycle_EA,RiskFreeYields_cy_EA.ts)


tseries::adf.test(data_EA.ts$cycle[current_dates_EA], k = 0)

plot(X_EA.ts)

plot(nss_cycle_EA[,40],main = "Cycle Fit")
lines(fittedYields_cy_EA.ts[,40], col = "blue", lwd = 2)
xts::addLegend("topright", legend.names = c("Acutal","Fit"),
               col = c("black","blue"), lty = c(1,1), lwd = c(2,2))

plot(r_EA.ts)
# lines(r_US.ts, col = "blue", lwd = 2)
# lines(r_pool.ts, col = "red", lwd = 2)
# lines(r_EA_USc.ts, col = "orange", lwd = 2)
# addLegend("topright",legend.names = c("EA_trend","US_trend","Pooled_Trend","US_coef_Trend"),
#           col = c("black","blue","red","orange"), lty = c(1,1,1,1), lwd = c(2,2,2,2))
