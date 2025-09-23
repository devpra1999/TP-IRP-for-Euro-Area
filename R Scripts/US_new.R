
#Read macro data
PCECTPI <- read_csv("Data_Files/PCECTPI.csv")
GDPC1 <- read_csv("Data_Files/GDPC1.csv")
GDPPOT <- read_csv("Data_Files/GDPPOT.csv")
data_US <- merge(merge(PCECTPI,GDPC1),GDPPOT, all = TRUE)
###########
data_US$DATE <- data_US$DATE + months(3)
US_LR <- read_csv("data/data_us_demographic_structure.csv")
colnames(US_LR) <- c("DATE","MY")
US_LR$DATE <- US_LR$DATE + months(1)
data_US <- merge(data_US,US_LR, all = TRUE)
FRED_data <- read.delim("Data_Files/HISTDATA.TXT", sep = ",")
###########
FRED_data$DATE <- seq(as.Date("1968-04-01"),length=nrow(FRED_data), by="quarters")
PTR_US <- FRED_data %>% select(DATE, PTR)
data_US <- merge(data_US,PTR_US, all = TRUE)
data_US$PTR <- data_US$PTR/100
data_US$MY <- data_US$MY/100
rm(PCECTPI,GDPC1,GDPPOT,US_LR,FRED_data,PTR_US)

data_US$LP <- log(data_US$PCECTPI)
data_US$pi <- data_US$LP - lag(data_US$LP,4)
data_US$y <- log(data_US$GDPC1)
data_US$y_pot <- log(data_US$GDPPOT)
data_US$y_gap  <- (data_US$y - data_US$y_pot)
data_US$dy_pot <- (data_US$y_pot - lag.xts(data_US$y_pot, 4))
data_US$pi_gap <- data_US$pi - data_US$PTR
data_US.ts <- xts(data_US[2:ncol(data_US)], order.by = data_US$DATE)
index(data_US.ts) <- index(data_US.ts) - months(1)
data_US.ts$PTR["2024-06-01/2050-12-01"] <- 0.02

#Get yield curve for US
#load('data/raw/yields1.rda')
N = 40
ttm = c(1:N)*(1/4)
maturities<-c(1:N)
source("NSS.R")
L <- nss_yields("Data_Files/USnss.csv",N*3)
rawYields <- L$rawYields/100
plot_dates <- L$plot_dates
Yields <- as.matrix(rawYields)
Yields <- xts(Yields,plot_dates)
Yields <- Yields[seq(from = 1, to = nrow(Yields), by = 3),seq(from = 3, to = N*3, length.out = N)]
colnames(Yields) <- paste("n",seq(1,N),sep = "_")
T = nrow(Yields)
nss_yields_US.ts <- Yields
rm(L,rawYields,plot_dates,Yields)
# time_window_US <- "1980-03-01/2034-12-01"
current_time_window_US <- "1980-03-01/2025-09-01"

time_window_US <- "1980-03-01/2038-12-01"
current_time_window_US <- "1980-03-01/2025-09-01"
data_US.ts <- data_US.ts[time_window_US]
nss_yields_US.ts <- nss_yields_US.ts[current_time_window_US]
dates_US <- index(data_US.ts)
current_dates_US <- index(data_US.ts[current_time_window_US])
dates_lag_US <-seq(as.Date("1980-06-01"),length=nrow(nss_yields_US.ts)-1, by="quarters")

#Run trend regression
data_US.ts <- merge(data_US.ts,nss_yields_US.ts$n_1)
data_US.ts$rf <- 0.25*data_US.ts$n_1
longrun_US <- lm(rf~MY+dy_pot+PTR-1,data=data_US.ts[current_dates_US])
# longrun_US <- lm(rf~MY+dy_pot+PTR-1,data=data_US.ts[current_dates_US])
longrun_a_US<-lm(n_1~MY+dy_pot+PTR-1,data=data_US.ts[current_dates_US])

#Check stationarity of residuals
u_US <- resid(longrun_US)
u_US.ts <- xts(u_US, order.by = index(data_US.ts[current_dates_US]))
tseries::adf.test(u_US.ts,k=0)
r_US <- longrun_US$coefficients[1]*data_US.ts$MY + 
  longrun_US$coefficients[2]*data_US.ts$dy_pot + 
  longrun_US$coefficients[3]*data_US.ts$PTR
#r_US <- longrun_US$coefficients[1]*data_US.ts$MY + longrun_US$coefficients[2]*data_US.ts$dy_pot + longrun_US$coefficients[3]*data_US.ts$PTR
r_US.ts <- xts(r_US, order.by = index(data_US.ts[dates_US]))
# u_US_a <- resid(longrun_a_US)
# u_US_a.ts <- xts(u_US_a, order.by = index(data_US.ts[current_dates_US]))
# r_US_a <- fitted(longrun_a_US)
# r_US_a.ts <- xts(r_US_a, order.by = index(data_US.ts[current_dates_US]))

colnames(r_US.ts) <- "trend"
data_US.ts <- merge(data_US.ts,r_US.ts)
colnames(u_US.ts) <- "cycle"
data_US.ts <- merge(data_US.ts,u_US.ts)

# colnames(r_US.ts) <- "trend"
# data_US.ts <- merge(data_US.ts,r_US.ts)
# colnames(u_US.ts) <- "cycle"
# data_US.ts <- merge(data_US.ts,u_US.ts)

#rm(r_US,r_US.ts,u_US,u_US.ts)
#rm(time_window_US,current_time_window_US)


#Make trend matrix-----------------------
data.df <- data_US.ts$trend[time_window_US]
T1 = nrow(data.df)
Y_trend <- matrix(0,T1,N)
Y_trend[,1]<-4*data.df[,1]

for (i in 2: N){
  for (j in 1:(T1 - i)) {
    Y_trend[j, i] <- (Y_trend[j, (i-1)] + Y_trend[(j +i- 1), 1])
  }}

for (i in 1:N){Y_trend[, i]<-Y_trend[, i]/i}
Y_trend_df<- data.frame(Y_trend[1:length(dates_US),])
colnames(Y_trend_df) <- paste("n",seq(1,N),sep = "_")
Y_trend.ts<-xts(Y_trend_df, order.by=dates_US)
rm(Y_trend_df,Y_trend,data.df,T1)

#Extract cycle
# nss_yields_df<-data.frame(dates = index(nss_yields_US.ts), Data=coredata(nss_yields_US.ts))
# nss_cycle<-nss_yields_df[,2:61]-Y_trend_df[,1]
# colnames(nss_cycle) <- paste("n",seq(1,N),sep = "_")
# nss_cycle_US <- xts(nss_cycle, order.by=current_dates_US)

#Extract cycle
N = dim(nss_yields_US.ts)[2]
ttm = c(1:N)*(1/4)
#nss_cycle_US <- sweep(nss_yields_US.ts,1,r_US_a.ts,FUN = "-")
nss_cycle_US <- sweep(nss_yields_US.ts,1,Y_trend.ts[current_dates_US,1],FUN = "-")
#nss_cycle_US <- nss_yields_US.ts - Y_trend.ts[current_dates_US]
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
#nss_yields_US.ts$hpr60<- (1/4)*lag.xts(nss_yields_US.ts$n_60)- (60/4-1/4)*(nss_yields_US.ts$n_59-lag.xts(nss_yields_US.ts$n_60))

nss_yields_US.ts$hper4=nss_yields_US.ts$hpr4-(1/4)*lag.xts(nss_yields_US.ts$n_1)
nss_yields_US.ts$hper8=nss_yields_US.ts$hpr8-(1/4)*lag.xts(nss_yields_US.ts$n_1)
nss_yields_US.ts$hper12=nss_yields_US.ts$hpr12-(1/4)*lag.xts(nss_yields_US.ts$n_1)
nss_yields_US.ts$hper16=nss_yields_US.ts$hpr16-(1/4)*lag.xts(nss_yields_US.ts$n_1)
nss_yields_US.ts$hper20=nss_yields_US.ts$hpr20-(1/4)*lag.xts(nss_yields_US.ts$n_1)
nss_yields_US.ts$hper28=nss_yields_US.ts$hpr28-(1/4)*lag.xts(nss_yields_US.ts$n_1)
nss_yields_US.ts$hper40=nss_yields_US.ts$hpr40-(1/4)*lag.xts(nss_yields_US.ts$n_1)
#nss_yields_US.ts$hper60=nss_yields_US.ts$hpr60-(1/4)*lag.xts(nss_yields_US.ts$n_1)

# bias = -(data_US.ts$trend[current_dates_US]-lag.xts(data_US.ts$trend[current_dates_US]))
# bias = -(data_US.ts$trend[current_dates_US]-lag.xts(data_US.ts$trend[current_dates_US]))*4
# nss_yields_US.ts$hper4=nss_yields_US.ts$hpr4-(1/4)*lag.xts(nss_yields_US.ts$n_1)- (4/4-1/4)*bias
# nss_yields_US.ts$hper8=nss_yields_US.ts$hpr8-(1/4)*lag.xts(nss_yields_US.ts$n_1)- (8/4-1/4)*bias
# nss_yields_US.ts$hper12=nss_yields_US.ts$hpr12-(1/4)*lag.xts(nss_yields_US.ts$n_1)- (12/4-1/4)*bias
# nss_yields_US.ts$hper16=nss_yields_US.ts$hpr16-(1/4)*lag.xts(nss_yields_US.ts$n_1)- (16/4-1/4)*bias
# nss_yields_US.ts$hper20=nss_yields_US.ts$hpr20-(1/4)*lag.xts(nss_yields_US.ts$n_1)- (20/4-1/4)*bias
# nss_yields_US.ts$hper28=nss_yields_US.ts$hpr28-(1/4)*lag.xts(nss_yields_US.ts$n_1)- (28/4-1/4)*bias
# nss_yields_US.ts$hper40=nss_yields_US.ts$hpr40-(1/4)*lag.xts(nss_yields_US.ts$n_1)- (40/4-1/4)*bias
# nss_yields_US.ts$hper60=nss_yields_US.ts$hpr60-(1/4)*lag.xts(nss_yields_US.ts$n_1)- (60/4-1/4)*bias


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
#eqhper_60r <- hper60 ~  PC1_lag+ PC2_lag+PC3_lag+PC4_lag+PC5_lag


system_hper_r <- list(eqhper_4r,eqhper_8r,eqhper_12r,eqhper_16r,eqhper_20r,eqhper_28r,
                      eqhper_40r)
rm(eqhper_4r,eqhper_8r,eqhper_12r,eqhper_16r,eqhper_20r,eqhper_28r,
   eqhper_40r)

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
#eqhper_60 <- hper60 ~ v_PC1+ v_PC2+v_PC3+v_PC4+v_PC5+
#  +PC1_lag+ PC2_lag+PC3_lag+PC4_lag+PC5_lag


system_hper_unr <- list(eqhper_4,eqhper_8,eqhper_12,eqhper_16,eqhper_20,eqhper_28,
                        eqhper_40)
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

m1=matrix(coeffmat_sys,nrow=7,ncol=11,byrow=TRUE)
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
rm(fittedLogPrices_cy,fittedYields_cy)

#RISK NEUTRAL YIELDS cycle
RiskFreeLogPrices_cy = t(as.vector(t(A_rf)) + t(B_rf) %*% t(X_US.ts))
RiskFreeYields_cy = - t(t(RiskFreeLogPrices_cy) / ttm)
RiskFreeYields_cy_US.ts <- xts(RiskFreeYields_cy, order.by=current_dates_US)
rm(RiskFreeLogPrices_cy,RiskFreeYields_cy)

#######################
#TERM PREMIA
########################
termpremia_US.ts = fittedYields_cy_US.ts - RiskFreeYields_cy_US.ts

######################
#FITTED YIELDS
######################
#fittedYields_US.ts <- Y_trend.ts[current_dates_US] + fittedYields_cy_US.ts
fittedYields_US.ts <- sweep(fittedYields_cy_US.ts,1,Y_trend.ts[current_dates_US,1],FUN = "+")

#rm(nss_cycle_US,RiskFreeYields_cy_US.ts)

# tseries::adf.test(data_US.ts$cycle[current_dates_US])
# 
# plot(X_US.ts)
# 
# plot(nss_cycle_US[,40],main = "Cycle Fit")
# lines(fittedYields_cy_US.ts[,40], col = "blue")
# # xts::addLegend("topright", legend.names = c("Acutal","Fit"),
# #           col = c("black","blue"), lty = c(1,1), lwd = c(2,1))
# 
# plot(nss_yields_US.ts[,40],main = "10Y Yield Fit")
# lines(fittedYields_US.ts[,40], col = "blue")
# 
# 
# plot(fittedYields_US.ts[current_dates_US,40])
# lines(Y_trend.ts[current_dates_US,40])
# lines(Y_trend.ts[current_dates_US,1])
# lines(termpremia_US.ts[current_dates_US,40])
# lines(fittedYields_US.ts[current_dates_US,40])
# lines(RiskFreeYields_cy_US.ts[current_dates_US,40])

# ###----------------------
# ###Forecasting
# ###----------------------
# 
# X_F<-X_US.ts["1980-03-01/2005-12-01"]
# var_forc <- VAR(X_F, p = 1, type = "const")
# summary(var_forc)
# X_FOR <- predict(var_forc, n.ahead = 75)
# X_PRED<-cbind(X_FOR$fcst$PC1[,1],X_FOR$fcst$PC2[,1],X_FOR$fcst$PC3[,1],
#               X_FOR$fcst$PC4[,1],X_FOR$fcst$PC5[,1])
# n_1_cy_pred<- -(A[1]+t(B[, 1])%*%t(X_PRED))/ttm[1]
# n_40_cy_pred<- -(A[40]+t(B[, 40])%*%t(X_PRED))/ttm[40]
# rm(X_FOR,X_PRED,var_forc)
# dates_for <-seq(as.Date("2006-03-01"),length=75, by="quarters")
# n_1_cy_pred.ts<- xts(t(n_1_cy_pred), order.by=dates_for)
# n_40_cy_pred.ts<- xts(t(n_40_cy_pred), order.by=dates_for)
# n_1_pred.ts <-merge(n_1_cy_pred.ts, nss_yields_US.ts$n_1, join = 'outer')
# n_1_pred.ts <- merge(n_1_pred.ts, Y_trend.ts[,1], join = 'outer')
# colnames(n_1_pred.ts) <- c("n_1cy_pred","n_1","n_1tr")
# n_1_pred.ts$n_1for <- n_1_pred.ts$n_1cy_pred+n_1_pred.ts$n_1tr
# rm(n_1_cy_pred,n_1_cy_pred.ts)
# 
# n_40_pred.ts <- merge(n_40_cy_pred.ts, nss_yields_US.ts$n_40, join = 'outer')
# n_40_pred.ts <- merge(n_40_pred.ts, Y_trend.ts[,40], join = 'outer')
# colnames(n_40_pred.ts) <- c("n_40cy_pred","n_40","n_40tr")
# n_40_pred.ts$n_40for <- n_40_pred.ts$n_40cy_pred+n_40_pred.ts$n_40tr
# rm(n_40_cy_pred,n_40_cy_pred.ts,dates_for)
# rm(X_F)
rm(r_US.ts,u_US.ts)

# #Prepare Macrodataset
# load('data/raw/rawdata1.rda')
# 
# # Inflation: π (pi)
# rawdata.ts$LP     <- log(rawdata.ts$PCECTPI)
# rawdata.ts$pi     <- (rawdata.ts$LP - lag.xts(rawdata.ts$LP, 4))
# rawdata.ts$pi_gap <- rawdata.ts$pi - rawdata.ts$PTR/100
# 
# # Output gap: y_gap
# rawdata.ts$y      <- log(rawdata.ts$GDPC1)
# rawdata.ts$y_pot  <- log(rawdata.ts$GDPPOT)
# rawdata.ts$y_gap  <- (rawdata.ts$y - rawdata.ts$y_pot)
# rawdata.ts$dy_pot <- (rawdata.ts$y_pot - lag.xts(rawdata.ts$y_pot, 4))
# #plot(rawdata.ts$dy_pot)
# #summary(dynlm(y_gap ~ L(y_gap)))
# 
# # Interest rates
# rawdata.ts$y3m_cc<-log(1+0.01*rawdata.ts$DGS3MO)
# rawdata.ts$US_RFcc<-log(1+0.01*rawdata.ts$US_RF)
# #rawdata.ts$y3m_ccq<-0.25*log(1+0.01*rawdata.ts$DGS3MO)*100
# 
# # rescale inflation target
# rawdata.ts$PTR<-0.01*rawdata.ts$PTR
# 
# 
# #Prepare yield curve dataset
# load('data/raw/yields1.rda')
# nss_yields_US.ts <- nss_yields
# rm(nss_yields)
# # maturities<-c(1:N)
# 
# #Add n1 yield to macrodataset for the regression
# data_US.ts <- merge(rawdata.ts,nss_yields_US.ts$n_1)
# rm(rawdata.ts)
# 
# #Extend data and subset to available dates
# data_US.ts$dy_pot["2026-03-01/2038-06-01"] <- 0.0175
# data_US.ts$PTR["2023-06-01/2038-06-01"] <- 0.02




