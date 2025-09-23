#Get yield curve for US
N = 60
ttm = c(1:N)*(1/4)
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

current_time_window_US <- "1980-03-01/2025-09-01"
current_dates_US <- index(nss_yields_US.ts[current_time_window_US])
dates_lag_US <-seq(as.Date("1980-06-01"),length=nrow(nss_yields_US.ts)-1, by="quarters")

#Get principal components
K=5
demeanedcyle = scale(coredata(nss_yields_US.ts), scale = FALSE)
k = prcomp((demeanedcyle))
summary(k)
PCs = demeanedcyle %*% (k$rotation)

# Get the principal components in .xts format and add them to yields.ts
X_US.ts <- xts(PCs[,1:K], order.by=current_dates_US)
nss_yields_US.ts <- merge(nss_yields_US.ts, X_US.ts, join = 'outer')
rm(demeanedcyle,k,PCs)


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

nss_yields_US.ts$hper4=nss_yields_US.ts$hpr4-(1/4)*lag.xts(nss_yields_US.ts$n_1)
nss_yields_US.ts$hper8=nss_yields_US.ts$hpr8-(1/4)*lag.xts(nss_yields_US.ts$n_1)
nss_yields_US.ts$hper12=nss_yields_US.ts$hpr12-(1/4)*lag.xts(nss_yields_US.ts$n_1)
nss_yields_US.ts$hper16=nss_yields_US.ts$hpr16-(1/4)*lag.xts(nss_yields_US.ts$n_1)
nss_yields_US.ts$hper20=nss_yields_US.ts$hpr20-(1/4)*lag.xts(nss_yields_US.ts$n_1)
nss_yields_US.ts$hper28=nss_yields_US.ts$hpr28-(1/4)*lag.xts(nss_yields_US.ts$n_1)
nss_yields_US.ts$hper40=nss_yields_US.ts$hpr40-(1/4)*lag.xts(nss_yields_US.ts$n_1)
nss_yields_US.ts$hper60=nss_yields_US.ts$hpr60-(1/4)*lag.xts(nss_yields_US.ts$n_1)

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
nss_yields_US.ts$rf<-0.25*nss_yields_US.ts$n_1
mod3_US <- lm(nss_yields_US.ts$rf ~ X_US.ts)
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

fittedLogPrices = t(as.vector(t(A)) + t(B) %*% t(X_US.ts))
fittedYields = - t(t(fittedLogPrices) / ttm)
fittedYields_US.ts <- xts(fittedYields, order.by=current_dates_US)
rm(fittedLogPrices,fittedYields)


RiskFreeLogPrices = t(as.vector(t(A_rf)) + t(B_rf) %*% t(X_US.ts))
RiskFreeYields = - t(t(RiskFreeLogPrices) / ttm)
RiskFreeYields_US.ts <- xts(RiskFreeYields, order.by=current_dates_US)
rm(RiskFreeLogPrices,RiskFreeYields)

#######################
#TERM PREMIA
########################
termpremia_US.ts = fittedYields_US.ts - RiskFreeYields_US.ts


