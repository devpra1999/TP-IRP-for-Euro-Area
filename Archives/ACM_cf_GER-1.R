## -----------------------------------------------------------------------------
rm(list=ls())                                                                   # Clear the environment 

## -----------------------------------------------------------------------------
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))                     # Set the appropriate environment to work                                                                                # in every machine

## -----------------------------------------------------------------------------
listofpackages <- c("psych","pracma","zoo","lubridate","dplyr","highcharter",
                    "matrixcalc","matlib","xlsx","readxl","readr","xts","vars","systemfit")
#source("aux_scripts/preparePackages.R")
#preparePackages(listofpackages)

#Load original data
N = 120
source("NSS.R")
L <- nss_yields("Data_Files/gnss.csv",N)
rawYields <- L$rawYields/100
plot_dates <- L$plot_dates


#Zero Coupon Yields
Yields <- as.matrix(rawYields)
Yields <- zoo(Yields,plot_dates)
colnames(Yields) <- paste("n",seq(1,N),sep = "_")
T = nrow(Yields)

# Excess returns at chosen maturities.
rx_maturities = c(6,18,24,36,48,60,84,120) 
ttm = c(1:N)*(1/12)
logPrices = t(t(-rawYields)*ttm)
rf =  -1*logPrices[1:T,1]
rf_3 =  -1*logPrices[1:T,3]
rx =  logPrices[2:T, 1:(N-1)] - logPrices[1:(T-1), 2:N] - (-logPrices[1:T-1,1])
#-rf[-T] in the original code was a mistake the right command is the one above

# get data in .xts, select only relevant maturities, and generate excess returns
maturities = c(1,5,6,17,18,23,24,35,36,47,48,59,60,83,84,119,120)
sel_rawYields <- rawYields[,maturities]
colnames(sel_rawYields) <- paste("Y",maturities,sep = "_")
dates <-seq(as.Date("2000-01-01"),length=nrow(Yields), by="months")
yields.ts <- xts(sel_rawYields, order.by=dates)
plot(yields.ts)
#generate hpr and excess hpr with an approximation error  
yields.ts$hpr_6 <- (1/12)*lag.xts(yields.ts$Y_6)- (6/12-1/12)*(yields.ts$Y_5-lag.xts(yields.ts$Y_6))
yields.ts$hpr_18<- (1/12)*lag.xts(yields.ts$Y_18)- (18/12-1/12)*(yields.ts$Y_17-lag.xts(yields.ts$Y_18))
yields.ts$hpr_24<- (1/12)*lag.xts(yields.ts$Y_24)- (24/12-1/12)*(yields.ts$Y_23-lag.xts(yields.ts$Y_24))
yields.ts$hpr_36<- (1/12)*lag.xts(yields.ts$Y_36)- (36/12-1/12)*(yields.ts$Y_35-lag.xts(yields.ts$Y_36))
yields.ts$hpr_48<- (1/12)*lag.xts(yields.ts$Y_48)- (48/12-1/12)*(yields.ts$Y_47-lag.xts(yields.ts$Y_48))
yields.ts$hpr_60<- (1/12)*lag.xts(yields.ts$Y_60)- (60/12-1/12)*(yields.ts$Y_59-lag.xts(yields.ts$Y_60))
yields.ts$hpr_84<- (1/12)*lag.xts(yields.ts$Y_84)- (84/12-1/12)*(yields.ts$Y_83-lag.xts(yields.ts$Y_84))
yields.ts$hpr_120<- (1/12)*lag.xts(yields.ts$Y_120)- (120/12-1/12)*(yields.ts$Y_119-lag.xts(yields.ts$Y_120))

yields.ts$hper_6=yields.ts$hpr_6-(1/12)*lag.xts(yields.ts$Y_1)
yields.ts$hper_18=yields.ts$hpr_18-(1/12)*lag.xts(yields.ts$Y_1)
yields.ts$hper_24=yields.ts$hpr_24-(1/12)*lag.xts(yields.ts$Y_1)
yields.ts$hper_36=yields.ts$hpr_36-(1/12)*lag.xts(yields.ts$Y_1)
yields.ts$hper_48=yields.ts$hpr_48-(1/12)*lag.xts(yields.ts$Y_1)
yields.ts$hper_60=yields.ts$hpr_60-(1/12)*lag.xts(yields.ts$Y_1)
yields.ts$hper_84=yields.ts$hpr_84-(1/12)*lag.xts(yields.ts$Y_1)
yields.ts$hper_120=yields.ts$hpr_120-(1/12)*lag.xts(yields.ts$Y_1)

########################
# K Principal Components
########################

K=5
demeanedyield = scale(Yields, scale = FALSE)
k = prcomp((demeanedyield))
yieldPCs = demeanedyield %*% (k$rotation)

# get the principal components in .xts format and add them to yields.ts

factors.ts<- xts(yieldPCs[,1:K], order.by=dates)
yields.ts <- merge(yields.ts, factors.ts, join = 'outer')
X<-factors.ts

################################################################
#Step 1 - estimate VAR(1) for the time series of pricing factors
################################################################
#X <- zoo(yieldPCs[,1:K],plot_dates)
#X_t1 <-  X[2:T,] 
#X_t <-  X[1:(T-1),]
#mod1 <- lm(X_t1 ~ X_t)
#mu <- coef(mod1)[1,]
#phi <- t(coef(mod1)[-1,])
#v <- mod1$residuals
#Sigma <-  t(v)%*%v/nrow(v)

#Step 1 - using.xts, this needs the package "vars"

#my_var_model <- VAR(factors.ts, p = 1, type = "none") 
my_var_model <- VAR(factors.ts, p = 1, type = "const") 
summary(my_var_model)
coeff_matrices <- coef(my_var_model)
phi <- rbind(coeff_matrices$PC1[1:5,1],coeff_matrices$PC2[1:5,1],
                         coeff_matrices$PC3[1:5,1],coeff_matrices$PC4[1:5,1],
                         coeff_matrices$PC5[1:5,1])
mu<- rbind(coeff_matrices$PC1[6,1],coeff_matrices$PC2[6,1],
            coeff_matrices$PC3[6,1],coeff_matrices$PC4[6,1],
            coeff_matrices$PC5[6,1])
resmat <- residuals(my_var_model)
Sigma <-  t(resmat)%*%resmat/nrow(resmat)

#merge residuals into .xts database 
colnames(resmat) <- paste("v",colnames(resmat),sep = "_")
dates_lag <-seq(as.Date("2000-02-01"),length=nrow(yields.ts)-1, by="months")
resmat.ts <- xts(resmat, order.by=dates_lag)
yields.ts <- merge(yields.ts, resmat.ts, join = 'outer')

####################################
#Step 2 - regress log excess returns
####################################

#rx_maturities <- c(6, 18, 24, 36, 48, 60, 84, 120)
#selected_rx <- t(rx[,rx_maturities-1])
#VX <- as.matrix(bind_cols(v,X_t))
#mod2 <- lm(t(selected_rx) ~ VX)
#coefficients <- coef(mod2)

#a <- coefficients[1,]
#beta <- coefficients[2:(K+1), ]
#c <- coefficients[(K+2):nrow(coefficients),]

#E <- mod2$residuals
#sigmasq_ret = tr(t(E) %*% E)/(nrow(E)*ncol(E))
#THIS is correct  only if the pricing error has the same variance 
# across maturities otherwise we should have instead 
#Sigma_v=(t(E) %*% E)/(nrow(E))
#sigmasq_retc <- as.matrix(diag(Sigma_v))


#Step 2 - regress log excess returns using .xts and system 
#this needs the package systemfit

#lags of factors 
yields.ts$PC1_lag=lag.xts(yields.ts$PC1)
yields.ts$PC2_lag=lag.xts(yields.ts$PC2)
yields.ts$PC3_lag=lag.xts(yields.ts$PC3)
yields.ts$PC4_lag=lag.xts(yields.ts$PC4)
yields.ts$PC5_lag=lag.xts(yields.ts$PC5)

###----------------------
###System estimation for excess returns 
###----------------------

eqhper_6r <- hper_6 ~  PC1_lag+ PC2_lag+PC3_lag+PC4_lag+PC5_lag
eqhper_18r <- hper_18 ~  PC1_lag+ PC2_lag+PC3_lag+PC4_lag+PC5_lag
eqhper_24r <- hper_24 ~  PC1_lag+ PC2_lag+PC3_lag+PC4_lag+PC5_lag
eqhper_36r <- hper_36 ~  PC1_lag+ PC2_lag+PC3_lag+PC4_lag+PC5_lag
eqhper_48r <- hper_48 ~  PC1_lag+ PC2_lag+PC3_lag+PC4_lag+PC5_lag
eqhper_60r <- hper_60 ~  PC1_lag+ PC2_lag+PC3_lag+PC4_lag+PC5_lag
eqhper_84r <- hper_84 ~  PC1_lag+ PC2_lag+PC3_lag+PC4_lag+PC5_lag
eqhper_120r <- hper_120 ~  PC1_lag+ PC2_lag+PC3_lag+PC4_lag+PC5_lag


system_hper_r <- list(eqhper_6r,eqhper_18r,eqhper_24r,eqhper_36r,eqhper_48r,eqhper_60r,
                    eqhper_84r,eqhper_120r)

eqhper_6 <- hper_6 ~  v_PC1+ v_PC2+v_PC3+v_PC4+v_PC5+
                      +PC1_lag+ PC2_lag+PC3_lag+PC4_lag+PC5_lag
eqhper_18 <- hper_18 ~  v_PC1+ v_PC2+v_PC3+v_PC4+v_PC5+
  +PC1_lag+ PC2_lag+PC3_lag+PC4_lag+PC5_lag
eqhper_24 <- hper_24 ~  v_PC1+ v_PC2+v_PC3+v_PC4+v_PC5+
  +PC1_lag+ PC2_lag+PC3_lag+PC4_lag+PC5_lag
eqhper_36 <- hper_36 ~  v_PC1+ v_PC2+v_PC3+v_PC4+v_PC5+
  +PC1_lag+ PC2_lag+PC3_lag+PC4_lag+PC5_lag
eqhper_48 <- hper_48 ~  v_PC1+ v_PC2+v_PC3+v_PC4+v_PC5+
  +PC1_lag+ PC2_lag+PC3_lag+PC4_lag+PC5_lag
eqhper_60 <- hper_60 ~  v_PC1+ v_PC2+v_PC3+v_PC4+v_PC5+
  +PC1_lag+ PC2_lag+PC3_lag+PC4_lag+PC5_lag
eqhper_84 <- hper_84 ~  v_PC1+ v_PC2+v_PC3+v_PC4+v_PC5+
  +PC1_lag+ PC2_lag+PC3_lag+PC4_lag+PC5_lag
eqhper_120 <- hper_120 ~ v_PC1+ v_PC2+v_PC3+v_PC4+v_PC5+
  +PC1_lag+ PC2_lag+PC3_lag+PC4_lag+PC5_lag


system_hper_unr <- list(eqhper_6,eqhper_18,eqhper_24,eqhper_36,eqhper_48,eqhper_60,
                      eqhper_84,eqhper_120)

#estimation 
system_hper_r_fit <- systemfit( system_hper_r, "OLS", data = yields.ts )
summary(system_hper_r_fit)

system_hper_unr_fit <- systemfit( system_hper_unr, "OLS", data = yields.ts )
summary(system_hper_unr_fit)

coeffmat_sys <- coef(system_hper_unr_fit)
res_sys<- residuals(system_hper_unr_fit)
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
mod3 <- lm(rf ~ factors.ts)

delta0 <- coef(mod3)[1]
delta1 <- coef(mod3)[-1]
A[1] = -delta0 + 0.5*(sigmasq_ret)
B[, 1] = - delta1
mod3_3 <- lm(rf_3~X)
delta0_3 <- coef(mod3_3)[1]
delta1_3 <- coef(mod3_3)[-1]
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
termpremia.ts <- xts(termpremia, order.by=dates)
plot(termpremia.ts[,120])

#Computing model expected short rates
#We make a Txn matrix with each row containing 1 to "n" months ahead model implied rates
n <- 120 #Time till which forecasts are made
ESTR <- matrix(0,T,n)
for (i in 1:n){
  ESTR[,i] <- (delta0 + t(delta1 %*% matrix.power(phi,i) %*% t(X)))/ttm[1]
  #Correction term - adding it does not make a difference - accounts for mu
  correction <- delta1 %*% mu
  for (j in 1:(i-1)){
    correction <- correction + delta1 %*% matrix.power(phi,j) %*% mu
  }
  ESTR[,i] <- ESTR[,i] + matrix(correction,1,T)/ttm[1]
}

#Monetary Policy Component from ACM model as per our methodology 
MP_ACM <- ESTR[,1] + (1-1/n) * (ESTR[,1] - fittedYields[,1])
for (i in 2:n){
  MP_ACM <- MP_ACM + (1-i/n)*(ESTR[,i] - ESTR[,i-1])
}

n <- 120
ESTR_3 <- matrix(0,T,n)
for (i in 3:n){
  ESTR_3[,i] <- (delta0_3 + t(delta1_3 %*% matrix.power(phi,i) %*% t(X)))/ttm[3]
  #Correction term - adding it does not make a difference - accounts for mu
  correction <- delta1_3 %*% mu
  for (j in 1:(i-1)){
    correction <- correction + delta1_3 %*% matrix.power(phi,j) %*% mu
  }
  ESTR_3[,i] <- ESTR_3[,i] + matrix(correction,1,T)/ttm[3]
}
n <- 40
MP_ACM_3 <- ESTR_3[,3] + (1-1/n) * (ESTR_3[,3] - fittedYields[,3])
for (i in seq(6,120,3)){
  MP_ACM_3 <- MP_ACM_3 + (1-i/(3*n))*(ESTR_3[,i] - ESTR_3[,i-3])
}

#Plotting Interest Rate Projections
#Start date - 1 month ahead of the last observation
start <- ymd(plot_dates[T])
start_3 <- ymd(plot_dates[T])

#End date - "Y" years ahead
Y <- 3
end <- ymd(plot_dates[T]) %m+% period(paste(Y,"years"))

plot_dates_proj <- seq(as.Date(start),as.Date(end), length.out = Y*12 + 1)
plot_dates_proj_3 <- seq(as.Date(start_3),as.Date(end), length.out = Y*4 + 1)

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



###----------------------
###Forecasting  
###----------------------

#X_F<-X["1980-03-01/2005-12-01"]
var_forc <- VAR(X, p = 1, type = "const") 
summary(var_forc)
X_FOR <- predict(var_forc, n.ahead = 120)
X_PRED<-cbind(X_FOR$fcst$PC1[,1],X_FOR$fcst$PC2[,1],X_FOR$fcst$PC3[,1],
              X_FOR$fcst$PC4[,1],X_FOR$fcst$PC5[,1])
Y_1_pred<- -(A[1]+t(B[, 1])%*%t(X_PRED))/ttm[1]
Y_120_pred<- -(A[120]+t(B[, 120])%*%t(X_PRED))/ttm[120]
dates_for <-seq(as.Date("2023-12-01"),length=120, by="months")
Y_1_pred.ts<- xts(t(Y_1_pred), order.by=dates_for)
Y_120_pred.ts<- xts(t(Y_120_pred), order.by=dates_for)
Y_1_pred.ts <- merge(Y_1_pred.ts, yields.ts$Y_1, join = 'outer')
Y_120_pred.ts <- merge(Y_120_pred.ts, yields.ts$Y_120, join = 'outer')

colnames(Y_120_pred.ts) <- c("Y_120for","Y_120")
colnames(Y_1_pred.ts) <- c("Y_1for","Y_1")


plotforc3m <- data.frame(
  Date = index(Y_1_pred.ts),
  Actual = coredata(Y_1_pred.ts$Y_1),
  Forecast =coredata(Y_1_pred.ts$Y_1for) ) %>% filter(Date >= "2022-01-01")

plotforc10y <- data.frame(
  Date = index(Y_120_pred.ts),
  Actual = coredata(Y_120_pred.ts$Y_120),
  Forecast =coredata(Y_120_pred.ts$Y_120for) )

plot<-ggplot(plotforc10y, aes(x = Date)) +
  geom_line(aes(y = Y_120, color = "10-Year Yields"), size = 1) +
  geom_line(aes(y = Y_120for, color = "10-year forecast"), linetype = "dashed", size = 1) +
  #geom_line(aes(y = FF.n_40plot, color = "Our Model"), linetype = "dashed", size = 1) +
  labs(x = "Date", y = " Forecast", title = "10-year Yields") +
  scale_color_manual(values = c("10-Year Yields" = "blue", "10-year forecast" = "red","Our Model" = "green")) +
  theme_minimal() +
  theme(
    legend.position = "top",
    plot.title = element_text(hjust = 0.5)  # Center the title
  ) +
  guides(color = guide_legend(title = ""))
plot <- plot +
  geom_vline(xintercept = as.Date("2005-12-01"), color = "black")

plot_1<-ggplot(plotforc3m, aes(x = Date)) +
  geom_line(aes(y = Y_1*100, color = "1-month fitted rates"), size = 1) +
  geom_line(aes(y = Y_1for*100, color = "1-month rate forecast"), linetype = "dashed", size = 1) +
  #geom_line(aes(y = FF.n_40plot, color = "Our Model"), linetype = "dashed", size = 1) +
  labs(x = "Date", y = " Forecast", title = "1-month rate forecasts") +
  scale_color_manual(values = c("1-month fitted rates" = "blue", "1-month rate forecast" = "red","Our Model" = "green")) +
  theme_minimal() +
  theme(
    legend.position = "top",
    plot.title = element_text(hjust = 0.5)  # Center the title
  ) +
  guides(color = guide_legend(title = ""))
#plot_1 <- plot_1 +
#  geom_vline(xintercept = as.Date("2005-12-01"), color = "black")
