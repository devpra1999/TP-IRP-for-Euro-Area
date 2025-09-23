## -----------------------------------------------------------------------------
rm(list=ls())                                                                   # Clear the environment 

## -----------------------------------------------------------------------------
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))                     # Set the appropriate environment to work                                                                                # in every machine

## -----------------------------------------------------------------------------
listofpackages <- c("psych","pracma","zoo","lmtest","lubridate","dplyr",
                    "matrixcalc","matlib","xlsx","readxl","readr","xts","vars",
                    "systemfit","tidyverse",  "ggplot2", "stargazer", 
                    "tseries", "dynlm","sandwich" )

for (j in listofpackages){
  if(sum(installed.packages()[, 1] == j) == 0) {
    install.packages(j)
  }
  library(j, character.only = T)
}
#source("aux_scripts/preparePackages.R")
#preparePackages(listofpackages)
#rm(listofpackages, preparePackages)

## -----------------------------------------------------------------------------
## Load NS data 
## -----------------------------------------------------------------------------

load('data/raw/yields1.rda')

#identify the maturity, in this case 60 quarters one period is one quarter. 
N=60
ttm = c(1:N)*(1/4)
maturities<-c(1:N)

## -----------------------------------------------------------------------------
## Load macro data 
## -----------------------------------------------------------------------------

load('data/raw/rawdata1.rda')

# Inflation: π (pi)
rawdata.ts$LP     <- log(rawdata.ts$PCECTPI)
rawdata.ts$pi     <- (rawdata.ts$LP - lag.xts(rawdata.ts$LP, 4))
rawdata.ts$pi_gap <- rawdata.ts$pi - rawdata.ts$PTR

# Output gap: y_gap 
rawdata.ts$y      <- log(rawdata.ts$GDPC1)
rawdata.ts$y_pot  <- log(rawdata.ts$GDPPOT)
rawdata.ts$y_gap  <- (rawdata.ts$y - rawdata.ts$y_pot)
rawdata.ts$dy_pot <- (rawdata.ts$y_pot - lag.xts(rawdata.ts$y_pot, 4))
plot(rawdata.ts$dy_pot)
#summary(dynlm(y_gap ~ L(y_gap)))

# Interest rates
rawdata.ts$y3m_cc<-log(1+0.01*rawdata.ts$DGS3MO)
rawdata.ts$US_RFcc<-log(1+0.01*rawdata.ts$US_RF)
#rawdata.ts$y3m_ccq<-0.25*log(1+0.01*rawdata.ts$DGS3MO)*100

# rescale inflation target 
rawdata.ts$PTR<-0.01*rawdata.ts$PTR
## -----------------------------------------------------------------------------
## select sample  
## -----------------------------------------------------------------------------
nss_yields<-nss_yields["1980-03-01/2023-06-01"]
rawdata.ts<-rawdata.ts["1980-03-01/2050-12-01"]
dates <- index(nss_yields)
dates_lag <-seq(as.Date("1980-06-01"),length=nrow(nss_yields)-1, by="quarters")


###################################
# Factors 
###################################

data.ts <-merge(rawdata.ts, nss_yields$n_1, join = 'outer')
data.ts$rf<-0.25*data.ts$n_1
longrun<-lm(rf~MY+dy_pot+PTR-1,data=data.ts["1980-03-01/2023-06-01"])
summary(longrun)
longrun_a<-lm(n_1~MY+dy_pot+PTR-1,data=data.ts["1980-03-01/2023-06-01"])
summary(longrun_a)
stargazer(longrun_a, se=list(coeftest(longrun_a)[,2]), type='latex')
u_t1 <- ts(resid(longrun_a), start=c(1980,1), frequency = 4)
adf.test(u_t1,  k = 0, )
u_t1lag<-lag.xts(u_t1)
u_t1diff<-diff(u_t1)
adftest<-lm(u_t1diff ~ 0 + u_t1lag[2:174])
summary(adftest)
#create trend and cycle for the entire term structure 
data.ts$dy_pot["2026-03-01/2038-06-01"]<-0.0175
data.ts$PTR["2023-09-01/2038-06-01"]<-0.02
data.ts$trend<-longrun$coefficients[1]*data.ts$MY+longrun$coefficients[2]*data.ts$dy_pot+
  longrun$coefficients[3]*data.ts$PTR
data.ts$cycle<-data.ts$rf-data.ts$trend
adf.test(na.omit(data.ts$cycle))
data.ts$cycle_Lag1 <- lag.xts(data.ts$cycle, 1)
shortrun <- lm(cycle ~ cycle_Lag1-1, data = data.ts)
summary(shortrun)
# Run a dynamic linear regression
#shortrun <- lm(cycle ~ cycle_Lag1+pi_gap+y_gap-1, data = data.ts)
#summary(shortrun)

#create a matrix to generate all trend and put data.ts$trend in the first column

data.df<-as.data.frame(data.ts$trend["1980-03-01/2038-06-01"])
T=nrow(data.df)
N=60
Y_trend <- matrix(0,T,N)
Y_trend[,1]<-4*data.df[,1]

for (i in 2: N){
  for (j in 1:(T - N)) {  
    Y_trend[j, i] <- (Y_trend[j, (i-1)] + Y_trend[j + 1, 1])
   
  }}

for (i in 1:N){Y_trend[, i]<-Y_trend[, i]/i}

Y_trend_df<- data.frame(Y_trend[1:174,])
Y_trend.ts<-xts(Y_trend_df, order.by=dates)
colnames(Y_trend_df) <- paste("n",seq(1,N),sep = "_")
nss_yields_df<-data.frame( dates = index(nss_yields), Data=coredata(nss_yields))
nss_cycle<-nss_yields_df[,2:61]-Y_trend_df
colnames(nss_cycle) <- paste("n",seq(1,N),sep = "_")
nss_cycle <- xts(nss_cycle, order.by=dates)

# saving the data for plots to be executed in ACM_cf_US3.R

plot10Y_tr <- data.frame(
  Date = index(nss_yields),
  Actual = nss_yields[, "n_40"],
  Trend = Y_trend_df[, "n_40"]
)

plot1q_tr <- data.frame(
  Date = index(nss_yields),
  Actual = nss_yields[, "n_1"],
  Trend = Y_trend_df[, "n_1"]
)

###################################
# K Principal Components of cycle as risk factors
###################################
K=5
demeanedcyle = scale(nss_cycle, scale = FALSE)
k = prcomp((demeanedcyle))
cyclePCs = demeanedcyle %*% (k$rotation)

# get the principal components in .xts format and add them to yields.ts

X<- xts(cyclePCs[,1:K], order.by=dates)
X1.df <- data.frame(
  Date = index(X),X)
save(X1.df,file="data/output/X_ff.rda")
nss_yields <- merge(nss_yields, X, join = 'outer')

################################################################
#Step 1 - estimate VAR(1) for the time series of pricing factors
################################################################
#using.xts, this needs the package "vars"

my_var_model <- VAR(X, p = 1, type = "const") 
summary(my_var_model)
#cointegration analysis
result <- ca.jo(X, type = "eigen", ecdet = "const",K=2)
summary(result)

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
resmat.ts <- xts(resmat, order.by=dates_lag)
nss_yields <- merge(nss_yields, resmat.ts, join = 'outer')

####################################
#Step 2 - regress log excess returns
####################################
#generate excess returns
#generate hpr and excess hpr with an approximation error  
nss_yields$hpr4 <- (1/4)*lag.xts(nss_yields$n_4)- (4/4-1/4)*(nss_yields$n_3-lag.xts(nss_yields$n_4))
nss_yields$hpr8<- (1/4)*lag.xts(nss_yields$n_8)- (8/4-1/4)*(nss_yields$n_7-lag.xts(nss_yields$n_8))
nss_yields$hpr12<- (1/4)*lag.xts(nss_yields$n_12)- (12/4-1/4)*(nss_yields$n_11-lag.xts(nss_yields$n_12))
nss_yields$hpr16<- (1/4)*lag.xts(nss_yields$n_16)- (16/4-1/4)*(nss_yields$n_15-lag.xts(nss_yields$n_16))
nss_yields$hpr20<- (1/4)*lag.xts(nss_yields$n_20)- (20/4-1/4)*(nss_yields$n_19-lag.xts(nss_yields$n_20))
nss_yields$hpr28<- (1/4)*lag.xts(nss_yields$n_28)- (28/4-1/4)*(nss_yields$n_27-lag.xts(nss_yields$n_28))
nss_yields$hpr40<- (1/4)*lag.xts(nss_yields$n_40)- (40/4-1/4)*(nss_yields$n_39-lag.xts(nss_yields$n_40))
nss_yields$hpr60<- (1/4)*lag.xts(nss_yields$n_60)- (60/4-1/4)*(nss_yields$n_59-lag.xts(nss_yields$n_60))

nss_yields$hper4=nss_yields$hpr4-(1/4)*lag.xts(nss_yields$n_1)
nss_yields$hper8=nss_yields$hpr8-(1/4)*lag.xts(nss_yields$n_1)
nss_yields$hper12=nss_yields$hpr12-(1/4)*lag.xts(nss_yields$n_1)
nss_yields$hper16=nss_yields$hpr16-(1/4)*lag.xts(nss_yields$n_1)
nss_yields$hper20=nss_yields$hpr20-(1/4)*lag.xts(nss_yields$n_1)
nss_yields$hper28=nss_yields$hpr28-(1/12)*lag.xts(nss_yields$n_1)
nss_yields$hper40=nss_yields$hpr40-(1/4)*lag.xts(nss_yields$n_1)
nss_yields$hper60=nss_yields$hpr60-(1/4)*lag.xts(nss_yields$n_1)


#lags of factors 
nss_yields$PC1_lag=lag.xts(nss_yields$PC1)
nss_yields$PC2_lag=lag.xts(nss_yields$PC2)
nss_yields$PC3_lag=lag.xts(nss_yields$PC3)
nss_yields$PC4_lag=lag.xts(nss_yields$PC4)
nss_yields$PC5_lag=lag.xts(nss_yields$PC5)


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

#estimation 
system_hper_r_fit <- systemfit( system_hper_r, "OLS", data = nss_yields )
summary(system_hper_r_fit)

system_hper_unr_fit <- systemfit( system_hper_unr, "OLS", data = nss_yields )
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
nss_cycle$rf<-0.25*nss_cycle$n_1
mod3 <- lm(nss_cycle$rf ~ X)
delta0 <- coef(mod3)[1]
delta1 <- coef(mod3)[-1]

A[1] = -delta0 + 0.5*(sigmasq_ret)
B[, 1] = - delta1
#mod3_3 <- lm(rf_3~X)
#delta0_3 <- coef(mod3_3)[1]
#delta1_3 <- coef(mod3_3)[-1]
for (i in 2:N){
  cat(t(B[, i-1]) %*% Sigma %*% B[, i-1],sigmasq_ret)
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
###############################
#FITTED YIELDS cycle
###############################
fittedLogPrices_cy = t(as.vector(t(A)) + t(B) %*% t(X))
fittedYields_cy = - t(t(fittedLogPrices_cy) / ttm)
#fittedYields_cy.ts <- xts(fittedYields_cy, order.by=dates)

#plot actual and fitted 10-year and 3-month  cycles
plotactfit10Y_cy <- data.frame(
  Date = index(nss_cycle),
  Actual = nss_cycle[, "n_60"],
  Trend = fittedYields_cy[, 60]
)

plot <-ggplot(plotactfit10Y_cy, aes(x = Date)) +
  geom_line(aes(y = n_60, color = "n_60"), size = 1) +
  geom_line(aes(y = Trend, color = "Trend"), linetype = "dashed", size = 1) +
  labs(x = "Date", y = "Yield", title = "Actual vs. cycle 10-Y Yield") +
  scale_color_manual(values = c("n_60" = "blue", "Trend" = "red")) +
  theme_minimal() +
  theme(
    legend.position = "top",
    plot.title = element_text(hjust = 0.5)  # Center the title
  ) +
  guides(color = guide_legend(title = "Series"))
ggsave("figures/fig_ff_actfit_cy1.pdf", plot = plot)


plotactfit3m_cy <- data.frame(
  Date = index(nss_cycle),
  Actual = nss_cycle[, "n_1"],
  Trend = fittedYields_cy[, 1]
)
plot <-ggplot(plotactfit3m_cy, aes(x = Date)) +
  geom_line(aes(y = n_1, color = "n_1"), size = 1) +
  geom_line(aes(y = Trend, color = "Trend"), linetype = "dashed", size = 1) +
  labs(x = "Date", y = "Yield", title = "Actual vs. cycle 10-Y Yield") +
  scale_color_manual(values = c("n_1" = "blue", "Trend" = "red")) +
  theme_minimal() +
  theme(
    legend.position = "top",
    plot.title = element_text(hjust = 0.5)  # Center the title
  ) +
  guides(color = guide_legend(title = "Series"))
ggsave("figures/fig_ff_actfit_cy2.pdf", plot = plot)

#RISK NEUTRAL YIELDS cycle
RiskFreeLogPrices_cy = t(as.vector(t(A_rf)) + t(B_rf) %*% t(X))
RiskFreeYields_cy = - t(t(RiskFreeLogPrices_cy) / ttm)
#######################
#TERM PREMIA
########################
termpremia = fittedYields_cy - RiskFreeYields_cy
plot_tp <- data.frame(
  Date = index(nss_cycle),
  TP40 = termpremia[, 40],
  TP4 = termpremia[, 4]
)

###############################
#FITTED YIELDS 
###############################

fittedYields=Y_trend_df+fittedYields_cy

plotactfit10Y <- data.frame(
  Date = index(nss_yields),
  Actual = nss_yields[, 40],
  Fitted = fittedYields[, 40]
)
plotactfit3m <- data.frame(
  Date = index(nss_yields),
  Actual = nss_yields[, 1],
  Fitted = fittedYields[, 1]
)

###----------------------
###Forecasting  
###----------------------

X_F<-X["1980-03-01/2005-12-01"]
var_forc <- VAR(X_F, p = 1, type = "const") 
summary(var_forc)
X_FOR <- predict(var_forc, n.ahead = 70)
X_PRED<-cbind(X_FOR$fcst$PC1[,1],X_FOR$fcst$PC2[,1],X_FOR$fcst$PC3[,1],
              X_FOR$fcst$PC4[,1],X_FOR$fcst$PC5[,1])
n_1_cy_pred<- -(A[1]+t(B[, 1])%*%t(X_PRED))/ttm[1]
n_40_cy_pred<- -(A[40]+t(B[, 40])%*%t(X_PRED))/ttm[40]
dates_for <-seq(as.Date("2006-03-01"),length=70, by="quarters")
n_1_cy_pred.ts<- xts(t(n_1_cy_pred), order.by=dates_for)
n_40_cy_pred.ts<- xts(t(n_40_cy_pred), order.by=dates_for)
n_1_pred.ts <-merge(n_1_cy_pred.ts, nss_yields$n_1, join = 'outer')
n_1_pred.ts <- merge(n_1_pred.ts, Y_trend.ts[,1], join = 'outer')
colnames(n_1_pred.ts) <- c("n_1cy_pred","n_1","n_1tr")
n_1_pred.ts$n_1for <- n_1_pred.ts$n_1cy_pred+n_1_pred.ts$n_1tr

n_40_pred.ts <-merge(n_40_cy_pred.ts, nss_yields$n_40, join = 'outer')
n_40_pred.ts <- merge(n_40_pred.ts, Y_trend.ts[,40], join = 'outer')
colnames(n_40_pred.ts) <- c("n_40cy_pred","n_40","n_40tr")
n_40_pred.ts$n_40for <- n_40_pred.ts$n_40cy_pred+n_40_pred.ts$n_40tr

plotforc3m <- data.frame(
  Date = index(n_1_pred.ts),
  Actual = coredata(n_1_pred.ts$n_1),
  Forecast =coredata(n_1_pred.ts$n_1for) )

plotforc10y <- data.frame(
  Date = index(n_40_pred.ts),
  Actual = coredata(n_40_pred.ts$n_40),
  Forecast =coredata(n_40_pred.ts$n_40for) )

# saving the output from the FF model 
FF <-  data.frame(plot10Y_tr,plot1q_tr[,2:3], plotactfit10Y[,3],plotactfit3m[,3],
                    plot_tp[,2:3],plotforc10y[,3],plotforc3m[,3]
                    
)
colnames(FF) <- c("Date","n_40","n_40trend","n_1","n_1trend","n_40fit","n_1fit","TP40","TP4","n_40for","n_1for")
save(FF,file="data/output/FF.rda")