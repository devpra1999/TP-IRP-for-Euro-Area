## -----------------------------------------------------------------------------
## Run the file where the Monetary Policy model by Favero and Fernández-Fuertes
## is estimated. That will immediately add the neccesary variables to our 
## dataset
source("atsm_eu/src/getMainData.R")

## -----------------------------------------------------------------------------
## FUNCTIONS:

quad_form   <- function(b){
  c(outer(b,b))
}

getFittedPrice     <- function(A,B,X,dates,cols){
  p.mat          <- t(as.vector(t(A)) + t(B) %*% t(X))
  p.ts           <- xts(p.mat, order.by = dates)
  colnames(p.ts) <- paste0("n",1:dim(p.ts)[2]) 
  return(p.ts)
}

getAffineTScoeffs  <- function(params, N, K, flag=0){
  
  delta0  <- params$delta$delta0
  delta1  <- params$delta$delta1
  if (flag!="rf"){
    lambda0 <- params$lambda$lambda0
    lambda1 <- params$lambda$lambda1
  } else{
    lambda0 <- matrix(0,K,1)
    lambda1 <- matrix(0,K,K)
  }
  
  Phi   <- params$X$Phi
  mu    <- params$X$mu
  Sigma <- params$X$Sigma
  # Initialise A and B
  A      <- rep(0,N)
  B      <- matrix(0,K,N)
  A[1]   <- -delta0 + 0.5*(sigma2)
  B[, 1] <- -delta1
  for (n in 2:N){
    A[n]  <- A[n-1] + t(B[, n-1]) %*% (mu-lambda0) + 0.5 * (t(B[, n-1]) %*% Sigma %*% B[, n-1] + params$sigma2) - delta0
    B[,n] <- (B[, n-1]) %*% (Phi - lambda1) - delta1
  }
  return(list(A=A,B=B))
}

computeTrendMatrix <- function(init_trend, T, N) {
  # Initialize the trend matrix
  Y_trend <- matrix(NA,T,N)
  Y_trend[,1]<-init_trend
  
  for (n in 2: N){
    for (t in 1:(T - n)) {  
      Y_trend[t, n] <- (n-1)/n*Y_trend[t+1, (n-1)] + 1/n*Y_trend[t, 1]
      
    }}
  
  #for (i in 1:N){Y_trend[, i]<-Y_trend[, i]/i}
  
  return(Y_trend)
}


## -----------------------------------------------------------------------------
current_time_window <- "2001-03-01/2023-06-01"
time_window <- "2001-03-01/2038-06-01"

## -----------------------------------------------------------------------------
## Reload the data with the pure names (no EA_ prefix)
load("atsm_eu/data/rda/EA_macroData.rda")
load("atsm_eu/data/rda/EA_yieldCurve.rda")
load("atsm_eu/data/rda/EA_xr.rda")
data.ts       <- EA_macroData.ts[time_window]
dates         <- index(data.ts)
current_dates <- index(data.ts[current_time_window])
T <- length(dates)


N=60
yieldCurve.ts <- EA_yieldCurve.ts[current_time_window, c(1:N)]


# Take the maturities that are in the dataset
maturities.cols <- grep("^(n)([1-9][0-9]{0,2}|1000|0)$", colnames(yieldCurve.ts))
maturities.all  <- colnames(yieldCurve.ts)[maturities.cols]
maturities.list <- as.numeric(gsub("n", "", maturities.all))



# That's what I was mentioning before: we need this to construct the log prices:
y.ts            <- yieldCurve.ts
ttm             <- maturities.list*(1/4)
p.ts            <- -t(t(y.ts)*ttm)
colnames(p.ts)  <- colnames(y.ts)


## -----------------------------------------------------------------------------
## FACTORS
## First: Compute the trend matrix:
# init_trend            <- coredata(data.ts$r_star)
# y_trend.matrix        <- computeTrendMatrix(init_trend, T, N)
# y_trend.ts            <- xts(y_trend.matrix, order.by=dates)*4
# y_trend.ts[y_trend.ts==0] <- NA
# colnames(y_trend.ts)  <- paste("n",seq(1,N),sep = "")
# y_cycle.ts            <- sweep(y.ts[current_time_window],1,y_trend.ts[current_time_window][,1], FUN = "-")
# rm(init_trend, y_trend.matrix)
# ## -----------------------------------------------------------------------------
# ## K Principal Components
# ## Here we start replicating the procedure described in ACM2013, where they use 
# ## the first 5 principal components. We thus have to take the 5 first PC in our 
# ## dataset y.ts. Notice that some maturities are missing.
# K <- 5
# y_cycle.ts.demeaned   <- scale(y_cycle.ts, scale = FALSE)
# k                     <- prcomp((y_cycle.ts.demeaned))
# y_cycle.df.PC         <- y_cycle.ts.demeaned %*% (k$rotation)                   # Get Principal Components
# X.ts                  <- xts(y_cycle.df.PC[,1:K], order.by=current_dates)     # Get Factors (5 first PC)
# data.ts               <- merge(data.ts, X.ts, join = 'outer')                   # Merge it to data.ts, which is our main dataset
# rm(k, y_cycle.df.PC, y_cycle.ts.demeaned)
# 
# ## -----------------------------------------------------------------------------
# ## STEP 1 - estimate VAR(1) for the time series of pricing factors
# ## The model is Xt = mu + phi Xt_ + eps
# X.var <- VAR(X.ts, p = 1, type = "const") 
# summary(X.var)
# X.var.coef  <- coef(X.var)                                                      # Get all the coefficients
# X.var.names <- names(X.var.coef)
# 
# Phi <- matrix(0, nrow = K, ncol = K)
# mu  <- matrix(0, nrow = K, ncol = 1) 
# 
# # Populate matrices
# for (i in 1:K) {
#   X.var.name       <- X.var.names[i]
#   X.var.estimates  <- X.var.coef[[X.var.name]] 
#   Phi[i,]          <- X.var.estimates[1:K, 1]
#   mu[i,1]          <- X.var.estimates[K+1, 1]
# }
# rm(X.var.name, X.var.estimates, X.var.coef)
# 
# X.var.v           <- residuals(X.var)                                           # Get the residuals
# colnames(X.var.v) <- paste("v",colnames(X.var.v), sep = "_")
# dates_lag         <- seq(current_dates[2],length=length(current_dates)-1, by="quarters")
# v.ts              <- xts(X.var.v, order.by=dates_lag)
# Sigma             <- t(v.ts)%*%v.ts/nrow(v.ts)                                  # Get the variance-covariance matrix
# params.X          <- list(Phi=Phi, mu=mu, Sigma=Sigma)
# rm(dates_lag, X.var.v)
# 
# data.ts <- merge(data.ts, v.ts, join = 'outer')                                 # Merge residuals into out main xts dataset, data.ts
# 
# ## -----------------------------------------------------------------------------
# ## STEP 2 - regress log excess returns,this needs the package systemfit
# 
# # Include lags of factors
# PC_names <- paste0("PC", 1:K)
# PC_list  <- lapply(PC_names, function(name) data.ts[,name])
# data.df  <- data.frame(data.ts)
# for (pc in 1:K) {data.df[,paste0("PC", pc, "_lag")] <- lag.xts(PC_list[[pc]])}
# data.ts  <- xts(data.df, order.by = dates)
# rm(data.df, PC_list)
# 
# # Equations:
# eqhper_3r  <- hpern3  ~  PC1_lag + PC2_lag + PC3_lag #+ PC4_lag + PC5_lag
# eqhper_7r  <- hpern7  ~  PC1_lag + PC2_lag + PC3_lag #+ PC4_lag + PC5_lag
# eqhper_11r <- hpern11 ~  PC1_lag + PC2_lag + PC3_lag #+ PC4_lag + PC5_lag
# eqhper_15r <- hpern15 ~  PC1_lag + PC2_lag + PC3_lag #+ PC4_lag + PC5_lag
# eqhper_19r <- hpern19 ~  PC1_lag + PC2_lag + PC3_lag #+ PC4_lag + PC5_lag
# eqhper_27r <- hpern27 ~  PC1_lag + PC2_lag + PC3_lag #+ PC4_lag + PC5_lag
# eqhper_39r <- hpern29 ~  PC1_lag + PC2_lag + PC3_lag #+ PC4_lag + PC5_lag
# eqhper_59r <- hpern59 ~  PC1_lag + PC2_lag + PC3_lag #+ PC4_lag + PC5_lag
# 
# system_hper_r <- list(eqhper_3r, eqhper_7r, eqhper_11r, eqhper_15r, eqhper_19r,
#                       eqhper_27r, eqhper_39r,eqhper_59r)
# 
# eqhper_3  <- hpern3  ~ v_PC1 + v_PC2 + v_PC3  + PC1_lag + PC2_lag + PC3_lag #+ PC4_lag + PC5_lag  + v_PC4 + v_PC5
# eqhper_7  <- hpern7  ~ v_PC1 + v_PC2 + v_PC3  + PC1_lag + PC2_lag + PC3_lag #+ PC4_lag + PC5_lag  + v_PC4 + v_PC5
# eqhper_11 <- hpern11 ~ v_PC1 + v_PC2 + v_PC3  + PC1_lag + PC2_lag + PC3_lag #+ PC4_lag + PC5_lag  + v_PC4 + v_PC5
# eqhper_15 <- hpern15 ~ v_PC1 + v_PC2 + v_PC3  + PC1_lag + PC2_lag + PC3_lag #+ PC4_lag + PC5_lag  + v_PC4 + v_PC5
# eqhper_19 <- hpern19 ~ v_PC1 + v_PC2 + v_PC3  + PC1_lag + PC2_lag + PC3_lag #+ PC4_lag + PC5_lag  + v_PC4 + v_PC5
# eqhper_27 <- hpern27 ~ v_PC1 + v_PC2 + v_PC3  + PC1_lag + PC2_lag + PC3_lag #+ PC4_lag + PC5_lag  + v_PC4 + v_PC5
# eqhper_39 <- hpern39 ~ v_PC1 + v_PC2 + v_PC3  + PC1_lag + PC2_lag + PC3_lag #+ PC4_lag + PC5_lag  + v_PC4 + v_PC5
# eqhper_59 <- hpern59 ~ v_PC1 + v_PC2 + v_PC3  + PC1_lag + PC2_lag + PC3_lag #+ PC4_lag + PC5_lag  + v_PC4 + v_PC5
# 
# system_hper_unr <- list(eqhper_3, eqhper_7, eqhper_11, eqhper_15, eqhper_19,
#                         eqhper_27, eqhper_39, eqhper_59)
# 
# load("atsm_eu/data/rda/EA_hper.rda")
# EA_hper.ts <- EA_hper.ts[time_window]
# colnames(EA_hper.ts) <- paste0("hper", gsub("EA_n","",colnames(EA_hper.ts)))
# data.ts <- merge(data.ts, EA_hper.ts)
# 
# # Estimate:
# system_hper_r_fit <- systemfit( system_hper_r, "OLS", data = data.ts[current_time_window] )
# summary(system_hper_r_fit)
# 
# system_hper_unr_fit <- systemfit( system_hper_unr, "OLS", data = data.ts[current_time_window] )
# summary(system_hper_unr_fit)
# rm(system_hper_r, system_hper_unr)
# 
# coeffmat_sys <- coef(system_hper_unr_fit)
# res_sys      <- residuals(system_hper_unr_fit)
# res_sys      <- res_sys[-1,] #first row contains NAs because of the lag 
# E            <- as.matrix(res_sys)
# 
# sigma2 = tr(t(E) %*% E)/(nrow(E)*ncol(E))
# 
# m1=matrix(coeffmat_sys,nrow=8,ncol=K*2+1,byrow=TRUE)
# 
# a     <- m1[,1]
# b     <- t(m1[,2:(K+1)])
# c     <- t(m1[,(K+2):ncol(m1)])
# rm(m1, res_sys, coeffmat_sys)
# 
# ## -----------------------------------------------------------------------------
# ## STEP 3 - Run cross-sectional regressions
# 
# BStar   <- t(apply(t(b),1,quad_form))
# lambda0 <- Ginv(t(b)) %*% (a + 0.5*((BStar %*% c(Sigma)) + sigma2))
# lambda1 <- Ginv(t(b)) %*% t(c)
# lambda  <- list(lambda0=lambda0, lambda1=lambda1)
# rm(BStar)
# 
# 
# ## -----------------------------------------------------------------------------
# ## STEP 4 - Get the coefficients of the Affine Term Structure Model
# mod_r     <- lm(y.ts$n1/4 ~ X.ts)
# delta0     <- coef(mod_r)[1]
# delta1     <- coef(mod_r)[-1]
# delta      <- list(delta0=delta0, delta1=delta1)
# summary(mod_r)
# 
# 
# params <- list(delta=delta, X=params.X, sigma2=sigma2, lambda=lambda)
# TScoeffs <- getAffineTScoeffs(params, N, K)
# A        <- TScoeffs$A
# B        <- TScoeffs$B
# 
# #Recursions for Risk Free Yields
# TScoeffs  <- getAffineTScoeffs(params, N, K, "rf")
# A_rf      <- TScoeffs$A
# B_rf      <- TScoeffs$B
# 
# rm(TScoeffs)
# rm(a,b,c)
# 
# 
# ## -----------------------------------------------------------------------------
# ## STEP 5 - Get the fitted values
# 
# p.cols           <- colnames(p.ts)
# #FITTED YIELDS
# p_cycle.ts.fit <- getFittedPrice(A,B,X.ts,current_dates,colnames(p.ts))
# y_cycle.ts.fit <- xts(- t(t(p_cycle.ts.fit) / ttm), order.by = index(p_cycle.ts.fit))
# 
# #RISK NEUTRAL YIELDS
# p_cycle_rf.ts.fit <- getFittedPrice(A_rf,B_rf,X.ts,current_dates,colnames(p.ts))
# y_cycle_rf.ts.fit <- xts(- t(t(p_cycle_rf.ts.fit) / ttm), order.by=index(p_cycle_rf.ts.fit))
# 
# #TERM PREMIA
# termpremia = y_cycle.ts.fit - y_cycle_rf.ts.fit
# termpremia.ts <- xts(termpremia, order.by=current_dates)
# rm(termpremia)
# 
# # TREND + CYCLE
# y.ts.fit <- y_trend.ts + y_cycle.ts.fit
# rm(p.cols)
# 
# 
# 
# ## -----------------------------------------------------------------------------
# ## STEP 6: EXTRA TERMS OF THE TERM PREMIA FORMULA ARE INDEED TINY
# 
# dy_trend.ts <- y_trend.ts - lag.xts(y_trend.ts)
# summary(dy_trend.ts[,1])
# 
# differences = list() # A list to store the differences
# 
# # Looping from 1 to N
# for (i in 1:N) {
#   # Calculating the difference for each i
#   diff = y_cycle_rf.ts.fit[,i] - y_cycle.ts[,1]
#   
#   # Storing the difference in the list
#   differences[[i]] = diff
# }
# 
# # Calculating the mean and standard deviation of each difference
# means = sapply(differences, mean)
# stds = sapply(differences, sd)

## -----------------------------------------------------------------------------
## FACTORS
## First: Compute the trend matrix:
init_trend            <- coredata(data.ts$r_star)
y_trend.matrix        <- computeTrendMatrix(init_trend, T, N)*4
y_trend.ts            <- xts(y_trend.matrix, order.by=dates)
y_trend.ts[y_trend.ts==0] <- NA
colnames(y_trend.ts)  <- paste("n",seq(1,N),sep = "")
# y_cycle.matrix <- as.matrix(y.ts) - as.matrix(y_trend.ts[1:length(current_dates),])
# y_cycle.ts <- xts(y_cycle.matrix,order.by = current_dates)
y_cycle.ts            <- sweep(y.ts[current_time_window],1,y_trend.ts[current_time_window][,1], FUN = "-")
y_cycle.matrix <- as.matrix(coredata(y_cycle.ts))
rawYields <- y_cycle.matrix
plot_dates.FF <- current_dates
Yields <- as.matrix(rawYields)
Yields <- zoo(Yields,plot_dates.FF)
colnames(Yields) <- paste("n",seq(1,N),sep = "_")
T = nrow(Yields)

# Use excess returns at these maturities to estimate the model.
rx_maturities = c(6,18,24,36,48,60,84,120)/3
ttm = c(1:N)*(1/4)
logPrices = t(t(-rawYields)*ttm)
rf =  -1*logPrices[1:T,1]
rx =  logPrices[2:T, 1:(N-1)] - logPrices[1:(T-1), 2:N] - rf[-T]

# K Principal Components
K=5
demeanedyield = scale(Yields,scale = FALSE)
k = prcomp((demeanedyield))
yieldPCs = demeanedyield %*% (k$rotation)

#Step 1 - estimate VAR(1) for the time series of pricing factors
X <- zoo(yieldPCs[,1:K],plot_dates.FF)
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
fittedLogPrices.FF = t(as.vector(t(A)) + t(B) %*% t(X))
fittedYields.FF = - t(t(fittedLogPrices.FF) / ttm)

#RISK NEUTRAL YIELDS
RiskFreeLogPrices.FF = t(as.vector(t(A_rf)) + t(B_rf) %*% t(X))
RiskFreeYields.FF = - t(t(RiskFreeLogPrices.FF) / ttm)

#TERM PREMIA
termpremia_EA.FF = fittedYields.FF - RiskFreeYields.FF

fittedYields.FF = fittedYields.FF + y_trend.matrix[1:length(plot_dates.FF),]
RiskFreeYields.FF = fittedYields.FF - termpremia_EA.FF
RiskFreeYields.FF.ts = xts(RiskFreeYields.FF,order.by = index(X))

X.FF.ts <- xts(X,plot_dates.FF)
