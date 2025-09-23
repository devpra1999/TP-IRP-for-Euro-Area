################################################################################
###
### AUTHOR:       Carlo Favero and Rubén Fernández-Fuertes
### DATE:         Fall 2023
### DESCRIPTION:   
### OUTPUT:       
###
################################################################################


## -----------------------------------------------------------------------------
#rm(list=ls())                                                                   # Clear the environment 

## -----------------------------------------------------------------------------
#setwd(dirname(rstudioapi::getActiveDocumentContext()$path))                     # Set the appropriate environment to work in every machine


## -----------------------------------------------------------------------------
#listofpackages <- c("psych","pracma","zoo","lubridate","dplyr","highcharter",
#                    "matrixcalc","matlib","xlsx","readxl","readr","xts","vars",
#                    "stargazer", "systemfit")

#source("aux_scripts/preparePackages.R")
#preparePackages(listofpackages)
#rm(listofpackages, preparePackages)



## -----------------------------------------------------------------------------
## Run the file where the Monetary Policy model by Favero and Fernández-Fuertes
## is estimated. That will immediately add the neccesary variables to our 
## dataset
source("US/estimateMP.R")

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
  Y_trend <- matrix(0, T, N)
  Y_trend[, 1] <- 4 * init_trend
  
  # Compute the trend for other columns
  for (n in 2:N) {
    for (t in 1:(T - N)) {
      Y_trend[t, n] <- Y_trend[t, n-1] + Y_trend[t + 1, 1]
    }
  }
  
  # Divide by the maturity
  for (n in 1:N) {
    Y_trend[, n] <- Y_trend[, n] / n
  }
  
  return(Y_trend)
}


## -----------------------------------------------------------------------------
current_time_window <- "1980-03-01/2023-06-01"
time_window <- "1980-03-01/2038-06-01"

## -----------------------------------------------------------------------------
## Load original data: 
## This is the first step. We load the data from our database, we select the 
## appropriate time window, and we build two subdatasets, one with the yields
## and another one with the prices.
load("US/data/main.rda")
data.ts <- data.ts[time_window]
dates   <- index(data.ts)
current_dates <- index(data.ts[current_time_window])
T <- length(dates)


# Notice that we do not have all the maturities between 1 and 60 (60 quarters is
# our last maturity), i.e., there are some of them missing. That's why we need 
# to build a list with the maturities that are present in our dataset. It will 
# be useful later on when defining the log prices.
maturities.cols <- grep("^(y|yNSS)([1-9][0-9]{0,2}|1000|0)$", colnames(data.ts))
maturities.all  <- colnames(data.ts)[maturities.cols]
maturities.list <- gsub("NSS", "", maturities.all)
maturities.list <- as.numeric(gsub("y", "", maturities.list))
N <- max(maturities.list)


# Select the sub-dataset containing ONLY the yields
y.ts           <- data.ts[, maturities.all]
colnames(y.ts) <- gsub("NSS","",colnames(y.ts))      
colnames(y.ts) <- gsub("y","n",colnames(y.ts))                                  # Change the name of the columns so that the format of the name of the columns
# is yq where q is the number of quarters and not yNSSq
load("US/data/raw/yields_nss.rda")                                                 # Now load the equivalent dataset with all yields fitted from NSS formula
yNSS.ts <- yields.ts.NSS[time_window]
rm(yields.ts.NSS)

# That's what I was mentioning before: we need this to construct the log prices:
ttm   <- maturities.list*(1/4)
p.ts  <- -t(t(yNSS.ts)*ttm)
colnames(p.ts) <- colnames(y.ts)

## -----------------------------------------------------------------------------
## FACTORS
## First: Compute the trend matrix:
init_trend <- coredata(data.ts$trend)
Y_trend    <- computeTrendMatrix(init_trend, T, N)

y_trend.ts           <- xts(Y_trend, order.by=dates)
colnames(y_trend.ts) <- paste("n",seq(1,N),sep = "")
y_cycle.ts           <- na.omit(y.ts - y_trend.ts)
y_cycle.ts           <- y_cycle.ts[current_time_window]
rm(init_trend, Y_trend)

## -----------------------------------------------------------------------------
## K Principal Components
## Here we start replicating the procedure described in ACM2013, where they use 
## the first 5 principal components. We thus have to take the 5 first PC in our 
## dataset y.ts. Notice that some maturities are missing.
K <- 5
y_cycle.ts.demeaned   <- scale(y_cycle.ts, scale = FALSE)
k                     <- prcomp((y_cycle.ts.demeaned))
y_cycle.df.PC         <- y_cycle.ts.demeaned %*% (k$rotation)                   # Get Principal Components
X.ts                  <- xts(y_cycle.df.PC[,1:K], order.by=current_dates)       # Get Factors (5 first PC)
data.ts               <- merge(data.ts, X.ts, join = 'outer')                   # Merge it to data.ts, which is our main dataset
rm(k, y_cycle.df.PC)

## -----------------------------------------------------------------------------
## STEP 1 - estimate VAR(1) for the time series of pricing factors
## The model is Xt = mu + phi Xt_ + eps
X.var <- VAR(X.ts, p = 1, type = "const") 

X.var.coef  <- coef(X.var)                                                      # Get all the coefficients
X.var.names <- names(X.var.coef)

Phi <- matrix(0, nrow = K, ncol = K)
mu  <- matrix(0, nrow = K, ncol = 1) 

# Populate matrices
for (i in 1:K) {
  X.var.name       <- X.var.names[i]
  X.var.estimates  <- X.var.coef[[X.var.name]] 
  Phi[i,]          <- X.var.estimates[1:K, 1]
  mu[i,1]          <- X.var.estimates[K+1, 1]
}
rm(X.var.name, X.var.estimates, X.var.coef)

X.var.v           <- residuals(X.var)                                           # Get the residuals
colnames(X.var.v) <- paste("v",colnames(X.var.v), sep = "_")
dates_lag         <- seq(as.Date("1980-06-01"),length=length(current_dates)-1, by="quarters")
v.ts              <- xts(X.var.v, order.by=dates_lag)
Sigma             <- t(v.ts)%*%v.ts/nrow(v.ts)                                  # Get the variance-covariance matrix
params.X          <- list(Phi=Phi, mu=mu, Sigma=Sigma)
rm(dates_lag, X.var.v)

data.ts <- merge(data.ts, v.ts, join = 'outer')                                 # Merge residuals into out main xts dataset, data.ts

## -----------------------------------------------------------------------------
## STEP 2 - regress log excess returns,this needs the package systemfit

# Include lags of factors
PC_names <- paste0("PC", 1:K)
PC_list  <- lapply(PC_names, function(name) data.ts[,name])
data.df  <- data.frame(data.ts)
for (pc in 1:K) {data.df[,paste0("PC", pc, "_lag")] <- lag.xts(PC_list[[pc]])}
data.ts  <- xts(data.df, order.by = dates)
rm(data.df)

# Equations:
eqhper_4r  <- hper4  ~  PC1_lag + PC2_lag + PC3_lag + PC4_lag + PC5_lag
eqhper_8r  <- hper8  ~  PC1_lag + PC2_lag + PC3_lag + PC4_lag + PC5_lag
eqhper_12r <- hper12 ~  PC1_lag + PC2_lag + PC3_lag + PC4_lag + PC5_lag
eqhper_16r <- hper16 ~  PC1_lag + PC2_lag + PC3_lag + PC4_lag + PC5_lag
eqhper_20r <- hper20 ~  PC1_lag + PC2_lag + PC3_lag + PC4_lag + PC5_lag
eqhper_28r <- hper28 ~  PC1_lag + PC2_lag + PC3_lag + PC4_lag + PC5_lag
eqhper_40r <- hper40 ~  PC1_lag + PC2_lag + PC3_lag + PC4_lag + PC5_lag
eqhper_60r <- hper60 ~  PC1_lag + PC2_lag + PC3_lag + PC4_lag + PC5_lag

system_hper_r <- list(eqhper_4r, eqhper_8r, eqhper_12r, eqhper_16r, eqhper_20r,
                      eqhper_28r, eqhper_40r,eqhper_60r)

eqhper_4  <- hper4  ~ v_PC1 + v_PC2 + v_PC3 + v_PC4 + v_PC5 + PC1_lag + PC2_lag + PC3_lag + PC4_lag + PC5_lag
eqhper_8  <- hper8  ~ v_PC1 + v_PC2 + v_PC3 + v_PC4 + v_PC5 + PC1_lag + PC2_lag + PC3_lag + PC4_lag + PC5_lag
eqhper_12 <- hper12 ~ v_PC1 + v_PC2 + v_PC3 + v_PC4 + v_PC5 + PC1_lag + PC2_lag + PC3_lag + PC4_lag + PC5_lag
eqhper_16 <- hper16 ~ v_PC1 + v_PC2 + v_PC3 + v_PC4 + v_PC5 + PC1_lag + PC2_lag + PC3_lag + PC4_lag + PC5_lag
eqhper_20 <- hper20 ~ v_PC1 + v_PC2 + v_PC3 + v_PC4 + v_PC5 + PC1_lag + PC2_lag + PC3_lag + PC4_lag + PC5_lag
eqhper_28 <- hper28 ~ v_PC1 + v_PC2 + v_PC3 + v_PC4 + v_PC5 + PC1_lag + PC2_lag + PC3_lag + PC4_lag + PC5_lag
eqhper_40 <- hper40 ~ v_PC1 + v_PC2 + v_PC3 + v_PC4 + v_PC5 + PC1_lag + PC2_lag + PC3_lag + PC4_lag + PC5_lag
eqhper_60 <- hper60 ~ v_PC1 + v_PC2 + v_PC3 + v_PC4 + v_PC5 + PC1_lag + PC2_lag + PC3_lag + PC4_lag + PC5_lag

system_hper_unr <- list(eqhper_4, eqhper_8, eqhper_12, eqhper_16, eqhper_20,
                        eqhper_28, eqhper_40, eqhper_60)

# Estimate:
system_hper_r_fit <- systemfit( system_hper_r, "OLS", data = data.ts[current_time_window] )
#summary(system_hper_r_fit)

system_hper_unr_fit <- systemfit( system_hper_unr, "OLS", data = data.ts[current_time_window] )
#summary(system_hper_unr_fit)

coeffmat_sys <- coef(system_hper_unr_fit)
res_sys      <- residuals(system_hper_unr_fit)
res_sys      <- res_sys[-1,] #first row contains NAs because of the lag 
E            <- as.matrix(res_sys)

sigma2 = tr(t(E) %*% E)/(nrow(E)*ncol(E))

m1=matrix(coeffmat_sys,nrow=8,ncol=11,byrow=TRUE)

a     <- m1[,1]
b     <- t(m1[,2:(K+1)])
c     <- t(m1[,(K+2):ncol(m1)])
rm(m1, res_sys, coeffmat_sys)

## -----------------------------------------------------------------------------
## STEP 3 - Run cross-sectional regressions

BStar   <- t(apply(t(b),1,quad_form))
lambda0 <- Ginv(t(b)) %*% (a + 0.5*((BStar %*% c(Sigma)) + sigma2))
lambda1 <- Ginv(t(b)) %*% t(c)
lambda  <- list(lambda0=lambda0, lambda1=lambda1)
rm(BStar)


## -----------------------------------------------------------------------------
## STEP 4 - Get the coefficients of the Affine Term Structure Model

y_cycle.ts$rf <- y_cycle.ts$n1/4
mod_rf     <- lm(y_cycle.ts$rf ~ X.ts)
delta0     <- coef(mod_rf)[1]
delta1     <- coef(mod_rf)[-1]
delta      <- list(delta0=delta0, delta1=delta1)
#summary(mod_rf)
rm(mod_rf)

params <- list(delta=delta, X=params.X, sigma2=sigma2, lambda=lambda)
TScoeffs <- getAffineTScoeffs(params, N, K)
A        <- TScoeffs$A
B        <- TScoeffs$B

#Recursions for Risk Free Yields
TScoeffs  <- getAffineTScoeffs(params, N, K, "rf")
A_rf      <- TScoeffs$A
B_rf      <- TScoeffs$B

rm(TScoeffs)


## -----------------------------------------------------------------------------
## STEP 5 - Get the fitted values

p.cols           <- colnames(p.ts)
#FITTED YIELDS
p_cycle.ts.fit <- getFittedPrice(A,B,X.ts,current_dates,colnames(p.ts))
y_cycle.ts.fit <- xts(- t(t(p_cycle.ts.fit) / ttm), order.by = index(p_cycle.ts.fit))

#RISK NEUTRAL YIELDS
p_cycle_rf.ts.fit <- getFittedPrice(A_rf,B_rf,X.ts,current_dates,colnames(p.ts))
y_cycle_rf.ts.fit <- xts(- t(t(p_cycle_rf.ts.fit) / ttm), order.by=index(p_cycle_rf.ts.fit))

#TERM PREMIA
termpremia = y_cycle.ts.fit - y_cycle_rf.ts.fit
termpremia.ts <- xts(termpremia, order.by=current_dates)
#plot(termpremia.ts[,40], main=NULL)
#title(main="Term premium", ylab="")

# TREND + CYCLE
y.ts.fit <- y_trend.ts + y_cycle.ts.fit


rm(p.cols)
