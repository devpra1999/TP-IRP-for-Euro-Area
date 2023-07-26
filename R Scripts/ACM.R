library(psych)
library(matrixcalc)
library(matlib)
library(pracma)
library(zoo)
library(lubridate)
library(dplyr)

n_maturities = 120
K=5

#file = read.csv("Data_Files/gnss.csv", header=TRUE)
#attach(file)
#yield = matrix(rep(0,nrow(file)*120),nrow(file),120)
#for(n in 1:n_maturities){ 
#  k =n/12
#  yield[,n]= BETA0 +BETA1*(1-exp(-k/TAU1))/(k/TAU1)+ BETA2*((1 -exp(-k/TAU1))/(k/TAU1) - exp(-k/TAU1))+ BETA3*((1-exp(-k/TAU2))/(k/TAU2)-exp(-k/TAU2))
#}
#yield = yield/100
#yield_raw=as.data.frame(yield)
#View(yield_raw)
#yield_raw[,121]= (file$ï..Date)


#isMax <- function(x) seq_along(x) == which.max(as.Date(x))
#yield_m =subset(yield_raw, as.logical(ave(yield_raw[,121], substr(yield_raw[,121], 4, 10), FUN = isMax)))

L <- nss_yields("Data_Files/gnss.xlsx",120)
yield_m <- L$rawYields/100
yield_m[,121] <- L$plot_dates


# Use excess returns at these maturities to estimate the model.
rx_maturities = c(6,18,24,36,48,60,84,120) 
ttm = c(1:120)*(1/12)
logPrices = matrix(rep(0,nrow(yield_m)*120),nrow(yield_m),120)
for(i in 1 :n)
{
  logPrices[,i] = -1*yield_m[,i]*ttm[i]
}
rf =  -1*logPrices[(1:nrow(logPrices)-1),1]
rx =  logPrices[2:nrow(logPrices), 1:(ncol(logPrices)-1)] - logPrices[1:(nrow(logPrices)-1), 2:(ncol(logPrices))] -rf

# Principal component 
K=5
yield =yield_m[,1:120]
demeanedyield = matrix(rep(0,nrow(yield)*ncol(yield)),nrow(yield),ncol(yield))
for(i in 1:ncol(yield))
{
  demeanedyield[,i]= yield[,i]-mean(yield[,i])
}  
k = prcomp((demeanedyield))
yieldPCs = demeanedyield %*% (k$rotation)
x = t(yieldPCs[1:nrow(yieldPCs),1:K])

x_lhs=  x[,(2:ncol(x))] 
x_rhs =  x[,(1:(ncol(x)-1))]
var_coeffs = x_lhs %*% t(x_rhs)%*%solve(x_rhs%*%t(x_rhs))
phi = var_coeffs[,1:ncol(var_coeffs)]
v = x_lhs - var_coeffs %*% x_rhs
Sigma =  v %*% t(v)/ncol(v)

# Step 2
selected_rx = t(rx[,(rx_maturities-1)])
N = nrow(selected_rx)
Z = rbind((rep(1,(nrow(yield)-1))),v,x[,1:(ncol(x)-1)])
abc = selected_rx %*% t(Z)%*%solve(Z%*%t(Z))
E = selected_rx - abc %*% Z

sigmasq_ret = tr(E %*% t(E)) / (ncol(E)*nrow(E))
h =  x[, 1:(ncol(x)-1)]
a = abc[,1]
beta = t(abc[, 1:K+1])
c = abc[, (K+2): ncol(abc)]
l = K*K
# Step(3)
Bstar = t(matrix(rep(0,N*l),N,l))
for(i in 1:ncol(Bstar))
{
  Bstar[,i]= vec(beta[,i]%*%t(beta[,i]))
}  

lambda0 = solve(beta%*%t(beta))%*%beta%*%(a +0.5*(t(Bstar) %*% vec(Sigma) + sigmasq_ret * rep(1,N)))
lambda1  = solve(beta%*%t(beta))%*% beta%*%c

# Run Bond Pricing recursion
A = rep(0,n_maturities)
B = matrix(rep(0,n_maturities),K,n_maturities)
mat =rbind(rep(1,ncol(v)),x[,2:(ncol(x))])
delta = t(rf)%*%t(mat) %*% solve(mat%*%t(mat))
delta0 = delta[1]
delta1 = delta[,2:ncol(delta)]
A[1]=   0.5*(sigmasq_ret)-delta0
B[,1] = -t(delta1) 
for(i in 2:(n_maturities))
{
  A[i] = A[i-1]+ t(B[,i-1])%*%(-lambda0) +0.5*(t(B[,i-1])%*% Sigma %*% B[,i-1]+ sigmasq_ret)-delta0
  B[,i] = t(B[,i-1])%*%(phi-lambda1)-t(delta1)
}

# Construct fitted yields
fittedLogPrices = t(t(B)%*%x) 
fittedYields = t(t(B)%*%x) 
for(i in 1: 120)
  fittedLogPrices[,i] = fittedLogPrices[,i] +A[i]
for(i in 1: 120)
  fittedYields[,i] =  t((fittedLogPrices[,i]/ttm[i])*(-1))

#Construct risk free yields
A = rep(0,(n_maturities))
B = matrix(rep(0,(n_maturities)),K,(n_maturities))
A[1]= 0.5*(sigmasq_ret)-delta0
B[,1] = -t(delta1)

for(i in 2:(n_maturities))
{
  A[i] = A[i-1]+0.5*(t(B[,i-1])%*% Sigma %*% B[,i-1]+ sigmasq_ret)-delta0
  B[,i] = t(B[,i-1])%*%(phi)-t(delta1)
}

# Construct risk free yields
RiskfreeLogPrices = t(t(B)%*%x) 
RiskfreeYields = t(t(B)%*%x) 
for(i in 1: 120)
  RiskfreeLogPrices[,i] = RiskfreeLogPrices[,i] +A[i]
for(i in 1: 120)
  RiskfreeYields[,i] =  t((RiskfreeLogPrices[,i]/ttm[i])*(-1))

termpremia = fittedYields - RiskfreeYields
win.graph()
plot(L$plot_dates,termpremia[,120]*100, type='l' , col="red",ylab="10 years Term Premium", xlab="dates") 


win.graph()
plot(L$plot_dates,termpremia[,120]*100, type='l' , col="red",ylab="10 years Term Premium", xlab="dates",ylim = c(-2,6)) 
lines(L$plot_dates,RiskfreeYields[,120]*100, type='l' , col="blue")
lines(L$plot_dates,yield_m[,120]*100, type='l' , col="black")
legend("topright",c("10Y Yield","E. Monetary Policy","Term Premia"),col = c("black","blue","red"),lwd = c(1,1,1))

s1 <- cbind(as.Date(L$plot_dates),fittedYields[,120]*100)
names(s1) <- c("x","y")
s2 <- cbind(as.Date(L$plot_dates),RiskfreeYields[,120]*100)
names(s2) <- c("x","y")
s3 <- cbind(as.Date(L$plot_dates),termpremia[,120]*100)
names(s3) <- c("x","y")
s4 <- cbind(as.Date(L$plot_dates),rep(0,length(L$plot_dates)))
names(s4) <- c("x","y")
Germany_Term_Premia_ACM <- highchart() %>%
  hc_add_series(s1, "line", hcaes(x, y), name = "Yield", color = "black") %>%
  hc_add_series(s2, "line", hcaes(x, y),
                name = "Consensus Term Premia", color = "blue",connectNulls = TRUE, dashStyle = "dash",
                marker = list(symbol = "circle", lineWidth = 0, radius = 2)) %>%
  hc_add_series(s3, "line", hcaes(x, y),
                name = "Monetary Policy Component", dashStyle = "dash", color = "red",
                marker = list(symbol = "circle", lineWidth = 0, radius = 2), connectNulls = TRUE) %>%
  hc_add_series(s4, "line", hcaes(x, y),
                name = "", dashStyle = "dot", color = "black",
                showInLegend = FALSE) %>%
  hc_title(text = "Germany") %>%
  hc_xAxis(type = "datetime", title = list(text = "Date")) %>%
  hc_yAxis(title = list(text = "Yield & Term Premia"), min = -2, max = 7) %>%
  hc_legend(enabled = TRUE) %>%
  hc_boost(enabled = TRUE) %>%
  hc_exporting(enabled = TRUE)