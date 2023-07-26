#LONG TERM BOND DATA----------------------------------------------------------

#Getting the data for German 10 year bonds
Y_G_10 <- read.csv("Data_Files/Germany-long-bond.csv",
                   skip = 6,col.names = c("Date","Yield","Type"), header = FALSE)
Y_G_10$Date <- ym(Y_G_10$Date)
Y_G_10 <- Y_G_10[order(Y_G_10$Date),]


#Getting the data for French 10 year bonds
Y_F_10 <- read.csv("Data_Files/France-long-bond.csv",
                   skip = 6,col.names = c("Date","Yield","Type"), header = FALSE)
Y_F_10$Date <- ym(Y_F_10$Date)
Y_F_10 <- Y_F_10[order(Y_F_10$Date),]


#Getting the data for Italian 10 year bonds
Y_I_10 <- read.csv("Data_Files/Italy-long-bond.csv",
                   skip = 6,col.names = c("Date","Yield","Type"), header = FALSE)
Y_I_10$Date <- ym(Y_I_10$Date)
Y_I_10 <- Y_I_10[order(Y_I_10$Date),]


#Getting the data for Spanish 10 year bonds
Y_S_10 <- read.csv("Data_Files/Spain-long-bond.csv",
                   skip = 6,col.names = c("Date","Yield","Type"), header = FALSE)
Y_S_10$Date <- ym(Y_S_10$Date)
Y_S_10 <- Y_S_10[order(Y_S_10$Date),]



#SHORT TERM BOND DATA-----------------------------------------------------------

#Getting data for short-term rates

#Market rates
Y_st <- read.csv("Data_Files/Short-term-rates.csv")
Y_st <- Y_st %>% filter(int_rt == "IRT_M3" & geo == "EA") %>% select(TIME_PERIOD,OBS_VALUE)
colnames(Y_st) <- c("Date","Rate")
Y_st$Date <- ym(Y_st$Date)
Y_st <- Y_st[order(Y_st$Date),]

#Official rates
#Y_st <- read.csv("./Data_Files/ECB-rates.csv", skip = 6
#                 , col.names = c("Date","Rate","Type"))
#Y_st$Date <- ymd(Y_st$Date)


#CONSENSUS FORECASTS DATA--------------------------------------------------------

#The following file contains the forecasts where target period ends 3 months after the survey
#The value for a given quarter refers to the forecast made 3 months (1 quarter) before
f_3 <- read.csv("Data_Files/3-months.csv", skip = 6,
                col.names = c("Date","Q1_forecast","Type"), header = FALSE)
#Order by date and convert quarterly data into date format
f_3 <- f_3[order(f_3$Date),]
f_3$Date = as.Date(as.yearqtr(f_3$Date, format = "%YQ%q")) + months(3)

#Repeat for 6,9 and 12 month ahead forecasts
f_6 <- read.csv("Data_Files/6-months.csv", skip = 6,col.names = c("Date","Q2_forecast","Type"), header = FALSE)
f_6 <- f_6[order(f_6$Date),]
f_6$Date = as.Date(as.yearqtr(f_6$Date, format = "%YQ%q")) + months(3)

f_9 <- read.csv("Data_Files/9-months.csv", skip = 6,col.names = c("Date","Q3_forecast","Type"), header = FALSE)
f_9 <- f_9[order(f_9$Date),]
f_9$Date = as.Date(as.yearqtr(f_9$Date, format = "%YQ%q")) + months(3)

f_12 <- read.csv("Data_Files/12-months.csv", skip = 6, header = FALSE)
colnames(f_12)[1:3] = c("Date","Q4_forecast","Type")
f_12 <- f_12[order(f_12$Date),]
f_12$Date = as.Date(as.yearqtr(f_12$Date, format = "%YQ%q")) + months(3)


#ANNUAL DATA NOW
# Read and process 15-months.csv
f_15 <- read.csv("Data_Files/15-months.csv", skip = 6, col.names = c("Date", "Q5_forecast", "Type"), header = FALSE)
f_15 <- f_15[order(f_15$Date),]
f_15$Date <- as.Date(as.yearqtr(f_15$Date, format = "%YQ%q")) + months(12)

# Read and process 18-months.csv
f_18 <- read.csv("Data_Files/18-months.csv", skip = 6, col.names = c("Date", "Q6_forecast", "Type"), header = FALSE)
f_18 <- f_18[order(f_18$Date),]
f_18$Date <- as.Date(as.yearqtr(f_18$Date, format = "%YQ%q")) + months(12)

# Read and process 21-months.csv
f_21 <- read.csv("Data_Files/21-months.csv", skip = 6, col.names = c("Date", "Q7_forecast", "Type"), header = FALSE)
f_21 <- f_21[order(f_21$Date),]
f_21$Date <- as.Date(as.yearqtr(f_21$Date, format = "%YQ%q")) + months(12)

# Read and process 24-months.csv
f_24 <- read.csv("Data_Files/24-months.csv", skip = 6, col.names = c("Date", "Q8_forecast", "Type"), header = FALSE)
f_24 <- f_24[order(f_24$Date),]
f_24$Date <- as.Date(as.yearqtr(f_24$Date, format = "%YQ%q")) + months(12)

# Continue this pattern for 27, 30, 33, and 36 months
f_27 <- read.csv("Data_Files/27-months.csv", skip = 6, col.names = c("Date", "Q9_forecast", "Type"), header = FALSE)
f_27 <- f_27[order(f_27$Date),]
f_27$Date <- as.Date(as.yearqtr(f_27$Date, format = "%YQ%q")) + months(12)

f_30 <- read.csv("Data_Files/30-months.csv", skip = 6, col.names = c("Date", "Q10_forecast", "Type"), header = FALSE)
f_30 <- f_30[order(f_30$Date),]
f_30$Date <- as.Date(as.yearqtr(f_30$Date, format = "%YQ%q")) + months(12)

f_33 <- read.csv("Data_Files/33-months.csv", skip = 6, col.names = c("Date", "Q11_forecast", "Type"), header = FALSE)
f_33 <- f_33[order(f_33$Date),]
f_33$Date <- as.Date(as.yearqtr(f_33$Date, format = "%YQ%q")) + months(12)

f_36 <- read.csv("Data_Files/36-months.csv", skip = 6, col.names = c("Date", "Q12_forecast", "Type"), header = FALSE)
f_36 <- f_36[order(f_36$Date),]
f_36$Date <- as.Date(as.yearqtr(f_36$Date, format = "%YQ%q")) + months(12)



#MACROECONOMIC DATA------------------------------------------------------------------------
#GDP
gdp_dat <- read.csv("Data_Files/gdp_data.csv")
gdp_dat <- gdp_dat %>% select(geo,TIME_PERIOD,OBS_VALUE)
colnames(gdp_dat) <- c("Country","Date","GDP")
gdp_dat$Date <- as.Date(as.yearqtr(gdp_dat$Date, format = "%Y-Q%q")) + months(3)
gdp_dat <- gdp_dat[order(gdp_dat$Date),]

Germany_ga <- read.csv("Data_Files/Germany-real-growth-yy.csv", skip = 6, header = FALSE)
colnames(Germany_ga)[1:2] <- c("Date","Growth_a")
Germany_ga <- Germany_ga %>%
  select(Date,Growth_a) %>%
  mutate(Date = as.Date(as.yearqtr(Date, format = "%YQ%q")) + months(3), Growth_a = as.numeric(Growth_a)) %>%
  filter(Date >= "2010-04-01") %>%
  arrange(Date)

France_ga <- read.csv("Data_Files/France-real-growth-yy.csv", skip = 6, header = FALSE)
colnames(France_ga)[1:2] <- c("Date", "Growth_a")
France_ga <- France_ga %>%
  select(Date, Growth_a) %>%
  mutate(Date = as.Date(as.yearqtr(Date, format = "%YQ%q")) + months(3), Growth_a = as.numeric(Growth_a)) %>%
  filter(Date >= "2010-04-01") %>%
  arrange(Date)

Italy_ga <- read.csv("Data_Files/Italy-real-growth-yy.csv", skip = 6, header = FALSE)
colnames(Italy_ga)[1:2] <- c("Date", "Growth_a")
Italy_ga <- Italy_ga %>%
  select(Date, Growth_a) %>%
  mutate(Date = as.Date(as.yearqtr(Date, format = "%YQ%q")) + months(3), Growth_a = as.numeric(Growth_a)) %>%
  filter(Date >= "2010-04-01") %>%
  arrange(Date)

Spain_ga <- read.csv("Data_Files/Spain-real-growth-yy.csv", skip = 6, header = FALSE)
colnames(Spain_ga)[1:2] <- c("Date", "Growth_a")
Spain_ga <- Spain_ga %>%
  select(Date, Growth_a) %>%
  mutate(Date = as.Date(as.yearqtr(Date, format = "%YQ%q")) + months(3), Growth_a = as.numeric(Growth_a)) %>%
  filter(Date >= "2010-04-01") %>%
  arrange(Date)

Germany_gq <- read.csv("Data_Files/Germany-real-growth-qq.csv", skip = 6, header = FALSE)
colnames(Germany_gq)[1:2] <- c("Date","Growth_q")
Germany_gq <- Germany_gq %>%
  select(Date,Growth_q) %>%
  mutate(Date = as.Date(as.yearqtr(Date, format = "%YQ%q")) + months(3), Growth_q = as.numeric(Growth_q)) %>%
  filter(Date >= "2010-04-01") %>%
  arrange(Date)

France_gq <- read.csv("Data_Files/France-real-growth-qq.csv", skip = 6, header = FALSE)
colnames(France_gq)[1:2] <- c("Date", "Growth_q")
France_gq <- France_gq %>%
  select(Date, Growth_q) %>%
  mutate(Date = as.Date(as.yearqtr(Date, format = "%YQ%q")) + months(3), Growth_q = as.numeric(Growth_q)) %>%
  filter(Date >= "2010-04-01") %>%
  arrange(Date)

Italy_gq <- read.csv("Data_Files/Italy-real-growth-qq.csv", skip = 6, header = FALSE)
colnames(Italy_gq)[1:2] <- c("Date", "Growth_q")
Italy_gq <- Italy_gq %>%
  select(Date, Growth_q) %>%
  mutate(Date = as.Date(as.yearqtr(Date, format = "%YQ%q")) + months(3), Growth_q = as.numeric(Growth_q)) %>%
  filter(Date >= "2010-04-01") %>%
  arrange(Date)

Spain_gq <- read.csv("Data_Files/Spain-real-growth-qq.csv", skip = 6, header = FALSE)
colnames(Spain_gq)[1:2] <- c("Date", "Growth_q")
Spain_gq <- Spain_gq %>%
  select(Date, Growth_q) %>%
  mutate(Date = as.Date(as.yearqtr(Date, format = "%YQ%q")) + months(3), Growth_q = as.numeric(Growth_q)) %>%
  filter(Date >= "2010-04-01") %>%
  arrange(Date)


#Inflation
Germany_inf <- read.csv("Data_Files/Germany-inflation.csv", skip = 6, header = FALSE)
colnames(Germany_inf)[1:2] <- c("Date","Inflation")
Germany_inf <- Germany_inf %>%
  select(Date,Inflation) %>%
  mutate(Date = ym(Date), Inflation = as.numeric(Inflation)) %>%
  filter(Date >= "2010-04-01") %>%
  arrange(Date)
#Germany_inf$Date <- ym(Germany_inf$Date)
#Germany_inf <- Germany_inf[order(Germany_inf$Date),]

France_inf <- read.csv("Data_Files/France-inflation.csv", skip = 6, header = FALSE)
colnames(France_inf)[1:2] <- c("Date","Inflation")
France_inf <- France_inf %>%
  select(Date,Inflation) %>%
  mutate(Date = ym(Date), Inflation = as.numeric(Inflation)) %>%
  filter(Date >= "2010-04-01") %>%
  arrange(Date)

Italy_inf <- read.csv("Data_Files/Italy-inflation.csv", skip = 6, header = FALSE)
colnames(Italy_inf)[1:2] <- c("Date","Inflation")
Italy_inf <- Italy_inf %>%
  select(Date,Inflation) %>%
  mutate(Date = ym(Date), Inflation = as.numeric(Inflation)) %>%
  filter(Date >= "2010-04-01") %>%
  arrange(Date)

Spain_inf <- read.csv("Data_Files/Spain-inflation.csv", skip = 6, header = FALSE)
colnames(Spain_inf)[1:2] <- c("Date","Inflation")
Spain_inf <- Spain_inf %>%
  select(Date,Inflation) %>%
  mutate(Date = ym(Date), Inflation = as.numeric(Inflation)) %>%
  filter(Date >= "2010-04-01") %>%
  arrange(Date)

Germany_inf_q <- read.csv("Data_Files/Germany-inflation-qq.csv", skip = 6, header = FALSE)
colnames(Germany_inf_q)[1:2] <- c("Date","Inflation_QQ")
Germany_inf_q <- Germany_inf_q %>%
  select(Date,Inflation_QQ) %>%
  mutate(Date = ymd(Date), Inflation_QQ = as.numeric(Inflation_QQ)) %>%
  filter(Date >= "2010-04-01") %>%
  arrange(Date)
#Germany_inf$Date <- ym(Germany_inf$Date)
#Germany_inf <- Germany_inf[order(Germany_inf$Date),]

France_inf_q <- read.csv("Data_Files/France-inflation-qq.csv", skip = 6, header = FALSE)
colnames(France_inf_q)[1:2] <- c("Date","Inflation_QQ")
France_inf_q <- France_inf_q %>%
  select(Date,Inflation_QQ) %>%
  mutate(Date = ymd(Date), Inflation_QQ = as.numeric(Inflation_QQ)) %>%
  filter(Date >= "2010-04-01") %>%
  arrange(Date)

Italy_inf_q <- read.csv("Data_Files/Italy-inflation-qq.csv", skip = 6, header = FALSE)
colnames(Italy_inf_q)[1:2] <- c("Date","Inflation_QQ")
Italy_inf_q <- Italy_inf_q %>%
  select(Date,Inflation_QQ) %>%
  mutate(Date = ymd(Date), Inflation_QQ = as.numeric(Inflation_QQ)) %>%
  filter(Date >= "2010-04-01") %>%
  arrange(Date)

Spain_inf_q <- read.csv("Data_Files/Spain-inflation-qq.csv", skip = 6, header = FALSE)
colnames(Spain_inf_q)[1:2] <- c("Date","Inflation_QQ")
Spain_inf_q <- Spain_inf_q %>%
  select(Date,Inflation_QQ) %>%
  mutate(Date = ymd(Date), Inflation_QQ = as.numeric(Inflation_QQ)) %>%
  filter(Date >= "2010-04-01") %>%
  arrange(Date)

Global_CP <- read.csv("Data_Files/Commodity_Prices.csv")
colnames(Global_CP)[1:2] <- c("Date","CP_Index")
Global_CP <- Global_CP %>%
  select(Date,CP_Index) %>%
  mutate(Date = ymd(Date), CP_Index = as.numeric(CP_Index)) %>%
  filter(Date >= "2010-04-01") %>%
  arrange(Date)

Global_CP_QQ <- read.csv("Data_Files/GCP-QQ.csv")
colnames(Global_CP_QQ)[1:2] <- c("Date","GCP_QQ")
Global_CP_QQ <- Global_CP_QQ %>%
  select(Date,GCP_QQ) %>%
  mutate(Date = ymd(Date), GCP_QQ = as.numeric(GCP_QQ)) %>%
  filter(Date >= "2010-04-01") %>%
  arrange(Date)
Global_CP <- merge(Global_CP, Global_CP_QQ, by = "Date")

Global_CP_YY <- read.csv("Data_Files/GCP-YY.csv")
colnames(Global_CP_YY)[1:2] <- c("Date","GCP_YY")
Global_CP_YY <- Global_CP_YY %>%
  select(Date,GCP_YY) %>%
  mutate(Date = ymd(Date), GCP_YY = as.numeric(GCP_YY)) %>%
  filter(Date >= "2010-04-01") %>%
  arrange(Date)
Global_CP <- merge(Global_CP, Global_CP_YY, by = "Date")

#NSS parameters
Germany_NSS <- read.csv("Data_Files/Germany-NSS-params.csv",skip = 5)
Germany_NSS <- Germany_NSS %>% select_if(~ !any(is.na(.))) 
colnames(Germany_NSS) <- c("Date","Beta0","Beta1","Beta2","Beta3","Tau1","Tau2")
Germany_NSS <- Germany_NSS %>% mutate(Date = ym(Date)) %>% filter(Date >= "2000-01-01") %>% filter(Date <= "2020-01-01")

#Compute Yields
L <- nss_yields("Data_Files/gnss.xlsx",120)
rawYields <- L$rawYields

#LogPrice
ttm <- seq(1,120)/12
logPrices <-  t(t(-rawYields)*ttm)
colnames(logPrices) <- paste("n",seq(1,120),sep = "_")

#Risk-free rate
rf <- -logPrices[1:(nrow(logPrices)-1),1]

#Excess Returns
rx <- logPrices[2:nrow(logPrices),1:119] - logPrices[1:(nrow(logPrices)-1),2:ncol(logPrices)] - rf

#PCA
#K <- 5
#X <- t(prcomp(rawYields, center = TRUE, scale = TRUE, rank. = K)$x)
library("pracma")
scaledYields <- scale(rawYields, scale = TRUE)
scaledYieldCov <- cov(scaledYields)
eigen_result <- eigen(scaledYieldCov)
eigenvalues <- eigen_result$values
eigenvectors <- eigen_result$vectors
yieldPCs <- scale(scaledYields %*% eigenvectors)

K <- 5
X <- yieldPCs[, 1:K]

#Step 1 - estimate VAR(1) for the time series of pricing factors
X_t1 <- X[2:nrow(X), ]  
X_t <- X[1:(nrow(X) - 1),]
mod1 <- lm(X_t1 ~ X_t)
coefficients <- coef(mod1)
mu <- coefficients[1,]
phi <- t(coefficients[-1,])

#v <- X_t1 - X_t %*% phi #residuals
v <- residuals(mod1)
Sigma <- t(v) %*% v / nrow(X_t) #covariance matrix

#Step 2 - regress log excess returns
rx_maturities <- c(6, 18, 24, 36, 48, 60, 84, 120)
selected_rx <- rx[,rx_maturities-1]
VX <- cbind(v,X_t)
mod2 <- lm(rx ~ VX)
coefficients <- coef(mod2)

a <- coefficients[1,]
beta <- coefficients[2:(K+1), ]
c <- coefficients[(K+2):nrow(coefficients),]

E <- mod2$residuals
sigmasq_ret = sum(E^2)/(nrow(E)*ncol(E))

#Step 3 - Run cross-sectional regressions
quad_form <- function(b){
  c(outer(b,b))
}
BStar <- t(apply(t(beta),1,quad_form))
lambda0 <- ginv(t(beta)) %*% (a + 0.5*((BStar %*% c(Sigma)) + sigmasq_ret))
lambda1 <- ginv(t(beta)) %*% t(c)


#Regress risk free on X

mod3 <- lm(rf ~ X_t)
delta0 <- coef(mod3)[1]
delta1 <- coef(mod3)[-1]

#Recursions
A <- matrix(0,1,120)
B <- matrix(0,K,120)
A[1, 1] = - delta0
B[, 1] = - delta1
#lambda0 = matrix(0,5,1)
#lambda1 = matrix(0,5,5)
for (i in 1:119){
  A[1, i+1] = A[1, i] + t(B[, i]) %*% (mu - lambda0) + 0.5 * (t(B[, i]) %*% Sigma %*% B[, i] + sigmasq_ret) - delta0
  B[, i+1] = t(B[, i]) %*% (phi - lambda1) - delta1
}

fittedLogPrices = t(as.vector(t(A)) + t(B) %*% t(X))
fittedYields = - t(t(fittedLogPrices) / ttm)

#Risk Neutral Yields
A_rf <- matrix(0,1,120)
B_rf <- matrix(0,K,120)
A_rf[1, 1] = - delta0
B_rf[, 1] = - delta1
for (i in 1:119){
  A_rf[1, i+1] = A_rf[1, i] + (t(B_rf[, i]) %*% mu) + 0.5*(t(B_rf[, i])%*% Sigma%*%B_rf[, i] + sigmasq_ret) - delta0
  B_rf[, i+1] = t(B_rf[, i]) %*% phi - delta1
}

fittedLogPrices_rf = t(as.vector(t(A_rf)) + (t(B_rf)%*%t(X)))
fittedYields_rf = -t(t(fittedLogPrices_rf)/ttm)

Risk_Neutral_Yield_10Y <- fittedYields_rf[,120]
TP_10Y <- fittedYields[,48] - fittedYields_rf[,120]

plot(L$plot_dates,fittedYields[,120],type="l")
lines(L$plot_dates, rawYields[,120], col = "red")
plot(L$plot_dates,Risk_Neutral_Yield_10Y,col="blue",type="l")
lines(L$plot_dates,TP_10Y,col="red")
