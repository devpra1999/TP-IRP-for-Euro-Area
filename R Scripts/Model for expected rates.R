#We'll start with a simple model dy_t+1 = rho*dy_t + e_t+1
#Or equivalently dy_t = rho*dy_t-1 + e_t

#Estimation of coefficient for the lag (rho)
mod <- lm(df$l1 ~ df$l2 - 1)
rho = mod$coefficients[1]

#How many time periods -> T
T = 10

#An array to store the cumulative sum of expected short rate movements
sum_Er <- rep(0,length(df$l1))

#A variable to store the current rate movement
#Will be used to estimate the future rate movement using our rho parameter
curr <- df$l1

#Loop to find the total weighted expected sum of movements
for (i in 1:T){
  sum_Er <- sum_Er + (1-i/T)*rho*curr
  curr <- rho*curr
}

#Compute the term premia
df$TP <- df$Spread - sum_Er
#Compute the common expected short term rate component of the yield
df$sum_Er <- sum_Er

df <- df %>%
  select(Date, Yield, Rate, Spread, l1, l2, sum_Er, TP, L1_forecast, L2_forecast, L3_forecast, L4_forecast,
         L5_forecast, L6_forecast, L7_forecast, L8_forecast, L9_forecast, L10_forecast,
         L11_forecast, L12_forecast)

