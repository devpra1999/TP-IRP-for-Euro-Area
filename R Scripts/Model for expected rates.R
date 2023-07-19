#We'll start with a simple model dy_t+1 = rho*dy_t + e_t+1
#Or equivalently dy_t = rho*dy_t-1 + e_t

#Estimation of coefficient for the lag (rho)
dt = 3
mod <- lm(Rate_df$FD_Rate ~ dplyr::lag(Rate_df$FD_Rate,dt) - 1)
rho = mod$coefficients[1]

#How many time periods -> T
T = 10

#An array to store the cumulative sum of expected short rate movements
sum_Er <- rep(0,length(df$Rate))

#A variable to store the current rate movement
#Will be used to estimate the future rate movement using our rho parameter
curr <- df$Rate - dplyr::lag(df$Rate,dt)

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
  select(Date, Yield, Rate, Spread, Excess_Return, HPR, sum_Er, TP, Q1_forecast, Q2_forecast, Q3_forecast, Q4_forecast,
         Q5_forecast, Q6_forecast, Q7_forecast, Q8_forecast, Q9_forecast, Q10_forecast,
         Q11_forecast, Q12_forecast)

