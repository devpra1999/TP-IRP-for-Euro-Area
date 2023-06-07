#CALCULATING TERM PREMIA USING CONSENSUS FORECASTS
T <- 40
df$TP_cf <- NA
df$sum_Er_cf <- (1 - 1/T)*(df$L1_forecast- df$Rate) + (1 - 2/T)*(df$L2_forecast- df$L1_forecast) +
  (1 - 3/T)*(df$L3_forecast- df$L2_forecast) + (1 - 4/T)*(df$L4_forecast- df$L3_forecast)

df$TP_cf <- df$Spread - df$sum_Er_cf

#Final dataframe - reordered for better visual analysis
df <- df %>%
  select(Date, Yield, Rate, Spread, TP, TP_cf, sum_Er, sum_Er_cf, l1, l2, L1_forecast, L2_forecast, L3_forecast, L4_forecast)






