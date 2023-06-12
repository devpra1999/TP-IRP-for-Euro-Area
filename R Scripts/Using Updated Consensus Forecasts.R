#Remove NAs and start dates from 2010
df <- df %>% filter(!is.na(Yield)) %>% filter(Date >= "2010-04-01")

#Interpolation for annual to quarterly data
i <- grep("L4_forecast", colnames(df))
j <- grep("L12_forecast", colnames(df))
temp <- as.data.frame(t(na.spline(t(df[,i:j]))))
colnames(temp) <- colnames(df[i:j])
df <- cbind(df[,1:i-1],temp)

#CALCULATING TERM PREMIA USING CONSENSUS FORECASTS
T <- 40
df$TP_cf <- NA
df$sum_Er_cf <- (1 - 1/T)*(df$L1_forecast - df$Rate) +
  (1 - 2/T)*(df$L2_forecast - df$L1_forecast) +
  (1 - 3/T)*(df$L3_forecast - df$L2_forecast) +
  (1 - 4/T)*(df$L4_forecast - df$L3_forecast) +
  (1 - 5/T)*(df$L5_forecast - df$L4_forecast) +
  (1 - 6/T)*(df$L6_forecast - df$L5_forecast) +
  (1 - 7/T)*(df$L7_forecast - df$L6_forecast) +
  (1 - 8/T)*(df$L8_forecast - df$L7_forecast) +
  (1 - 9/T)*(df$L9_forecast - df$L8_forecast) +
  (1 - 10/T)*(df$L10_forecast - df$L9_forecast) +
  (1 - 11/T)*(df$L11_forecast - df$L10_forecast) +
  (1 - 12/T)*(df$L12_forecast - df$L11_forecast)

df$TP_cf <- df$Spread - df$sum_Er_cf

#Final dataframe - reordered for better visual analysis
df <- df %>%
  select(Date, Yield, Rate, Spread, TP, TP_cf, sum_Er, sum_Er_cf, l1, l2, L1_forecast,
         L2_forecast, L3_forecast, L4_forecast, L5_forecast, L6_forecast, L7_forecast,
         L8_forecast, L9_forecast, L10_forecast, L11_forecast, L12_forecast)


