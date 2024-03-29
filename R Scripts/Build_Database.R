#Function to buid a database for a single country
build_df <- function(long_yield, rate = Y_st){
  #Merge 10 year bond yield and short term rate
  df <- merge(long_yield,rate, by = "Date")
  df <- df %>% select(Date, Yield, Rate) %>% filter(Date >= "2000-01-01")
  Rate_df <- rate %>% select(Date,Rate) %>% filter(Date >= "2000-01-01")
  
  #Compute the spreads (long bond yield - short term rate)
  df$Spread <- df$Yield - df$Rate
  
  #FOR EXCESS RETURNS
  #Compute 3-month returns for 10-Y bond
  ly_10_m <- df$Yield/(100*12)
  dur <- (1-(1+mean(df$Yield)/100)^(-10))/(1-(1+mean(df$Yield)/100)^(-1))
  lret_b_10_m <- (dur*ly_10_m-(dur-1/12)*dplyr::lead(ly_10_m))
  df$Excess_Return <- lret_b_10_m*100*12 - df$Rate
  
  df$HPR <- (10*df$Yield - 9.75*dplyr::lead(df$Yield))*4 - df$Rate
  
  #FOR EXPECTED RATES MODEL
  #Compute the first difference of short-term rates (dy_t+1)
  #Time period taken = dt
  dt = 3
  Rate_df$FD_Rate <- Rate_df$Rate - dplyr::lag(Rate_df$Rate,dt)
  #df$l2 <- lag(df$Rate,dt) - lag(df$Rate,2*dt)
  
  
  #FOR CONSENSUS FORECASTS
  df_list <- list(df, f_3, f_6, f_9, f_12, f_15, f_18, f_21, f_24, f_27, f_30, f_33, f_36)
  
  df <- df_list %>%
    reduce(full_join, by = "Date") %>%
    select(Date, Yield, Rate, Spread, Excess_Return, HPR, Q1_forecast, Q2_forecast, Q3_forecast, Q4_forecast,
           Q5_forecast, Q6_forecast, Q7_forecast, Q8_forecast, Q9_forecast, Q10_forecast,
           Q11_forecast, Q12_forecast) %>%
    tidyr::complete(Date = seq.Date(min(Date), max(Date), by = "month"))
  
  
  #We need to use leads to put the forecasted target period to the time the forecast was made
  df$Q1_forecast <- dplyr::lead(df$Q1_forecast, 3)
  df$Q2_forecast <- dplyr::lead(df$Q2_forecast, 6)
  df$Q3_forecast <- dplyr::lead(df$Q3_forecast, 9)
  df$Q4_forecast <- dplyr::lead(df$Q4_forecast, 12)
  df$Q5_forecast <- dplyr::lead(df$Q5_forecast, 15)
  df$Q6_forecast <- dplyr::lead(df$Q6_forecast, 18)
  df$Q7_forecast <- dplyr::lead(df$Q7_forecast, 21)
  df$Q8_forecast <- dplyr::lead(df$Q8_forecast, 24)
  df$Q9_forecast <- dplyr::lead(df$Q9_forecast, 27)
  df$Q10_forecast <- dplyr::lead(df$Q10_forecast, 30)
  df$Q11_forecast <- dplyr::lead(df$Q11_forecast, 33)
  df$Q12_forecast <- dplyr::lead(df$Q12_forecast, 36)
  
  return(list("main_data" = df,"rate_data" = Rate_df))
}


