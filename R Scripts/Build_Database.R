#Function to buid a database for a single country
build_df <- function(long_yield, short_yield = Y_st){
  #Merge 10 year bond yield and short term rate
  df <- merge(long_yield,short_yield, by = "Date")
  df <- df %>% select(Date, Yield, Rate) %>% filter(Date >= "1999-01-01")
  
  #Compute the spreads (long bond yield - short term rate)
  df$Spread <- df$Yield - df$Rate

  #FOR EXPECTED RATES MODEL
  #Compute the first difference of short-term rates (dy_t+1)
  #Time period taken = dt
  dt = 3
  df$l1 <- df$Rate - lag(df$Rate,dt)
  df$l2 <- lag(df$Rate,dt) - lag(df$Rate,2*dt)
  
  
  #FOR CONSENSUS FORECASTS
  df_list <- list(df,f_3,f_6,f_9,f_12)
  df <- df_list %>% reduce(full_join, by="Date") %>%
    select(Date, Yield, Rate, Spread, l1, l2, L1_forecast, L2_forecast, L3_forecast, L4_forecast) %>%
    complete(Date = seq.Date(min(Date), max(Date), by="month"))
  
  #We need to use leads to put the forecasted target period to the time the forecast was made
  df$L1_forecast <- lead(df$L1_forecast,3)
  df$L2_forecast <- lead(df$L2_forecast,6)
  df$L3_forecast <- lead(df$L3_forecast,9)
  df$L4_forecast <- lead(df$L4_forecast,12)
  return(df)
}


