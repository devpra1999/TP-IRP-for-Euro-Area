#Function for linear interpolation - standard linear interpolation functions do not work well if there are terminal NA values
linear_interpolation <- function(x){
  if (is.na(x[1])){
    return(x)
  }
  curr <- 4
  for (i in 5:length(x)){
    if (is.na(x[i])){
      if (i!=length(x)){
        next
      }
      else{
        temp <- x[curr]
        for (j in (curr+1):i){
          x[j] <- temp + diff
          temp <- x[j]
        }
      }
    }
    else{
      if ((i-curr)>1){
        diff <- (x[i] - x[curr])/(i-curr)
        temp <- x[curr]
        for (j in (curr+1):(i-1)){
          x[j] <- temp + diff
          temp <- x[j]
        }
      }
      curr <- i
    }
  }
  return(x)
}

#Remove NAs and start dates from 2010
df <- df %>% filter(!is.na(Yield)) %>% filter(Date >= "2010-04-01")

#Interpolation for annual to quarterly data
i <- grep("Q1_forecast", colnames(df))
j <- grep("Q12_forecast", colnames(df))
#temp <- as.data.frame(t(na.spline(t(df[,i:j]))))
temp <- as.data.frame(t(apply(df[,i:j],1,linear_interpolation)))
colnames(temp) <- colnames(df[i:j])
df <- cbind(df[,1:i-1],temp)

#CALCULATING TERM PREMIA USING CONSENSUS FORECASTS
T <- 40
df$TP_cf <- NA
df$sum_Er_cf <- (1 - 1/T)*(df$Q1_forecast - df$Rate) +
  (1 - 2/T)*(df$Q2_forecast - df$Q1_forecast) +
  (1 - 3/T)*(df$Q3_forecast - df$Q2_forecast) +
  (1 - 4/T)*(df$Q4_forecast - df$Q3_forecast) +
  (1 - 5/T)*(df$Q5_forecast - df$Q4_forecast) +
  (1 - 6/T)*(df$Q6_forecast - df$Q5_forecast) +
  (1 - 7/T)*(df$Q7_forecast - df$Q6_forecast) +
  (1 - 8/T)*(df$Q8_forecast - df$Q7_forecast) +
  (1 - 9/T)*(df$Q9_forecast - df$Q8_forecast) +
  (1 - 10/T)*(df$Q10_forecast - df$Q9_forecast) +
  (1 - 11/T)*(df$Q11_forecast - df$Q10_forecast) +
  (1 - 12/T)*(df$Q12_forecast - df$Q11_forecast)

df$TP_cf <- df$Spread - df$sum_Er_cf

# Final dataframe - columns reordered
df <- df %>%
  select(Date, Yield, Rate, Spread, Excess_Return, HPR, TP, TP_cf, sum_Er, sum_Er_cf, Q1_forecast,
         Q2_forecast, Q3_forecast, Q4_forecast, Q5_forecast, Q6_forecast, Q7_forecast,
         Q8_forecast, Q9_forecast, Q10_forecast, Q11_forecast, Q12_forecast)



