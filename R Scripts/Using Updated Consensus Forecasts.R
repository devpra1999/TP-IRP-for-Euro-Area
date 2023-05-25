#CALCULATING TERM PREMIA USING CONSENSUS FORECASTS

T <- 40
df$TP_cf <- NA
sum_Er <- (1 - 1/T)*(df$L1_forecast- df$Rate) + (1 - 2/T)*(df$L2_forecast- df$L1_forecast) +
  (1 - 3/T)*(df$L3_forecast- df$L2_forecast) + (1 - 4/T)*(df$L4_forecast- df$L3_forecast)

df$TP_cf <- df$Spread - sum_Er

#Final dataframe - reordered for better visual analysis
df <- df %>%
  select(Date, Yield, Rate, Spread, TP, TP_cf, ESTR, l1, l2, L1_forecast, L2_forecast, L3_forecast, L4_forecast)

#PLOTTING CONSENSUS FORECASTS
#plot(df$Date,df$Yield, type = "l", ylab = "Yield & Composition", xlab = "Date",
#     main = "Decomposition of yield", ylim = c(min(df$Yield, na.rm = TRUE)-2,max(df$Yield,na.rm = TRUE)))
#lines(df$Date,df$TP, lty = "dashed", col = "red",lwd = 2)
#lines(df$Date,(df$Yield - df$TP), lty = "dashed", col = "blue")
#lines(df$Date,rep(0,length(df$Date)), lty = "dotted")
#points(df$Date,df$TP_cf, lwd = 2, pch = 20)
#legend("topright",
#       legend = c("Bond Yield","Term Premia","Short-term","Consensus_TP"),
#       lty = c("solid","dashed","dashed",NA),
#       col = c("black","red","blue","black"),
#       pch = c(NA,NA,NA,20),
#       lwd = c(1,2,1,2)
#)





