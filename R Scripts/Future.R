#Create subset for the period 2022-last date which we will plot
recent_data <- master_df %>%
  select("Date", "Rate", "Yield_Germany", "Spread_Germany", "TP_cf_Germany", "Yield_France", "Spread_France",
         "TP_cf_France", "Yield_Spain", "Spread_Spain", "TP_cf_Spain", "Yield_Italy", "Spread_Italy", "TP_cf_Italy",
         "Q1_forecast", "Q2_forecast", "Q3_forecast", "Q4_forecast", "Q5_forecast", "Q6_forecast",
         "Q7_forecast", "Q8_forecast", "Q9_forecast", "Q10_forecast", "Q11_forecast", "Q12_forecast") %>%
  na.trim(sides = "right")
lr <- tail(recent_data,1) #Last Row
recent_data <- recent_data %>% filter(Date >= (lr$Date - months(12)))

#Create vectors for future date, interest rate movements and interest rates -  using model
fut_date <- seq(lr$Date, as.Date(lr$Date + months(36)), by = "quarter")
n <- length(fut_date)
fut_move_mod <- rep(NA,n)
fut_rate_mod <- rep(NA,n)
move_curr <- tail(Rate_df,1)$FD_Rate
rate_curr <- lr$Rate
for (i in 1:n){
  fut_move_mod[i] <- move_curr
  fut_rate_mod[i] <- rate_curr
  move_curr <- mod$coefficients[1]*move_curr
  rate_curr <- rate_curr + move_curr
}
rm(move_curr, rate_curr, fut_move_mod)
#Data for consensus forecasts for interest rates
fut_rate_consensus <- rep(NA,n)
fut_rate_consensus[1] <- lr$Rate
i <- grep("Q1_forecast", colnames(lr))
fut_rate_consensus[2:13] <- as.numeric(as.vector(lr[1,i:(i+11)]))

#Save consensus forecasts
df_cf <- as.data.frame(cbind(fut_date,fut_rate_consensus))
names(df_cf) <- c("Date","Rate")
df_cf$Date <- as.Date(df_cf$Date)
save(df_cf,file = "SPF_forecasts.rda")


#Creating Table
Country <- c("Germany","France","Spain","Italy")

Yield_10Y <- c(lr$Yield_Germany, lr$Yield_France, lr$Yield_Spain, lr$Yield_Italy)
NaiveModel_ER <- fut_rate_mod[2:5]
# NaiveModel_MP <- c(lr$Yield_Germany-lr$TP_Germany, lr$Yield_France-lr$TP_France,
#               lr$Yield_Spain-lr$TP_Spain, lr$Yield_Italy-lr$TP_Italy)
# NaiveModel_TP <- c(lr$TP_Germany, lr$TP_France, lr$TP_Spain, lr$TP_Italy)
Consensus_ER <- fut_rate_consensus[2:13]
Consensus_MP <- c(lr$Yield_Germany-lr$TP_cf_Germany,  lr$Yield_France-lr$TP_cf_France,
                  lr$Yield_Spain-lr$TP_cf_Spain, lr$Yield_Italy-lr$TP_cf_Italy)
Consensus_TP <-  c(lr$TP_cf_Germany, lr$TP_cf_France, lr$TP_cf_Spain, lr$TP_cf_Italy)
T = length(termpremia_France.ACM)
ACM_TP <- c(termpremia_Germany.ACM[T],termpremia_France.ACM[T],
                     termpremia_Spain.ACM[T],termpremia_Italy.ACM[T])
ACM_MP <- rep(RiskFreeYields.ACM.ts[T,40]*100,4)
#T = length(termpremia_France.FF)
#FF_TP <- c(termpremia_Germany.FF[T],termpremia_France.FF[T],
#                    termpremia_Spain.FF[T],termpremia_Italy.FF[T])
#FF_MP <- rep(RiskFreeYields.FF.ts[T,40]*100,4)


#User_EM <- rep(NA,4)
#User_TP <- rep(NA,4)

main_table <- data.frame(Country, Yield_10Y, 
                         Consensus_MP, Consensus_TP, 
                         ACM_MP, ACM_TP)
#                         FF_MP, FF_TP)

rm(Country, Yield_10Y, 
   Consensus_MP, Consensus_TP, 
   ACM_MP, ACM_TP)
#   FF_MP, FF_TP)
#rm(list=ls()[! ls() %in% c("master_df","Rate_df", "mod", "main_table", "recent_data", "lr", "fut_date","fut_rate_mod","fut_rate_consensus")])
