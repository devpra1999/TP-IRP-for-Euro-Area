#Create subset for the period 2022-last date which we will plot
lr <- tail(master_df,1) #Last Row
recent_data <- master_df %>% filter(Date >= (lr$Date - months(12)))

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

#Data for consensus forecasts for interest rates
fut_rate_consensus <- rep(NA,n)
fut_rate_consensus[1] <- lr$Rate
i <- grep("Q1_forecast", colnames(lr))
fut_rate_consensus[2:13] <- as.numeric(as.vector(lr[1,i:(i+11)]))


#Creating Table
Countries <- c("Germany","France","Spain","Italy")

Yield_10Y <- c(lr$Yield_Germany, lr$Yield_France, lr$Yield_Spain, lr$Yield_Italy)
Model_ER <- fut_rate_mod[2:5]
Model_EM <- c(lr$Yield_Germany-lr$TP_Germany, lr$Yield_France-lr$TP_France,
              lr$Yield_Spain-lr$TP_Spain, lr$Yield_Italy-lr$TP_Italy)
Model_TP <- c(lr$TP_Germany, lr$TP_France, lr$TP_Spain, lr$TP_Italy)
Consensus_ER <- fut_rate_consensus[2:13]
Consensus_EM <- c(lr$Yield_Germany-lr$TP_cf_Germany,  lr$Yield_France-lr$TP_cf_France,
                  lr$Yield_Spain-lr$TP_cf_Spain, lr$Yield_Italy-lr$TP_cf_Italy)
Consensus_TP <-  c(lr$TP_cf_Germany, lr$TP_cf_France, lr$TP_cf_Spain, lr$TP_cf_Italy)

User_EM <- rep(NA,4)
User_TP <- rep(NA,4)

main_table <- data.frame(Countries, Yield_10Y, Model_EM, Model_TP, Consensus_EM,
                         Consensus_TP,User_EM, User_TP)

rm(list=ls()[! ls() %in% c("master_df","Rate_df", "mod", "main_table", "recent_data", "lr", "fut_date","fut_rate_mod","fut_rate_consensus")])
