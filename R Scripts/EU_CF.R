####GET GERMANY YIELD CURVE--------------------------------
N = 60
source("NSS.R")
L <- nss_yields("Data_Files/gnss.csv",N*3)
rawYields <- L$rawYields/100
plot_dates <- L$plot_dates
Yields <- as.matrix(rawYields)
Yields <- xts(Yields,plot_dates)
Yields <- Yields[seq(from = 3, to = 285, by = 3),seq(from = 3, to = N*3, length.out = N)]
colnames(Yields) <- paste("n",seq(1,N),sep = "_")
T = nrow(Yields)

CF_Germany <- read_csv("Data_Files/Germany.csv")
colnames(CF_Germany) <- c("Date","Q1_forecast","Q4_forecast")
CF_Germany$Q2_forecast <- NA
CF_Germany$Q3_forecast <- NA
CF_Germany <- CF_Germany %>% select(Date, Q1_forecast, Q2_forecast, Q3_forecast, Q4_forecast)
temp <- as.data.frame(t(na.approx(t(CF_Germany[2:5]))))
temp <- temp/100
CF_Germany <- cbind(CF_Germany$Date,temp)
colnames(CF_Germany) <- c("Date","Q1_forecast","Q2_forecast","Q3_forecast","Q4_forecast")
CF_Germany.ts <- xts(CF_Germany[,2:5], order.by = CF_Germany$Date)
CF_Germany.ts <- merge.xts(CF_Germany.ts,Yields$n_1, join = "inner")
CF_Germany.ts <- merge.xts(CF_Germany.ts,Yields$n_40, join = "inner")


#CALCULATING TERM PREMIA USING CONSENSUS FORECASTS
T <- 40
CF_Germany.ts$TP_cf <- NA
CF_Germany.ts$sum_Er_cf <- (1 - 1/T)*(CF_Germany.ts$Q1_forecast - CF_Germany.ts$n_1) +
  (1 - 2/T)*(CF_Germany.ts$Q2_forecast - CF_Germany.ts$Q1_forecast) +
  (1 - 3/T)*(CF_Germany.ts$Q3_forecast - CF_Germany.ts$Q2_forecast) +
  (1 - 4/T)*(CF_Germany.ts$Q4_forecast - CF_Germany.ts$Q3_forecast)
CF_Germany.ts$TP_cf <- CF_Germany.ts$n_40 - CF_Germany.ts$n_1 - CF_Germany.ts$sum_Er_cf

CF_Germany_old <- master_df %>% select(Date,Yield_Germany,TP_cf_Germany) %>% drop_na()
CF_Germany_old$Date <- CF_Germany_old$Date - months(1)
CF_Germany_old.ts <- xts(CF_Germany_old[2:3],order.by = CF_Germany_old$Date)
CF_Germany.ts <- merge.xts(CF_Germany.ts,CF_Germany_old.ts)

plot(CF_Germany.ts$TP_cf*100) 
lines(CF_Germany.ts$TP_cf_Germany, col = "blue")
