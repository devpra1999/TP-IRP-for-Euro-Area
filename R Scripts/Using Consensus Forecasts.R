#CALCULATING TERM PREMIA USING CONSENSUS FORECASTS

#Adding consensus forecasts of short term rates
df$forecasts <- NA

f_2013 <- read.csv("~/Economics/TP-IRP/Data_Files/Consensus Forecasts/2013.csv", skip = 6,
         col.names = c("Date","Rate_Forecast","Type"))
f_2013 <- f_2013[order(f_2013$Date),]
df$forecasts[which(df$Date=="2013-01-01")] <- I(list(f_2013$Rate_Forecast))

f_2014 <- read.csv("~/Economics/TP-IRP/Data_Files/Consensus Forecasts/2014.csv", skip = 6,
                   col.names = c("Date","Rate_Forecast","Type"))
f_2014 <- f_2014[order(f_2014$Date),]
df$forecasts[which(df$Date=="2014-01-01")] <- I(list(f_2014$Rate_Forecast))

f_2015 <- read.csv("~/Economics/TP-IRP/Data_Files/Consensus Forecasts/2015.csv", skip = 6,
                   col.names = c("Date","Rate_Forecast","Type"))
f_2015 <- f_2015[order(f_2015$Date),]
df$forecasts[which(df$Date=="2015-01-01")] <- list(f_2015$Rate_Forecast)

f_2016 <- read.csv("~/Economics/TP-IRP/Data_Files/Consensus Forecasts/2016.csv", skip = 6,
                   col.names = c("Date","Rate_Forecast","Type"))
f_2016 <- f_2016[order(f_2016$Date),]
df$forecasts[which(df$Date=="2016-01-01")] <- list(f_2016$Rate_Forecast)

f_2017 <- read.csv("~/Economics/TP-IRP/Data_Files/Consensus Forecasts/2017.csv", skip = 6,
                   col.names = c("Date","Rate_Forecast","Type"))
f_2017 <- f_2017[order(f_2017$Date),]
df$forecasts[which(df$Date=="2017-01-01")] <- list(f_2017$Rate_Forecast)

f_2018 <- read.csv("~/Economics/TP-IRP/Data_Files/Consensus Forecasts/2018.csv", skip = 6,
                   col.names = c("Date","Rate_Forecast","Type"))
f_2018 <- f_2018[order(f_2018$Date),]
df$forecasts[which(df$Date=="2018-01-01")] <- list(f_2018$Rate_Forecast)

f_2019 <- read.csv("~/Economics/TP-IRP/Data_Files/Consensus Forecasts/2019.csv", skip = 6,
                   col.names = c("Date","Rate_Forecast","Type"))
f_2019 <- f_2019[order(f_2019$Date),]
df$forecasts[which(df$Date=="2019-01-01")] <- list(f_2019$Rate_Forecast)

f_2020 <- read.csv("~/Economics/TP-IRP/Data_Files/Consensus Forecasts/2020.csv", skip = 6,
                   col.names = c("Date","Rate_Forecast","Type"))
f_2020 <- f_2020[order(f_2020$Date),]
df$forecasts[which(df$Date=="2020-01-01")] <- list(f_2020$Rate_Forecast)

f_2021 <- read.csv("~/Economics/TP-IRP/Data_Files/Consensus Forecasts/2021.csv", skip = 6,
                   col.names = c("Date","Rate_Forecast","Type"))
f_2021 <- f_2021[order(f_2021$Date),]
df$forecasts[which(df$Date=="2021-01-01")] <- list(f_2021$Rate_Forecast)

#f_2022 <- read.csv("~/Economics/TP-IRP/Data_Files/Consensus Forecasts/2022.csv", skip = 6, col.names = c("Date","Rate_Forecast","Type"))
#f_2022 <- f_2022[order(f_2022$Date),]
#df$forecasts[which(df$Date=="2022-01-01")] <- list(f_2022$Rate_Forecast)


#Calculating Term Premia
T <- 40
df$TP_cf <- NA

#ind = which(df$Date=="2013-01-01")
#f_s <- unlist(df$forecasts)
sum_Er <- (1 - 1/T)*(sapply(df$forecasts, function(x) x[1]) - df$Rate)
for (i in 2:7){
  sum_Er <- sum_Er + (1-i/T)*
    (sapply(df$forecasts, function(x) x[i]) -
       sapply(df$forecasts, function(x) x[i-1]))
}
df$TP_cf <- df$Spread - sum_Er

plot(df$Date,df$Yield, type = "l", ylab = "Yield & Composition", xlab = "Date",
     main = "Decomposition of yield", ylim = c(min(df$Yield)-2,max(df$Yield)))
lines(df$Date,df$TP, lty = "dashed", col = "red",lwd = 2)
lines(df$Date,(df$Yield - df$TP), lty = "dashed", col = "blue")
lines(df$Date,rep(0,length(df$Date)), lty = "dotted")
points(df$Date,df$TP_cf, lwd = 3)
legend("topright",
       legend = c("Bond Yield","Term Premia","Short-term","Consensus_TP"),
       lty = c("solid","dashed","dashed",NA),
       col = c("black","red","blue","black"),
       pch = c(NA,NA,NA,1),
       lwd = c(1,2,1,3)
       )

