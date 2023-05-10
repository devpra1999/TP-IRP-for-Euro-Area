#We use the lubridate package for (partial) date conversions
#dplyr for table manipulations
library(lubridate)
library(dplyr)

#Getting the data for German 10 year bonds"
Y_G_10 <- read.csv("~/Economics/TP-IRP/Data_Files/Germany-long-bond.csv",
                   skip = 6,col.names = c("Date","Yield","Type"))
Y_G_10$Date <- ym(Y_G_10$Date)
plot(Y_G_10$Date,Y_G_10$Yield, type = "l",ylab = "Yield",
     xlab = "Date", main = "Yields for 10 year bond - Germany")

#Getting data for short-term rates
Y_st <- read.csv("~/Economics/TP-IRP/Data_Files/Short-term-rates.csv")
Y_st <- Y_st %>% filter(int_rt == "IRT_M3" & geo == "EA") %>% select(TIME_PERIOD,OBS_VALUE)
colnames(Y_st) <- c("Date","Rate")
Y_st$Date <- ym(Y_st$Date)
plot(Y_st$Date, Y_st$Rate, type = "l", ylab = "Rate", xlab = "Date",
     main = "Short Term Rates for the Euro Area")

df <- merge(Y_G_10,Y_st, by = "Date")
df <- df %>% select(Date, Yield, Rate) %>% filter(Date >= "1999-01-01")
df$Spread <- df$Yield - df$Rate




