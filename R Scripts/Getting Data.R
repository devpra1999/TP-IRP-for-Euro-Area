#We use the lubridate package for (partial) date conversions
#dplyr for table manipulations
library(lubridate)
library(dplyr)

#LONG TERM BOND DATA------------------------------------------------
#Getting the data for German 10 year bonds
Y_G_10 <- read.csv("~/Economics/TP-IRP/Data_Files/Germany-long-bond.csv",
                   skip = 6,col.names = c("Date","Yield","Type"))
Y_G_10$Date <- ym(Y_G_10$Date)
plot(Y_G_10$Date,Y_G_10$Yield, type = "l",ylab = "Yield",
     xlab = "Date", main = "Yields for 10 year bond - Germany")

#Getting the data for French 10 year bonds
Y_F_10 <- read.csv("~/Economics/TP-IRP/Data_Files/France-long-bond.csv",
                   skip = 6,col.names = c("Date","Yield","Type"))
Y_F_10$Date <- ym(Y_F_10$Date)
plot(Y_F_10$Date,Y_F_10$Yield, type = "l",ylab = "Yield",
     xlab = "Date", main = "Yields for 10 year bond - France")

#Getting the data for Italian 10 year bonds
Y_I_10 <- read.csv("~/Economics/TP-IRP/Data_Files/Italy-long-bond.csv",
                   skip = 6,col.names = c("Date","Yield","Type"))
Y_I_10$Date <- ym(Y_I_10$Date)
plot(Y_I_10$Date,Y_I_10$Yield, type = "l",ylab = "Yield",
     xlab = "Date", main = "Yields for 10 year bond - Italy")

#Getting the data for Spanish 10 year bonds
Y_S_10 <- read.csv("~/Economics/TP-IRP/Data_Files/Spain-long-bond.csv",
                   skip = 6,col.names = c("Date","Yield","Type"))
Y_S_10$Date <- ym(Y_S_10$Date)
plot(Y_S_10$Date,Y_S_10$Yield, type = "l",ylab = "Yield",
     xlab = "Date", main = "Yields for 10 year bond - Spain")


#SHORT TERM BOND DATA------------------------------------------------------
#Getting data for short-term rates
Y_st <- read.csv("~/Economics/TP-IRP/Data_Files/Short-term-rates.csv")
Y_st <- Y_st %>% filter(int_rt == "IRT_M3" & geo == "EA") %>% select(TIME_PERIOD,OBS_VALUE)
colnames(Y_st) <- c("Date","Rate")
Y_st$Date <- ym(Y_st$Date)
plot(Y_st$Date, Y_st$Rate, type = "l", ylab = "Rate", xlab = "Date",
     main = "Short Term Rates for the Euro Area")




