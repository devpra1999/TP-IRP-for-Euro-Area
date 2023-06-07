#LONG TERM BOND DATA----------------------------------------------------------

#Getting the data for German 10 year bonds
Y_G_10 <- read.csv("../Data_Files/Germany-long-bond.csv",
                   skip = 6,col.names = c("Date","Yield","Type"), header = FALSE)
Y_G_10$Date <- ym(Y_G_10$Date)
Y_G_10 <- Y_G_10[order(Y_G_10$Date),]


#Getting the data for French 10 year bonds
Y_F_10 <- read.csv("../Data_Files/France-long-bond.csv",
                   skip = 6,col.names = c("Date","Yield","Type"), header = FALSE)
Y_F_10$Date <- ym(Y_F_10$Date)
Y_F_10 <- Y_F_10[order(Y_F_10$Date),]


#Getting the data for Italian 10 year bonds
Y_I_10 <- read.csv("../Data_Files/Italy-long-bond.csv",
                   skip = 6,col.names = c("Date","Yield","Type"), header = FALSE)
Y_I_10$Date <- ym(Y_I_10$Date)
Y_I_10 <- Y_I_10[order(Y_I_10$Date),]


#Getting the data for Spanish 10 year bonds
Y_S_10 <- read.csv("../Data_Files/Spain-long-bond.csv",
                   skip = 6,col.names = c("Date","Yield","Type"), header = FALSE)
Y_S_10$Date <- ym(Y_S_10$Date)
Y_S_10 <- Y_S_10[order(Y_S_10$Date),]



#SHORT TERM BOND DATA-----------------------------------------------------------

#Getting data for short-term rates

#Market rates
Y_st <- read.csv("../Data_Files/Short-term-rates.csv")
Y_st <- Y_st %>% filter(int_rt == "IRT_M3" & geo == "EA") %>% select(TIME_PERIOD,OBS_VALUE)
colnames(Y_st) <- c("Date","Rate")
Y_st$Date <- ym(Y_st$Date)
Y_st <- Y_st[order(Y_st$Date),]

#Official rates
#Y_st <- read.csv("./Data_Files/ECB-rates.csv", skip = 6
#                 , col.names = c("Date","Rate","Type"))
#Y_st$Date <- ymd(Y_st$Date)


#CONSENSUS FORECASTS DATA--------------------------------------------------------

#The following file contains the forecasts where target period ends 3 months after the survey
#The value for a given quarter refers to the forecast made 3 months (1 quarter) before
f_3 <- read.csv("../Data_Files/Consensus Forecasts Updated/3-months.csv", skip = 6,
                col.names = c("Date","L1_forecast","Type"), header = FALSE)
#Order by date and convert quarterly data into date format
f_3 <- f_3[order(f_3$Date),]
f_3$Date = as.Date(as.yearqtr(f_3$Date, format = "%YQ%q")) + months(3)

#Repeat for 6,9 and 12 month ahead forecasts
f_6 <- read.csv("../Data_Files/Consensus Forecasts Updated/6-months.csv", skip = 6,col.names = c("Date","L2_forecast","Type"), header = FALSE)
f_6 <- f_6[order(f_6$Date),]
f_6$Date = as.Date(as.yearqtr(f_6$Date, format = "%YQ%q")) + months(3)

f_9 <- read.csv("../Data_Files/Consensus Forecasts Updated/9-months.csv", skip = 6,col.names = c("Date","L3_forecast","Type"), header = FALSE)
f_9 <- f_9[order(f_9$Date),]
f_9$Date = as.Date(as.yearqtr(f_9$Date, format = "%YQ%q")) + months(3)

f_12 <- read.csv("../Data_Files/Consensus Forecasts Updated/12-months.csv", skip = 6, header = FALSE)
colnames(f_12)[1:3] = c("Date","L4_forecast","Type")
f_12 <- f_12[order(f_12$Date),]
f_12$Date = as.Date(as.yearqtr(f_12$Date, format = "%YQ%q")) + months(3)

