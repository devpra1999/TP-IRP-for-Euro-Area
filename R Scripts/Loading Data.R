#LONG TERM BOND DATA----------------------------------------------------------

#Getting the data for German 10 year bonds
Y_G_10 <- read.csv("Germany-long-bond.csv",
                   skip = 6,col.names = c("Date","Yield","Type"), header = FALSE)
Y_G_10$Date <- ym(Y_G_10$Date)
Y_G_10 <- Y_G_10[order(Y_G_10$Date),]


#Getting the data for French 10 year bonds
Y_F_10 <- read.csv("France-long-bond.csv",
                   skip = 6,col.names = c("Date","Yield","Type"), header = FALSE)
Y_F_10$Date <- ym(Y_F_10$Date)
Y_F_10 <- Y_F_10[order(Y_F_10$Date),]


#Getting the data for Italian 10 year bonds
Y_I_10 <- read.csv("Italy-long-bond.csv",
                   skip = 6,col.names = c("Date","Yield","Type"), header = FALSE)
Y_I_10$Date <- ym(Y_I_10$Date)
Y_I_10 <- Y_I_10[order(Y_I_10$Date),]


#Getting the data for Spanish 10 year bonds
Y_S_10 <- read.csv("Spain-long-bond.csv",
                   skip = 6,col.names = c("Date","Yield","Type"), header = FALSE)
Y_S_10$Date <- ym(Y_S_10$Date)
Y_S_10 <- Y_S_10[order(Y_S_10$Date),]



#SHORT TERM BOND DATA-----------------------------------------------------------

#Getting data for short-term rates

#Market rates
Y_st <- read.csv("Short-term-rates.csv")
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
f_3 <- read.csv("3-months.csv", skip = 6,
                col.names = c("Date","L1_forecast","Type"), header = FALSE)
#Order by date and convert quarterly data into date format
f_3 <- f_3[order(f_3$Date),]
f_3$Date = as.Date(as.yearqtr(f_3$Date, format = "%YQ%q")) + months(3)

#Repeat for 6,9 and 12 month ahead forecasts
f_6 <- read.csv("6-months.csv", skip = 6,col.names = c("Date","L2_forecast","Type"), header = FALSE)
f_6 <- f_6[order(f_6$Date),]
f_6$Date = as.Date(as.yearqtr(f_6$Date, format = "%YQ%q")) + months(3)

f_9 <- read.csv("9-months.csv", skip = 6,col.names = c("Date","L3_forecast","Type"), header = FALSE)
f_9 <- f_9[order(f_9$Date),]
f_9$Date = as.Date(as.yearqtr(f_9$Date, format = "%YQ%q")) + months(3)

f_12 <- read.csv("12-months.csv", skip = 6, header = FALSE)
colnames(f_12)[1:3] = c("Date","L4_forecast","Type")
f_12 <- f_12[order(f_12$Date),]
f_12$Date = as.Date(as.yearqtr(f_12$Date, format = "%YQ%q")) + months(3)


#ANNUAL DATA NOW
# Read and process 15-months.csv
f_15 <- read.csv("15-months.csv", skip = 6, col.names = c("Date", "L5_forecast", "Type"), header = FALSE)
f_15 <- f_15[order(f_15$Date),]
f_15$Date <- as.Date(as.yearqtr(f_15$Date, format = "%YQ%q")) + months(12)

# Read and process 18-months.csv
f_18 <- read.csv("18-months.csv", skip = 6, col.names = c("Date", "L6_forecast", "Type"), header = FALSE)
f_18 <- f_18[order(f_18$Date),]
f_18$Date <- as.Date(as.yearqtr(f_18$Date, format = "%YQ%q")) + months(12)

# Read and process 21-months.csv
f_21 <- read.csv("21-months.csv", skip = 6, col.names = c("Date", "L7_forecast", "Type"), header = FALSE)
f_21 <- f_21[order(f_21$Date),]
f_21$Date <- as.Date(as.yearqtr(f_21$Date, format = "%YQ%q")) + months(12)

# Read and process 24-months.csv
f_24 <- read.csv("24-months.csv", skip = 6, col.names = c("Date", "L8_forecast", "Type"), header = FALSE)
f_24 <- f_24[order(f_24$Date),]
f_24$Date <- as.Date(as.yearqtr(f_24$Date, format = "%YQ%q")) + months(12)

# Continue this pattern for 27, 30, 33, and 36 months
f_27 <- read.csv("27-months.csv", skip = 6, col.names = c("Date", "L9_forecast", "Type"), header = FALSE)
f_27 <- f_27[order(f_27$Date),]
f_27$Date <- as.Date(as.yearqtr(f_27$Date, format = "%YQ%q")) + months(12)

f_30 <- read.csv("30-months.csv", skip = 6, col.names = c("Date", "L10_forecast", "Type"), header = FALSE)
f_30 <- f_30[order(f_30$Date),]
f_30$Date <- as.Date(as.yearqtr(f_30$Date, format = "%YQ%q")) + months(12)

f_33 <- read.csv("33-months.csv", skip = 6, col.names = c("Date", "L11_forecast", "Type"), header = FALSE)
f_33 <- f_33[order(f_33$Date),]
f_33$Date <- as.Date(as.yearqtr(f_33$Date, format = "%YQ%q")) + months(12)

f_36 <- read.csv("36-months.csv", skip = 6, col.names = c("Date", "L12_forecast", "Type"), header = FALSE)
f_36 <- f_36[order(f_36$Date),]
f_36$Date <- as.Date(as.yearqtr(f_36$Date, format = "%YQ%q")) + months(12)




