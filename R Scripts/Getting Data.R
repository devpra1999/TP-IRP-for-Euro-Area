#We use the lubridate package for (partial) date conversions
#dplyr for table manipulations
library(lubridate)
library(dplyr)
library(zoo)
library(tidyverse)
install.packages("R.utils")


#LONG TERM BOND DATA----------------------------------------------------------

#Getting the data for German 10 year bonds
url <- "https://sdw.ecb.europa.eu/quickviewexport.do;jsessionid=99AB5C95BA2BC70BAEB76C2AF91DA3AD?SERIES_KEY=229.IRS.M.DE.L.L40.CI.0000.EUR.N.Z&type=xls"
dest_file <- "../Data_Files/Germany-long-bond.csv"
download.file(url, dest_file)
Y_G_10 <- read.csv("../Data_Files/Germany-long-bond.csv",
                   skip = 6,col.names = c("Date","Yield","Type"), header = FALSE)
Y_G_10$Date <- ym(Y_G_10$Date)
Y_G_10 <- Y_G_10[order(Y_G_10$Date),]


#Getting the data for French 10 year bonds
url <- "https://sdw.ecb.europa.eu/quickviewexport.do;jsessionid=1D71340A49B14ABB4F5F8DBFF7E352A6?SERIES_KEY=229.IRS.M.FR.L.L40.CI.0000.EUR.N.Z&type=xls"
dest_file <- "../Data_Files/France-long-bond.csv"
download.file(url, dest_file)
Y_F_10 <- read.csv("../Data_Files/France-long-bond.csv",
                   skip = 6,col.names = c("Date","Yield","Type"), header = FALSE)
Y_F_10$Date <- ym(Y_F_10$Date)
Y_F_10 <- Y_F_10[order(Y_F_10$Date),]


#Getting the data for Italian 10 year bonds
url <- "https://sdw.ecb.europa.eu/quickviewexport.do;jsessionid=7623896DA72569BAD7559B496FC7B68C?SERIES_KEY=229.IRS.M.IT.L.L40.CI.0000.EUR.N.Z&type=xls"
dest_file <- "../Data_Files/Italy-long-bond.csv"
download.file(url, dest_file)
Y_I_10 <- read.csv("../Data_Files/Italy-long-bond.csv",
                   skip = 6,col.names = c("Date","Yield","Type"), header = FALSE)
Y_I_10$Date <- ym(Y_I_10$Date)
Y_I_10 <- Y_I_10[order(Y_I_10$Date),]


#Getting the data for Spanish 10 year bonds
url <- "https://sdw.ecb.europa.eu/quickviewexport.do;jsessionid=E6744786D25FD0F868A4ABA7695FE13C?SERIES_KEY=229.IRS.M.ES.L.L40.CI.0000.EUR.N.Z&type=xls"
dest_file <- "../Data_Files/Spain-long-bond.csv"
download.file(url, dest_file)
Y_S_10 <- read.csv("../Data_Files/Spain-long-bond.csv",
                   skip = 6,col.names = c("Date","Yield","Type"), header = FALSE)
Y_S_10$Date <- ym(Y_S_10$Date)
Y_S_10 <- Y_S_10[order(Y_S_10$Date),]



#SHORT TERM BOND DATA-----------------------------------------------------------

#Getting data for short-term rates

#Market rates
url <- "https://ec.europa.eu/eurostat/api/dissemination/sdmx/2.1/data/IRT_ST_M/?format=SDMX-CSV&compressed=true"
dest_file <- "../Data_Files/Short-term-rates.csv.gz"
download.file(url, dest_file)
library(R.utils)
gunzip(dest_file, remove=FALSE)
Y_st <- read.csv("../Data_Files/Short-term-rates.csv")
Y_st <- Y_st %>% filter(int_rt == "IRT_M3" & geo == "EA") %>% select(TIME_PERIOD,OBS_VALUE)
colnames(Y_st) <- c("Date","Rate")
Y_st$Date <- ym(Y_st$Date)
Y_st <- Y_st[order(Y_st$Date),]

#Official rates
#Y_st <- read.csv("./Data_Files/ECB-rates.csv", skip = 6
#                 , col.names = c("Date","Rate","Type"))
#Y_st$Date <- ymd(Y_st$Date)

#Plot short term rates
#plot(Y_st$Date, Y_st$Rate, type = "l", ylab = "Rate", xlab = "Date",
#     main = "Short Term Rates for the Euro Area")



#CONSENSUS FORECASTS DATA--------------------------------------------------------

#The following file contains the forecasts where target period ends 3 months after the survey
#The value for a given quarter refers to the forecast made 3 months (1 quarter) before
url <- "https://sdw.ecb.europa.eu/quickviewexport.do;jsessionid=4BFB50DC40CA72B025B1F33EF9F27E96?SERIES_KEY=138.SPF.Q.U2.ASSU.IR.P3M.Q.AVG&type=xls"
dest_file <- "../Data_Files/Consensus Forecasts Updated/3-months.csv"
download.file(url, dest_file)
f_3 <- read.csv("../Data_Files/Consensus Forecasts Updated/3-months.csv", skip = 6,
                col.names = c("Date","L1_forecast","Type"), header = FALSE)
#Order by date and convert quarterly data into date format
f_3 <- f_3[order(f_3$Date),]
f_3$Date = as.Date(as.yearqtr(f_3$Date, format = "%YQ%q")) + months(3)

#Repeat for 6,9 and 12 month ahead forecasts
url <- "https://sdw.ecb.europa.eu/quickviewexport.do;jsessionid=CEF3E31DC14E94FA19F368B5DEE9297F?SERIES_KEY=138.SPF.Q.U2.ASSU.IR.P6M.Q.AVG&type=xls"
dest_file <- "../Data_Files/Consensus Forecasts Updated/6-months.csv"
download.file(url, dest_file)
f_6 <- read.csv("../Data_Files/Consensus Forecasts Updated/6-months.csv", skip = 6,col.names = c("Date","L2_forecast","Type"), header = FALSE)
f_6 <- f_6[order(f_6$Date),]
f_6$Date = as.Date(as.yearqtr(f_6$Date, format = "%YQ%q")) + months(3)
url <- "https://sdw.ecb.europa.eu/quickviewexport.do;jsessionid=CEF3E31DC14E94FA19F368B5DEE9297F?SERIES_KEY=138.SPF.Q.U2.ASSU.IR.P6M.Q.AVG&type=xls"
dest_file <- "../Data_Files/Consensus Forecasts Updated/9-months.csv"
download.file(url, dest_file)
f_9 <- read.csv("../Data_Files/Consensus Forecasts Updated/9-months.csv", skip = 6,col.names = c("Date","L3_forecast","Type"), header = FALSE)
f_9 <- f_9[order(f_9$Date),]
f_9$Date = as.Date(as.yearqtr(f_9$Date, format = "%YQ%q")) + months(3)
url <- "https://sdw.ecb.europa.eu/quickviewexport.do;jsessionid=EF4872E8D06766518C57CA6448EA62FD?SERIES_KEY=138.SPF.Q.U2.ASSU.IR.P12M.Q.AVG&type=xls"
dest_file <- "../Data_Files/Consensus Forecasts Updated/12-months.csv"
download.file(url, dest_file)
f_12 <- read.csv("../Data_Files/Consensus Forecasts Updated/12-months.csv", skip = 6, col.names = c("Date","L4_forecast","Type"), header = FALSE)
f_12 <- f_12[order(f_12$Date),]
f_12$Date = as.Date(as.yearqtr(f_12$Date, format = "%YQ%q")) + months(3)


#PLOTS
#plot(Y_G_10$Date,Y_G_10$Yield,type="l",ylab="Yield",xlab="Date",main="Yields for 10 year bond - Germany")

