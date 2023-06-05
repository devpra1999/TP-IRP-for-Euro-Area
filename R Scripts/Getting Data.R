#We use the lubridate package for (partial) date conversions
#dplyr for table manipulations
library(lubridate)
library(dplyr)
library(zoo)
library(tidyverse)

#LONG TERM BOND DATA----------------------------------------------------------
#Getting the data for German 10 year bonds
url <- "https://sdw.ecb.europa.eu/quickviewexport.do;jsessionid=99AB5C95BA2BC70BAEB76C2AF91DA3AD?SERIES_KEY=229.IRS.M.DE.L.L40.CI.0000.EUR.N.Z&type=xls"
dest_file <- "../Data_Files/Germany-long-bond.csv"
download.file(url, dest_file,quite = TRUE)

#Getting the data for French 10 year bonds
url <- "https://sdw.ecb.europa.eu/quickviewexport.do;jsessionid=1D71340A49B14ABB4F5F8DBFF7E352A6?SERIES_KEY=229.IRS.M.FR.L.L40.CI.0000.EUR.N.Z&type=xls"
dest_file <- "../Data_Files/France-long-bond.csv"
download.file(url, dest_file,quite = TRUE)

#Getting the data for Italian 10 year bonds
url <- "https://sdw.ecb.europa.eu/quickviewexport.do;jsessionid=7623896DA72569BAD7559B496FC7B68C?SERIES_KEY=229.IRS.M.IT.L.L40.CI.0000.EUR.N.Z&type=xls"
dest_file <- "../Data_Files/Italy-long-bond.csv"
download.file(url, dest_file,quite = TRUE)

#Getting the data for Spanish 10 year bonds
url <- "https://sdw.ecb.europa.eu/quickviewexport.do;jsessionid=E6744786D25FD0F868A4ABA7695FE13C?SERIES_KEY=229.IRS.M.ES.L.L40.CI.0000.EUR.N.Z&type=xls"
dest_file <- "../Data_Files/Spain-long-bond.csv"
download.file(url, dest_file,quite = TRUE)


#SHORT TERM BOND DATA-----------------------------------------------------------
#Market rates
url <- "https://ec.europa.eu/eurostat/api/dissemination/sdmx/2.1/data/IRT_ST_M/?format=SDMX-CSV"
dest_file <- "../Data_Files/Short-term-rates.csv"
download.file(url, dest_file,quite = TRUE)

#Official rates
#Y_st <- read.csv("./Data_Files/ECB-rates.csv", skip = 6
#                 , col.names = c("Date","Rate","Type"))
#Y_st$Date <- ymd(Y_st$Date)


#CONSENSUS FORECASTS DATA--------------------------------------------------------

#The following file contains the forecasts where target period ends 3 months after the survey
#The value for a given quarter refers to the forecast made 3 months (1 quarter) before
url <- "https://sdw.ecb.europa.eu/quickviewexport.do;jsessionid=4BFB50DC40CA72B025B1F33EF9F27E96?SERIES_KEY=138.SPF.Q.U2.ASSU.IR.P3M.Q.AVG&type=xls"
dest_file <- "../Data_Files/Consensus Forecasts Updated/3-months.csv"
download.file(url, dest_file,quite = TRUE)

#Repeat for 6,9 and 12 month ahead forecasts
url <- "https://sdw.ecb.europa.eu/quickviewexport.do;jsessionid=CEF3E31DC14E94FA19F368B5DEE9297F?SERIES_KEY=138.SPF.Q.U2.ASSU.IR.P6M.Q.AVG&type=xls"
dest_file <- "../Data_Files/Consensus Forecasts Updated/6-months.csv"
download.file(url, dest_file,quite = TRUE)

url <- "https://sdw.ecb.europa.eu/quickviewexport.do;jsessionid=D8248552EA7D6E8D68F21D26D5A436AB?SERIES_KEY=138.SPF.Q.U2.ASSU.IR.P9M.Q.AVG&type=xls"
dest_file <- "../Data_Files/Consensus Forecasts Updated/9-months.csv"
download.file(url, dest_file,quite = TRUE)

url <- "https://sdw.ecb.europa.eu/quickviewexport.do;jsessionid=EF4872E8D06766518C57CA6448EA62FD?SERIES_KEY=138.SPF.Q.U2.ASSU.IR.P12M.Q.AVG&type=xls"
dest_file <- "../Data_Files/Consensus Forecasts Updated/12-months.csv"
download.file(url, dest_file,quite = TRUE)
