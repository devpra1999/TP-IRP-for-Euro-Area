#LONG TERM BOND DATA----------------------------------------------------------
#Getting the data for German 10 year bonds
url <- "https://sdw.ecb.europa.eu/quickviewexport.do;jsessionid=99AB5C95BA2BC70BAEB76C2AF91DA3AD?SERIES_KEY=229.IRS.M.DE.L.L40.CI.0000.EUR.N.Z&type=xls"
dest_file <- "Germany-long-bond.csv"
download.file(url, dest_file,quite = TRUE)

#Getting the data for French 10 year bonds
url <- "https://sdw.ecb.europa.eu/quickviewexport.do;jsessionid=1D71340A49B14ABB4F5F8DBFF7E352A6?SERIES_KEY=229.IRS.M.FR.L.L40.CI.0000.EUR.N.Z&type=xls"
dest_file <- "France-long-bond.csv"
download.file(url, dest_file,quite = TRUE)

#Getting the data for Italian 10 year bonds
url <- "https://sdw.ecb.europa.eu/quickviewexport.do;jsessionid=7623896DA72569BAD7559B496FC7B68C?SERIES_KEY=229.IRS.M.IT.L.L40.CI.0000.EUR.N.Z&type=xls"
dest_file <- "Italy-long-bond.csv"
download.file(url, dest_file,quite = TRUE)

#Getting the data for Spanish 10 year bonds
url <- "https://sdw.ecb.europa.eu/quickviewexport.do;jsessionid=E6744786D25FD0F868A4ABA7695FE13C?SERIES_KEY=229.IRS.M.ES.L.L40.CI.0000.EUR.N.Z&type=xls"
dest_file <- "Spain-long-bond.csv"
download.file(url, dest_file,quite = TRUE)


#SHORT TERM BOND DATA-----------------------------------------------------------
#Market rates
url <- "https://ec.europa.eu/eurostat/api/dissemination/sdmx/2.1/data/IRT_ST_M/?format=SDMX-CSV"
dest_file <- "Short-term-rates.csv"
download.file(url, dest_file,quite = TRUE)

#Official rates
#Y_st <- read.csv("./Data_Files/ECB-rates.csv", skip = 6
#                 , col.names = c("Date","Rate","Type"))
#Y_st$Date <- ymd(Y_st$Date)


#CONSENSUS FORECASTS DATA--------------------------------------------------------

#The following file contains the forecasts where target period ends 3 months after the survey
#The value for a given quarter refers to the forecast made 3 months (1 quarter) before
url <- "https://sdw.ecb.europa.eu/quickviewexport.do;jsessionid=4BFB50DC40CA72B025B1F33EF9F27E96?SERIES_KEY=138.SPF.Q.U2.ASSU.IR.P3M.Q.AVG&type=xls"
dest_file <- "3-months.csv"
download.file(url, dest_file,quite = TRUE)

#Repeat for 6,9 and 12 month ahead forecasts
url <- "https://sdw.ecb.europa.eu/quickviewexport.do;jsessionid=CEF3E31DC14E94FA19F368B5DEE9297F?SERIES_KEY=138.SPF.Q.U2.ASSU.IR.P6M.Q.AVG&type=xls"
dest_file <- "6-months.csv"
download.file(url, dest_file,quite = TRUE)

url <- "https://sdw.ecb.europa.eu/quickviewexport.do;jsessionid=D8248552EA7D6E8D68F21D26D5A436AB?SERIES_KEY=138.SPF.Q.U2.ASSU.IR.P9M.Q.AVG&type=xls"
dest_file <- "9-months.csv"
download.file(url, dest_file,quite = TRUE)

url <- "https://sdw.ecb.europa.eu/quickviewexport.do;jsessionid=EF4872E8D06766518C57CA6448EA62FD?SERIES_KEY=138.SPF.Q.U2.ASSU.IR.P12M.Q.AVG&type=xls"
dest_file <- "12-months.csv"
download.file(url, dest_file,quite = TRUE)

url <- "https://sdw.ecb.europa.eu/quickviewexport.do;jsessionid=A65A8B3F145A81B6FB2A08389F4DE5A3?SERIES_KEY=138.SPF.A.U2.ASSU.IR.P15M.Q.AVG&type=xls"
dest_file <- "15-months.csv"
download.file(url, dest_file,quite = TRUE)

url <- "https://sdw.ecb.europa.eu/quickviewexport.do;jsessionid=B521A9FF56061B8B63225B7A5FF93C03?SERIES_KEY=138.SPF.A.U2.ASSU.IR.P18M.Q.AVG&type=xls"
dest_file <- "18-months.csv"
download.file(url, dest_file,quite = TRUE)

url <- "https://sdw.ecb.europa.eu/quickviewexport.do;jsessionid=7F56F3AF9BD7F15959D1110DD46CDD55?SERIES_KEY=138.SPF.A.U2.ASSU.IR.P21M.Q.AVG&type=xls"
dest_file <- "21-months.csv"
download.file(url, dest_file,quite = TRUE)

url <- "https://sdw.ecb.europa.eu/quickviewexport.do;jsessionid=1CC9F510DEEF4BEAEC929142B89FD8C6?SERIES_KEY=138.SPF.A.U2.ASSU.IR.P24M.Q.AVG&type=xls"
dest_file <- "24-months.csv"
download.file(url, dest_file,quite = TRUE)

url <- "https://sdw.ecb.europa.eu/quickviewexport.do;jsessionid=5491FE489D7777018439FBB97062684A?SERIES_KEY=138.SPF.A.U2.ASSU.IR.P27M.Q.AVG&type=xls"
dest_file <- "27-months.csv"
download.file(url, dest_file,quite = TRUE)

url <- "https://sdw.ecb.europa.eu/quickviewexport.do;jsessionid=2E93713EFCB0EA007428CB34FB122869?SERIES_KEY=138.SPF.A.U2.ASSU.IR.P30M.Q.AVG&type=xls"
dest_file <- "30-months.csv"
download.file(url, dest_file,quite = TRUE)

url <- "https://sdw.ecb.europa.eu/quickviewexport.do;jsessionid=2B8CD0FB7E8212F80FA7F7137489F0E3?SERIES_KEY=138.SPF.A.U2.ASSU.IR.P33M.Q.AVG&type=xls"
dest_file <- "33-months.csv"
download.file(url, dest_file,quite = TRUE)

url <- "https://sdw.ecb.europa.eu/quickviewexport.do;jsessionid=88E02E1EEB11AC119CEAF714FB14438D?SERIES_KEY=138.SPF.A.U2.ASSU.IR.P36M.Q.AVG&type=xls"
dest_file <- "36-months.csv"
download.file(url, dest_file,quite = TRUE)
