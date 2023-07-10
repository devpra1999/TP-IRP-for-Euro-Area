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


#Macroeconomic Variables

url <- "https://ec.europa.eu/eurostat/api/dissemination/sdmx/2.1/data/NAMQ_10_GDP/Q.CP_MEUR.SCA.B1GQ.DE+ES+FR+IT/?format=SDMX-CSV&startPeriod=1999-Q4&endPeriod=2023-Q1"
dest_file <- "gdp_data.csv"
download.file(url,dest_file,quite = TRUE)

url <- "https://sdw.ecb.europa.eu/quickviewexport.do?SERIES_KEY=320.MNA.Q.Y.DE.W2.S1.S1.B.B1GQ._Z._Z._Z.XDC.LR.GY&type=xls"
dest_file <- "Germany-real-growth-yy.csv"
download.file(url, dest_file,quite = TRUE)

url <- "https://sdw.ecb.europa.eu/quickviewexport.do?SERIES_KEY=320.MNA.Q.Y.FR.W2.S1.S1.B.B1GQ._Z._Z._Z.XDC.LR.GY&type=xls"
dest_file <- "France-real-growth-yy.csv"
download.file(url, dest_file,quite = TRUE)

url <- "https://sdw.ecb.europa.eu/quickviewexport.do?SERIES_KEY=320.MNA.Q.Y.IT.W2.S1.S1.B.B1GQ._Z._Z._Z.XDC.LR.GY&type=xls"
dest_file <- "Italy-real-growth-yy.csv"
download.file(url, dest_file,quite = TRUE)

url <- "https://sdw.ecb.europa.eu/quickviewexport.do?SERIES_KEY=320.MNA.Q.Y.ES.W2.S1.S1.B.B1GQ._Z._Z._Z.XDC.LR.GY&type=xls"
dest_file <- "Spain-real-growth-yy.csv"
download.file(url, dest_file,quite = TRUE)

url <- "https://sdw.ecb.europa.eu/quickviewexport.do?SERIES_KEY=122.ICP.M.DE.N.XEF000.4.ANR&type=xls"
dest_file <- "Germany-inflation.csv"
download.file(url, dest_file,quite = TRUE)

url <- "https://sdw.ecb.europa.eu/quickviewexport.do?SERIES_KEY=122.ICP.M.FR.N.XEF000.4.ANR&type=xls"
dest_file <- "France-inflation.csv"
download.file(url, dest_file,quite = TRUE)

url <- "https://sdw.ecb.europa.eu/quickviewexport.do?SERIES_KEY=122.ICP.M.IT.N.XEF000.4.ANR&type=xls"
dest_file <- "Italy-inflation.csv"
download.file(url, dest_file,quite = TRUE)

url <- "https://sdw.ecb.europa.eu/quickviewexport.do?SERIES_KEY=122.ICP.M.ES.N.XEF000.4.ANR&type=xls"
dest_file <- "Spain-inflation.csv"
download.file(url, dest_file,quite = TRUE)

url <- "https://fred.stlouisfed.org/graph/fredgraph.csv?bgcolor=%23e1e9f0&chart_type=line&drp=0&fo=open%20sans&graph_bgcolor=%23ffffff&height=450&mode=fred&recession_bars=off&txtcolor=%23444444&ts=12&tts=12&width=1318&nt=0&thu=0&trc=0&show_legend=yes&show_axis_titles=yes&show_tooltip=yes&id=PALLFNFINDEXQ&scale=left&cosd=2003-01-01&coed=2023-01-01&line_color=%234572a7&link_values=false&line_style=solid&mark_type=none&mw=3&lw=2&ost=-99999&oet=99999&mma=0&fml=a&fq=Quarterly&fam=avg&fgst=lin&fgsnd=2020-02-01&line_index=1&transformation=lin&vintage_date=2023-07-09&revision_date=2023-07-09&nd=2003-01-01"
dest_file <- "Commodity_Prices.csv"
download.file(url, dest_file,quite = TRUE)