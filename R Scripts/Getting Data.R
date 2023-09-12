#LONG TERM BOND DATA----------------------------------------------------------
#Getting the data for German 10 year bonds
url <- "https://sdw.ecb.europa.eu/quickviewexport.do;jsessionid=99AB5C95BA2BC70BAEB76C2AF91DA3AD?SERIES_KEY=229.IRS.M.DE.L.L40.CI.0000.EUR.N.Z&type=xls"
dest_file <- "Data_Files/Germany-long-bond.csv"
download.file(url, dest_file,quite = TRUE)

#Getting the data for French 10 year bonds
url <- "https://sdw.ecb.europa.eu/quickviewexport.do;jsessionid=1D71340A49B14ABB4F5F8DBFF7E352A6?SERIES_KEY=229.IRS.M.FR.L.L40.CI.0000.EUR.N.Z&type=xls"
dest_file <- "Data_Files/France-long-bond.csv"
download.file(url, dest_file,quite = TRUE)

#Getting the data for Italian 10 year bonds
url <- "https://sdw.ecb.europa.eu/quickviewexport.do;jsessionid=7623896DA72569BAD7559B496FC7B68C?SERIES_KEY=229.IRS.M.IT.L.L40.CI.0000.EUR.N.Z&type=xls"
dest_file <- "Data_Files/Italy-long-bond.csv"
download.file(url, dest_file,quite = TRUE)

#Getting the data for Spanish 10 year bonds
url <- "https://sdw.ecb.europa.eu/quickviewexport.do;jsessionid=E6744786D25FD0F868A4ABA7695FE13C?SERIES_KEY=229.IRS.M.ES.L.L40.CI.0000.EUR.N.Z&type=xls"
dest_file <- "Data_Files/Spain-long-bond.csv"
download.file(url, dest_file,quite = TRUE)


#SHORT TERM BOND DATA-----------------------------------------------------------
#Market rates
url <- "https://ec.europa.eu/eurostat/api/dissemination/sdmx/2.1/data/IRT_ST_M/?format=SDMX-CSV"
dest_file <- "Data_Files/Short-term-rates.csv"
download.file(url, dest_file,quite = TRUE)

#Official rates
#Y_st <- read.csv("./Data_Files/ECB-rates.csv", skip = 6
#                 , col.names = c("Date","Rate","Type"))
#Y_st$Date <- ymd(Y_st$Date)


#CONSENSUS FORECASTS DATA--------------------------------------------------------

#The following file contains the forecasts where target period ends 3 months after the survey
#The value for a given quarter refers to the forecast made 3 months (1 quarter) before
url <- "https://sdw.ecb.europa.eu/quickviewexport.do;jsessionid=4BFB50DC40CA72B025B1F33EF9F27E96?SERIES_KEY=138.SPF.Q.U2.ASSU.IR.P3M.Q.AVG&type=xls"
dest_file <- "Data_Files/3-months.csv"
download.file(url, dest_file,quite = TRUE)

#Repeat for 6,9 and 12 month ahead forecasts
url <- "https://sdw.ecb.europa.eu/quickviewexport.do;jsessionid=CEF3E31DC14E94FA19F368B5DEE9297F?SERIES_KEY=138.SPF.Q.U2.ASSU.IR.P6M.Q.AVG&type=xls"
dest_file <- "Data_Files/6-months.csv"
download.file(url, dest_file,quite = TRUE)

url <- "https://sdw.ecb.europa.eu/quickviewexport.do;jsessionid=D8248552EA7D6E8D68F21D26D5A436AB?SERIES_KEY=138.SPF.Q.U2.ASSU.IR.P9M.Q.AVG&type=xls"
dest_file <- "Data_Files/9-months.csv"
download.file(url, dest_file,quite = TRUE)

url <- "https://sdw.ecb.europa.eu/quickviewexport.do;jsessionid=EF4872E8D06766518C57CA6448EA62FD?SERIES_KEY=138.SPF.Q.U2.ASSU.IR.P12M.Q.AVG&type=xls"
dest_file <- "Data_Files/12-months.csv"
download.file(url, dest_file,quite = TRUE)

url <- "https://sdw.ecb.europa.eu/quickviewexport.do;jsessionid=A65A8B3F145A81B6FB2A08389F4DE5A3?SERIES_KEY=138.SPF.A.U2.ASSU.IR.P15M.Q.AVG&type=xls"
dest_file <- "Data_Files/15-months.csv"
download.file(url, dest_file,quite = TRUE)

url <- "https://sdw.ecb.europa.eu/quickviewexport.do;jsessionid=B521A9FF56061B8B63225B7A5FF93C03?SERIES_KEY=138.SPF.A.U2.ASSU.IR.P18M.Q.AVG&type=xls"
dest_file <- "Data_Files/18-months.csv"
download.file(url, dest_file,quite = TRUE)

url <- "https://sdw.ecb.europa.eu/quickviewexport.do;jsessionid=7F56F3AF9BD7F15959D1110DD46CDD55?SERIES_KEY=138.SPF.A.U2.ASSU.IR.P21M.Q.AVG&type=xls"
dest_file <- "Data_Files/21-months.csv"
download.file(url, dest_file,quite = TRUE)

url <- "https://sdw.ecb.europa.eu/quickviewexport.do;jsessionid=1CC9F510DEEF4BEAEC929142B89FD8C6?SERIES_KEY=138.SPF.A.U2.ASSU.IR.P24M.Q.AVG&type=xls"
dest_file <- "Data_Files/24-months.csv"
download.file(url, dest_file,quite = TRUE)

url <- "https://sdw.ecb.europa.eu/quickviewexport.do;jsessionid=5491FE489D7777018439FBB97062684A?SERIES_KEY=138.SPF.A.U2.ASSU.IR.P27M.Q.AVG&type=xls"
dest_file <- "Data_Files/27-months.csv"
download.file(url, dest_file,quite = TRUE)

url <- "https://sdw.ecb.europa.eu/quickviewexport.do;jsessionid=2E93713EFCB0EA007428CB34FB122869?SERIES_KEY=138.SPF.A.U2.ASSU.IR.P30M.Q.AVG&type=xls"
dest_file <- "Data_Files/30-months.csv"
download.file(url, dest_file,quite = TRUE)

url <- "https://sdw.ecb.europa.eu/quickviewexport.do;jsessionid=2B8CD0FB7E8212F80FA7F7137489F0E3?SERIES_KEY=138.SPF.A.U2.ASSU.IR.P33M.Q.AVG&type=xls"
dest_file <- "Data_Files/33-months.csv"
download.file(url, dest_file,quite = TRUE)

url <- "https://sdw.ecb.europa.eu/quickviewexport.do;jsessionid=88E02E1EEB11AC119CEAF714FB14438D?SERIES_KEY=138.SPF.A.U2.ASSU.IR.P36M.Q.AVG&type=xls"
dest_file <- "Data_Files/36-months.csv"
download.file(url, dest_file,quite = TRUE)


#Macroeconomic Variables

url <- "https://ec.europa.eu/eurostat/api/dissemination/sdmx/2.1/data/NAMQ_10_GDP/Q.CP_MEUR.SCA.B1GQ.DE+ES+FR+IT/?format=SDMX-CSV&startPeriod=1999-Q4&endPeriod=2023-Q1"
dest_file <- "Data_Files/gdp_data.csv"
download.file(url,dest_file,quite = TRUE)

url <- "https://sdw.ecb.europa.eu/quickviewexport.do?SERIES_KEY=320.MNA.Q.Y.DE.W2.S1.S1.B.B1GQ._Z._Z._Z.XDC.LR.GY&type=xls"
dest_file <- "Data_Files/Germany-real-growth-yy.csv"
download.file(url, dest_file,quite = TRUE)

url <- "https://sdw.ecb.europa.eu/quickviewexport.do?SERIES_KEY=320.MNA.Q.Y.FR.W2.S1.S1.B.B1GQ._Z._Z._Z.XDC.LR.GY&type=xls"
dest_file <- "Data_Files/France-real-growth-yy.csv"
download.file(url, dest_file,quite = TRUE)

url <- "https://sdw.ecb.europa.eu/quickviewexport.do?SERIES_KEY=320.MNA.Q.Y.IT.W2.S1.S1.B.B1GQ._Z._Z._Z.XDC.LR.GY&type=xls"
dest_file <- "Data_Files/Italy-real-growth-yy.csv"
download.file(url, dest_file,quite = TRUE)

url <- "https://sdw.ecb.europa.eu/quickviewexport.do?SERIES_KEY=320.MNA.Q.Y.ES.W2.S1.S1.B.B1GQ._Z._Z._Z.XDC.LR.GY&type=xls"
dest_file <- "Data_Files/Spain-real-growth-yy.csv"
download.file(url, dest_file,quite = TRUE)

url <- "https://sdw.ecb.europa.eu/quickviewexport.do?SERIES_KEY=320.MNA.Q.Y.DE.W2.S1.S1.B.B1GQ._Z._Z._Z.XDC.LR.G1&type=xls"
dest_file <- "Data_Files/Germany-real-growth-qq.csv"
download.file(url, dest_file,quite = TRUE)

url <- "https://sdw.ecb.europa.eu/quickviewexport.do?SERIES_KEY=320.MNA.Q.Y.FR.W2.S1.S1.B.B1GQ._Z._Z._Z.XDC.LR.G1&type=xls"
dest_file <- "Data_Files/France-real-growth-qq.csv"
download.file(url, dest_file,quite = TRUE)

url <- "https://sdw.ecb.europa.eu/quickviewexport.do?SERIES_KEY=320.MNA.Q.Y.IT.W2.S1.S1.B.B1GQ._Z._Z._Z.XDC.LR.G1&type=xls"
dest_file <- "Data_Files/Italy-real-growth-qq.csv"
download.file(url, dest_file,quite = TRUE)

url <- "https://sdw.ecb.europa.eu/quickviewexport.do?SERIES_KEY=320.MNA.Q.Y.ES.W2.S1.S1.B.B1GQ._Z._Z._Z.XDC.LR.G1&type=xls"
dest_file <- "Data_Files/Spain-real-growth-qq.csv"
download.file(url, dest_file,quite = TRUE)

url <- "https://sdw.ecb.europa.eu/quickviewexport.do?SERIES_KEY=122.ICP.M.DE.N.000000.4.ANR&type=xls"
dest_file <- "Data_Files/Germany-inflation.csv"
download.file(url, dest_file,quite = TRUE)

url <- "https://sdw.ecb.europa.eu/quickviewexport.do?SERIES_KEY=122.ICP.M.FR.N.000000.4.ANR&type=xls"
dest_file <- "Data_Files/France-inflation.csv"
download.file(url, dest_file,quite = TRUE)

url <- "https://sdw.ecb.europa.eu/quickviewexport.do?SERIES_KEY=122.ICP.M.IT.N.000000.4.ANR&type=xls"
dest_file <- "Data_Files/Italy-inflation.csv"
download.file(url, dest_file,quite = TRUE)

url <- "https://sdw.ecb.europa.eu/quickviewexport.do?SERIES_KEY=122.ICP.M.ES.N.000000.4.ANR&type=xls"
dest_file <- "Data_Files/Spain-inflation.csv"
download.file(url, dest_file,quite = TRUE)

url <- "https://fred.stlouisfed.org/graph/fredgraph.csv?bgcolor=%23e1e9f0&chart_type=line&drp=0&fo=open%20sans&graph_bgcolor=%23ffffff&height=450&mode=fred&recession_bars=off&txtcolor=%23444444&ts=12&tts=12&width=1318&nt=0&thu=0&trc=0&show_legend=yes&show_axis_titles=yes&show_tooltip=yes&id=PALLFNFINDEXQ&scale=left&cosd=2003-01-01&coed=2023-01-01&line_color=%234572a7&link_values=false&line_style=solid&mark_type=none&mw=3&lw=2&ost=-99999&oet=99999&mma=0&fml=a&fq=Quarterly&fam=avg&fgst=lin&fgsnd=2020-02-01&line_index=1&transformation=lin&vintage_date=2023-07-09&revision_date=2023-07-09&nd=2003-01-01"
dest_file <- "Data_Files/Commodity_Prices.csv"
download.file(url, dest_file,quite = TRUE)

url <- "https://fred.stlouisfed.org/graph/fredgraph.csv?bgcolor=%23e1e9f0&chart_type=line&drp=0&fo=open%20sans&graph_bgcolor=%23ffffff&height=450&mode=fred&recession_bars=off&txtcolor=%23444444&ts=12&tts=12&width=1318&nt=0&thu=0&trc=0&show_legend=yes&show_axis_titles=yes&show_tooltip=yes&id=CPALTT01DEQ657N&scale=left&cosd=2000-01-01&coed=2023-01-01&line_color=%234572a7&link_values=false&line_style=solid&mark_type=none&mw=3&lw=2&ost=-99999&oet=99999&mma=0&fml=a&fq=Quarterly&fam=avg&fgst=lin&fgsnd=2020-02-01&line_index=1&transformation=lin&vintage_date=2023-07-16&revision_date=2023-07-16&nd=2000-01-01"
dest_file <- "Data_Files/Germany-inflation-qq.csv"
download.file(url, dest_file,quite = TRUE)

url <- "https://fred.stlouisfed.org/graph/fredgraph.csv?bgcolor=%23e1e9f0&chart_type=line&drp=0&fo=open%20sans&graph_bgcolor=%23ffffff&height=450&mode=fred&recession_bars=off&txtcolor=%23444444&ts=12&tts=12&width=1318&nt=0&thu=0&trc=0&show_legend=yes&show_axis_titles=yes&show_tooltip=yes&id=CPALTT01FRQ657N&scale=left&cosd=2000-01-01&coed=2023-01-01&line_color=%234572a7&link_values=false&line_style=solid&mark_type=none&mw=3&lw=2&ost=-99999&oet=99999&mma=0&fml=a&fq=Quarterly&fam=avg&fgst=lin&fgsnd=2020-02-01&line_index=1&transformation=lin&vintage_date=2023-07-16&revision_date=2023-07-16&nd=1960-01-01"
dest_file <- "Data_Files/France-inflation-qq.csv"
download.file(url, dest_file,quite = TRUE)

url <- "https://fred.stlouisfed.org/graph/fredgraph.csv?bgcolor=%23e1e9f0&chart_type=line&drp=0&fo=open%20sans&graph_bgcolor=%23ffffff&height=450&mode=fred&recession_bars=off&txtcolor=%23444444&ts=12&tts=12&width=1318&nt=0&thu=0&trc=0&show_legend=yes&show_axis_titles=yes&show_tooltip=yes&id=CPALTT01ITQ657N&scale=left&cosd=2000-01-01&coed=2023-01-01&line_color=%234572a7&link_values=false&line_style=solid&mark_type=none&mw=3&lw=2&ost=-99999&oet=99999&mma=0&fml=a&fq=Quarterly&fam=avg&fgst=lin&fgsnd=2020-02-01&line_index=1&transformation=lin&vintage_date=2023-07-16&revision_date=2023-07-16&nd=2000-01-01"
dest_file <- "Data_Files/Italy-inflation-qq.csv"
download.file(url, dest_file,quite = TRUE)

url <- "https://fred.stlouisfed.org/graph/fredgraph.csv?bgcolor=%23e1e9f0&chart_type=line&drp=0&fo=open%20sans&graph_bgcolor=%23ffffff&height=450&mode=fred&recession_bars=off&txtcolor=%23444444&ts=12&tts=12&width=1318&nt=0&thu=0&trc=0&show_legend=yes&show_axis_titles=yes&show_tooltip=yes&id=CPALTT01ESQ657N&scale=left&cosd=2000-01-01&coed=2023-01-01&line_color=%234572a7&link_values=false&line_style=solid&mark_type=none&mw=3&lw=2&ost=-99999&oet=99999&mma=0&fml=a&fq=Quarterly&fam=avg&fgst=lin&fgsnd=2020-02-01&line_index=1&transformation=lin&vintage_date=2023-07-16&revision_date=2023-07-16&nd=2000-01-01"
dest_file <- "Data_Files/Spain-inflation-qq.csv"
download.file(url, dest_file,quite = TRUE)

url <- "https://fred.stlouisfed.org/graph/fredgraph.csv?bgcolor=%23e1e9f0&chart_type=line&drp=0&fo=open%20sans&graph_bgcolor=%23ffffff&height=450&mode=fred&recession_bars=off&txtcolor=%23444444&ts=12&tts=12&width=1318&nt=0&thu=0&trc=0&show_legend=yes&show_axis_titles=yes&show_tooltip=yes&id=PALLFNFINDEXQ&scale=left&cosd=2003-01-01&coed=2023-01-01&line_color=%234572a7&link_values=false&line_style=solid&mark_type=none&mw=3&lw=2&ost=-99999&oet=99999&mma=0&fml=a&fq=Quarterly&fam=avg&fgst=lin&fgsnd=2020-02-01&line_index=1&transformation=pch&vintage_date=2023-07-16&revision_date=2023-07-16&nd=2003-01-01"
dest_file <- "Data_Files/GCP-QQ.csv"
download.file(url, dest_file,quite = TRUE)

url <- "https://fred.stlouisfed.org/graph/fredgraph.csv?bgcolor=%23e1e9f0&chart_type=line&drp=0&fo=open%20sans&graph_bgcolor=%23ffffff&height=450&mode=fred&recession_bars=off&txtcolor=%23444444&ts=12&tts=12&width=1318&nt=0&thu=0&trc=0&show_legend=yes&show_axis_titles=yes&show_tooltip=yes&id=PALLFNFINDEXQ&scale=left&cosd=2003-01-01&coed=2023-01-01&line_color=%234572a7&link_values=false&line_style=solid&mark_type=none&mw=3&lw=2&ost=-99999&oet=99999&mma=0&fml=a&fq=Quarterly&fam=avg&fgst=lin&fgsnd=2020-02-01&line_index=1&transformation=pc1&vintage_date=2023-07-16&revision_date=2023-07-16&nd=2003-01-01"
dest_file <- "Data_Files/GCP-YY.csv"
download.file(url, dest_file,quite = TRUE)

#NSS parameters
#BETA0
#Use Bundesbank API to get the parameter time series in text format
curl_command <- "curl -X 'GET' 'https://api.statistiken.bundesbank.de/rest/data/BBSIS/D.I.ZST.B0.EUR.S1311.B.A604._Z.R.A.A._Z._Z.A?startPeriod=2000-01-03&detail=dataonly' -H 'accept: text/csv'"
output <- system(curl_command, intern = TRUE)
#Convert into a dataframe
data <- strsplit(output, "\n")
data <- lapply(data, function(row) unlist(strsplit(row, ";")))
data_frame <- as.data.frame(t(as.data.frame(data,row.names = NULL)))
colnames(data_frame) <- data_frame[1,]
BETA0 <- data_frame[-1, ] %>% select("TIME_PERIOD","OBS_VALUE")
BETA0$OBS_VALUE <- as.numeric(BETA0$OBS_VALUE)
BETA0 <- BETA0 %>% fill(names(.),.direction = "down")
rownames(BETA0) <- NULL
colnames(BETA0) <- c("Date","BETA0")

#Repeat for the rest of the parameters
#BETA1
curl_command <- "curl -X 'GET' 'https://api.statistiken.bundesbank.de/rest/data/BBSIS/D.I.ZST.B1.EUR.S1311.B.A604._Z.R.A.A._Z._Z.A?startPeriod=2000-01-03&detail=dataonly' -H 'accept: text/csv'"
output <- system(curl_command, intern = TRUE)
data <- strsplit(output, "\n")
data <- lapply(data, function(row) unlist(strsplit(row, ";")))
data_frame <- as.data.frame(t(as.data.frame(data)))
colnames(data_frame) <- data_frame[1,]
BETA1 <- data_frame[-1, ] %>% select("TIME_PERIOD","OBS_VALUE")
BETA1$OBS_VALUE <- as.numeric(BETA1$OBS_VALUE)
BETA1 <- BETA1 %>% fill(names(.),.direction = "down")
rownames(BETA1) <- NULL
colnames(BETA1) <- c("Date","BETA1")

#BETA2
curl_command <- "curl -X 'GET' 'https://api.statistiken.bundesbank.de/rest/data/BBSIS/D.I.ZST.B2.EUR.S1311.B.A604._Z.R.A.A._Z._Z.A?startPeriod=2000-01-03&detail=dataonly' -H 'accept: text/csv'"
output <- system(curl_command, intern = TRUE)
data <- strsplit(output, "\n")
data <- lapply(data, function(row) unlist(strsplit(row, ";")))
data_frame <- as.data.frame(t(as.data.frame(data)))
colnames(data_frame) <- data_frame[1,]
BETA2 <- data_frame[-1, ] %>% select("TIME_PERIOD","OBS_VALUE")
BETA2$OBS_VALUE <- as.numeric(BETA2$OBS_VALUE)
BETA2 <- BETA2 %>% fill(names(.),.direction = "down")
rownames(BETA2) <- NULL
colnames(BETA2) <- c("Date","BETA2")

#BETA3
curl_command <- "curl -X 'GET' 'https://api.statistiken.bundesbank.de/rest/data/BBSIS/D.I.ZST.B3.EUR.S1311.B.A604._Z.R.A.A._Z._Z.A?startPeriod=2000-01-03&detail=dataonly' -H 'accept: text/csv'"
output <- system(curl_command, intern = TRUE)
data <- strsplit(output, "\n")
data <- lapply(data, function(row) unlist(strsplit(row, ";")))
data_frame <- as.data.frame(t(as.data.frame(data)))
colnames(data_frame) <- data_frame[1,]
BETA3 <- data_frame[-1, ] %>% select("TIME_PERIOD","OBS_VALUE")
BETA3$OBS_VALUE <- as.numeric(BETA3$OBS_VALUE)
BETA3 <- BETA3 %>% fill(names(.),.direction = "down")
rownames(BETA3) <- NULL
colnames(BETA3) <- c("Date","BETA3")
#TAU1
curl_command <- "curl -X 'GET' 'https://api.statistiken.bundesbank.de/rest/data/BBSIS/D.I.ZST.T1.EUR.S1311.B.A604._Z.R.A.A._Z._Z.A?startPeriod=2000-01-03&detail=dataonly' -H 'accept: text/csv'"
output <- system(curl_command, intern = TRUE)
data <- strsplit(output, "\n")
data <- lapply(data, function(row) unlist(strsplit(row, ";")))
data_frame <- as.data.frame(t(as.data.frame(data)))
colnames(data_frame) <- data_frame[1,]
TAU1 <- data_frame[-1, ] %>% select("TIME_PERIOD","OBS_VALUE")
TAU1$OBS_VALUE <- as.numeric(TAU1$OBS_VALUE)
TAU1 <- TAU1 %>% fill(names(.),.direction = "down")
rownames(TAU1) <- NULL
colnames(TAU1) <- c("Date","TAU1")

#TAU2
curl_command <- "curl -X 'GET' 'https://api.statistiken.bundesbank.de/rest/data/BBSIS/D.I.ZST.T2.EUR.S1311.B.A604._Z.R.A.A._Z._Z.A?startPeriod=2000-01-03&detail=dataonly' -H 'accept: text/csv'"
output <- system(curl_command, intern = TRUE)
data <- strsplit(output, "\n")
data <- lapply(data, function(row) unlist(strsplit(row, ";")))
data_frame <- as.data.frame(t(as.data.frame(data)))
colnames(data_frame) <- data_frame[1,]
TAU2 <- data_frame[-1, ] %>% select("TIME_PERIOD","OBS_VALUE")
TAU2$OBS_VALUE <- as.numeric(TAU2$OBS_VALUE)
TAU2 <- TAU2 %>% fill(names(.),.direction = "down")
rownames(TAU2) <- NULL
colnames(TAU2) <- c("Date","TAU2")

#Merge all the parameter data frames
df_list <- list(BETA0,BETA1,BETA2,BETA3,TAU1,TAU2)
NSS_params <- df_list %>% reduce(inner_join,"Date")
csv_file <- "Data_Files/gnss.csv"
write.csv(NSS_params, file = csv_file, sep = ";", row.names = FALSE)



