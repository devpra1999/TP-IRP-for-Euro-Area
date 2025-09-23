#LONG TERM BOND DATA----------------------------------------------------------

# #Getting the data for German 10 year bonds
# Y_G_10 <- read.csv("Data_Files/Germany-long-bond.csv",
#                    skip = 6,col.names = c("Date","Yield","Type"), header = FALSE)
# Y_G_10$Date <- ym(Y_G_10$Date)
# Y_G_10 <- Y_G_10[order(Y_G_10$Date),]
# 
# 
# #Getting the data for French 10 year bonds
# Y_F_10 <- read.csv("Data_Files/France-long-bond.csv",
#                    skip = 6,col.names = c("Date","Yield","Type"), header = FALSE)
# Y_F_10$Date <- ym(Y_F_10$Date)
# Y_F_10 <- Y_F_10[order(Y_F_10$Date),]
# 
# 
# #Getting the data for Italian 10 year bonds
# Y_I_10 <- read.csv("Data_Files/Italy-long-bond.csv",
#                    skip = 6,col.names = c("Date","Yield","Type"), header = FALSE)
# Y_I_10$Date <- ym(Y_I_10$Date)
# Y_I_10 <- Y_I_10[order(Y_I_10$Date),]
# 
# 
# #Getting the data for Spanish 10 year bonds
# Y_S_10 <- read.csv("Data_Files/Spain-long-bond.csv",
#                    skip = 6,col.names = c("Date","Yield","Type"), header = FALSE)
# Y_S_10$Date <- ym(Y_S_10$Date)
# Y_S_10 <- Y_S_10[order(Y_S_10$Date),]

#Getting the data for German 10 year bonds
Y_G_10 <- read.csv("Data_Files/Germany_10Y_Q.csv", col.names = c("Date","Yield"))
Y_F_10 <- read.csv("Data_Files/France_10Y_Q.csv", col.names = c("Date","Yield"))
Y_I_10 <- read.csv("Data_Files/Italy_10Y_Q.csv", col.names = c("Date","Yield"))
Y_S_10 <- read.csv("Data_Files/Spain_10Y_Q.csv", col.names = c("Date","Yield"))

# data %>% mutate(year_quarter = lubridate::quarter(fecha, with_year = TRUE)) %>%
#   group_by(year_quarter, other_categories_to_summarize_by) %>%
#   summarize(exportaciones = sum(exportaciones))

#SHORT TERM BOND DATA-----------------------------------------------------------

#Getting data for short-term rates

#Market rates
Y_st <- read.csv("Data_Files/Short-term-rates.csv") %>% select(TIME_PERIOD,OBS_VALUE)
colnames(Y_st) <- c("Date","Rate")
Y_st$Date <- as.Date(Y_st$Date)
Y_st$Rate <- as.numeric(Y_st$Rate)
Y_st$month <- floor_date(Y_st$Date,"month")
Y_st <- Y_st %>% group_by(month) %>% summarize(mean = mean(Rate,na.rm = TRUE))
colnames(Y_st) <- c("Date","Rate")
#Y_st$Date <- ym(Y_st$Date)
Y_st <- Y_st[order(Y_st$Date),]

#Official rates
#Y_st <- read.csv("./Data_Files/ECB-rates.csv", skip = 6
#                 , col.names = c("Date","Rate","Type"))
#Y_st$Date <- ymd(Y_st$Date)


#CONSENSUS FORECASTS DATA--------------------------------------------------------

#The following file contains the forecasts where target period ends 3 months after the survey
#The value for a given quarter refers to the forecast made 3 months (1 quarter) before
f_3 <- read.csv("Data_Files/3-months.csv")%>% select(TIME_PERIOD,OBS_VALUE)
colnames(f_3) <- c("Date","Q1_forecast")
#Order by date and convert quarterly data into date format
f_3$Date = as.Date(as.yearqtr(f_3$Date, format = "%Y-Q%q")) - months(1)
f_3 <- f_3[order(f_3$Date),]

#Repeat for 6,9 and 12 month ahead forecasts
f_6 <- read.csv("Data_Files/6-months.csv")%>% select(TIME_PERIOD,OBS_VALUE)
colnames(f_6) <- c("Date","Q2_forecast")
f_6$Date = as.Date(as.yearqtr(f_6$Date, format = "%Y-Q%q")) - months(4)
f_6 <- f_6[order(f_6$Date),]

f_9 <- read.csv("Data_Files/9-months.csv") %>% select(TIME_PERIOD, OBS_VALUE)
colnames(f_9) <- c("Date", "Q3_forecast")
f_9$Date <- as.Date(as.yearqtr(f_9$Date, format = "%Y-Q%q")) - months(7)
f_9 <- f_9[order(f_9$Date),]


f_12 <- read.csv("Data_Files/12-months.csv") %>% select(TIME_PERIOD, OBS_VALUE)
colnames(f_12) <- c("Date", "Q4_forecast")
f_12$Date <- as.Date(as.yearqtr(f_12$Date, format = "%Y-Q%q")) - months(10)
f_12 <- f_12[order(f_12$Date),]


#ANNUAL DATA NOW
# Read and process 15-months.csv
f_15 <- read.csv("Data_Files/15-months.csv") %>% select(TIME_PERIOD, OBS_VALUE)
colnames(f_15) <- c("Date", "Q5_forecast")
f_15 <- f_15[order(f_15$Date),]
f_15$Date <- as.Date(as.yearqtr(f_15$Date, format = "%Y")) - months(4)

# Read and process 18-months.csv
f_18 <- read.csv("Data_Files/18-months.csv") %>% select(TIME_PERIOD, OBS_VALUE)
colnames(f_18) <- c("Date", "Q6_forecast")
f_18 <- f_18[order(f_18$Date),]
f_18$Date <- as.Date(as.yearqtr(f_18$Date, format = "%Y")) - months(7)

# Read and process 21-months.csv
f_21 <- read.csv("Data_Files/21-months.csv") %>% select(TIME_PERIOD, OBS_VALUE)
colnames(f_21) <- c("Date", "Q7_forecast")
f_21 <- f_21[order(f_21$Date),]
f_21$Date <- as.Date(as.yearqtr(f_21$Date, format = "%Y")) - months(10)

# Read and process 24-months.csv
f_24 <- read.csv("Data_Files/24-months.csv") %>% select(TIME_PERIOD, OBS_VALUE)
colnames(f_24) <- c("Date", "Q8_forecast")
f_24 <- f_24[order(f_24$Date),]
f_24$Date <- as.Date(as.yearqtr(f_24$Date, format = "%Y")) - months(13)

f_27 <- read.csv("Data_Files/27-months.csv") %>% select(TIME_PERIOD, OBS_VALUE)
colnames(f_27) <- c("Date", "Q9_forecast")
f_27 <- f_27[order(f_27$Date),]
f_27$Date <- as.Date(as.yearqtr(f_27$Date, format = "%Y")) - months(16)

# Read and process 30-months.csv
f_30 <- read.csv("Data_Files/30-months.csv") %>% select(TIME_PERIOD, OBS_VALUE)
colnames(f_30) <- c("Date", "Q10_forecast")
f_30 <- f_30[order(f_30$Date),]
f_30$Date <- as.Date(as.yearqtr(f_30$Date, format = "%Y")) - months(19)

# Read and process 33-months.csv
f_33 <- read.csv("Data_Files/33-months.csv") %>% select(TIME_PERIOD, OBS_VALUE)
colnames(f_33) <- c("Date", "Q11_forecast")
f_33 <- f_33[order(f_33$Date),]
f_33$Date <- as.Date(as.yearqtr(f_33$Date, format = "%Y")) - months(22)

# Read and process 36-months.csv
f_36 <- read.csv("Data_Files/36-months.csv") %>% select(TIME_PERIOD, OBS_VALUE)
colnames(f_36) <- c("Date", "Q12_forecast")
f_36 <- f_36[order(f_36$Date),]
f_36$Date <- as.Date(as.yearqtr(f_36$Date, format = "%Y")) - months(25)



#MACROECONOMIC DATA------------------------------------------------------------------------
#GDP
# gdp_dat <- read.csv("Data_Files/gdp_data.csv")
# gdp_dat <- gdp_dat %>% dplyr::select(geo,TIME_PERIOD,OBS_VALUE)
# colnames(gdp_dat) <- c("Country","Date","GDP")
# gdp_dat$Date <- as.Date(as.yearqtr(gdp_dat$Date, format = "%Y-Q%q")) + months(3)
# gdp_dat <- gdp_dat[order(gdp_dat$Date),]
# 
# Germany_ga <- read.csv("Data_Files/Germany-real-growth-yy.csv", skip = 6, header = FALSE)
# colnames(Germany_ga)[1:2] <- c("Date","Growth_a")
# Germany_ga <- Germany_ga %>%
#   select(Date,Growth_a) %>%
#   mutate(Date = as.Date(as.yearqtr(Date, format = "%YQ%q")) + months(3), Growth_a = as.numeric(Growth_a)) %>%
#   filter(Date >= "2010-04-01") %>%
#   arrange(Date)
# 
# France_ga <- read.csv("Data_Files/France-real-growth-yy.csv", skip = 6, header = FALSE)
# colnames(France_ga)[1:2] <- c("Date", "Growth_a")
# France_ga <- France_ga %>%
#   select(Date, Growth_a) %>%
#   mutate(Date = as.Date(as.yearqtr(Date, format = "%YQ%q")) + months(3), Growth_a = as.numeric(Growth_a)) %>%
#   filter(Date >= "2010-04-01") %>%
#   arrange(Date)
# 
# Italy_ga <- read.csv("Data_Files/Italy-real-growth-yy.csv", skip = 6, header = FALSE)
# colnames(Italy_ga)[1:2] <- c("Date", "Growth_a")
# Italy_ga <- Italy_ga %>%
#   select(Date, Growth_a) %>%
#   mutate(Date = as.Date(as.yearqtr(Date, format = "%YQ%q")) + months(3), Growth_a = as.numeric(Growth_a)) %>%
#   filter(Date >= "2010-04-01") %>%
#   arrange(Date)
# 
# Spain_ga <- read.csv("Data_Files/Spain-real-growth-yy.csv", skip = 6, header = FALSE)
# colnames(Spain_ga)[1:2] <- c("Date", "Growth_a")
# Spain_ga <- Spain_ga %>%
#   select(Date, Growth_a) %>%
#   mutate(Date = as.Date(as.yearqtr(Date, format = "%YQ%q")) + months(3), Growth_a = as.numeric(Growth_a)) %>%
#   filter(Date >= "2010-04-01") %>%
#   arrange(Date)
# 
# Germany_gq <- read.csv("Data_Files/Germany-real-growth-qq.csv", skip = 6, header = FALSE)
# colnames(Germany_gq)[1:2] <- c("Date","Growth_q")
# Germany_gq <- Germany_gq %>%
#   select(Date,Growth_q) %>%
#   mutate(Date = as.Date(as.yearqtr(Date, format = "%YQ%q")) + months(3), Growth_q = as.numeric(Growth_q)) %>%
#   filter(Date >= "2010-04-01") %>%
#   arrange(Date)
# 
# France_gq <- read.csv("Data_Files/France-real-growth-qq.csv", skip = 6, header = FALSE)
# colnames(France_gq)[1:2] <- c("Date", "Growth_q")
# France_gq <- France_gq %>%
#   select(Date, Growth_q) %>%
#   mutate(Date = as.Date(as.yearqtr(Date, format = "%YQ%q")) + months(3), Growth_q = as.numeric(Growth_q)) %>%
#   filter(Date >= "2010-04-01") %>%
#   arrange(Date)
# 
# Italy_gq <- read.csv("Data_Files/Italy-real-growth-qq.csv", skip = 6, header = FALSE)
# colnames(Italy_gq)[1:2] <- c("Date", "Growth_q")
# Italy_gq <- Italy_gq %>%
#   select(Date, Growth_q) %>%
#   mutate(Date = as.Date(as.yearqtr(Date, format = "%YQ%q")) + months(3), Growth_q = as.numeric(Growth_q)) %>%
#   filter(Date >= "2010-04-01") %>%
#   arrange(Date)
# 
# Spain_gq <- read.csv("Data_Files/Spain-real-growth-qq.csv", skip = 6, header = FALSE)
# colnames(Spain_gq)[1:2] <- c("Date", "Growth_q")
# Spain_gq <- Spain_gq %>%
#   select(Date, Growth_q) %>%
#   mutate(Date = as.Date(as.yearqtr(Date, format = "%YQ%q")) + months(3), Growth_q = as.numeric(Growth_q)) %>%
#   filter(Date >= "2010-04-01") %>%
#   arrange(Date)
# 
# 
# #Inflation
# Germany_inf <- read.csv("Data_Files/Germany-inflation.csv", skip = 6, header = FALSE)
# colnames(Germany_inf)[1:2] <- c("Date","Inflation")
# Germany_inf <- Germany_inf %>%
#   select(Date,Inflation) %>%
#   mutate(Date = ym(Date), Inflation = as.numeric(Inflation)) %>%
#   filter(Date >= "2010-04-01") %>%
#   arrange(Date)
# #Germany_inf$Date <- ym(Germany_inf$Date)
# #Germany_inf <- Germany_inf[order(Germany_inf$Date),]
# 
# France_inf <- read.csv("Data_Files/France-inflation.csv", skip = 6, header = FALSE)
# colnames(France_inf)[1:2] <- c("Date","Inflation")
# France_inf <- France_inf %>%
#   select(Date,Inflation) %>%
#   mutate(Date = ym(Date), Inflation = as.numeric(Inflation)) %>%
#   filter(Date >= "2010-04-01") %>%
#   arrange(Date)
# 
# Italy_inf <- read.csv("Data_Files/Italy-inflation.csv", skip = 6, header = FALSE)
# colnames(Italy_inf)[1:2] <- c("Date","Inflation")
# Italy_inf <- Italy_inf %>%
#   select(Date,Inflation) %>%
#   mutate(Date = ym(Date), Inflation = as.numeric(Inflation)) %>%
#   filter(Date >= "2010-04-01") %>%
#   arrange(Date)
# 
# Spain_inf <- read.csv("Data_Files/Spain-inflation.csv", skip = 6, header = FALSE)
# colnames(Spain_inf)[1:2] <- c("Date","Inflation")
# Spain_inf <- Spain_inf %>%
#   select(Date,Inflation) %>%
#   mutate(Date = ym(Date), Inflation = as.numeric(Inflation)) %>%
#   filter(Date >= "2010-04-01") %>%
#   arrange(Date)
# 
# Germany_inf_q <- read.csv("Data_Files/Germany-inflation-qq.csv", skip = 6, header = FALSE)
# colnames(Germany_inf_q)[1:2] <- c("Date","Inflation_QQ")
# Germany_inf_q <- Germany_inf_q %>%
#   select(Date,Inflation_QQ) %>%
#   mutate(Date = ymd(Date), Inflation_QQ = as.numeric(Inflation_QQ)) %>%
#   filter(Date >= "2010-04-01") %>%
#   arrange(Date)
# #Germany_inf$Date <- ym(Germany_inf$Date)
# #Germany_inf <- Germany_inf[order(Germany_inf$Date),]
# 
# France_inf_q <- read.csv("Data_Files/France-inflation-qq.csv", skip = 6, header = FALSE)
# colnames(France_inf_q)[1:2] <- c("Date","Inflation_QQ")
# France_inf_q <- France_inf_q %>%
#   select(Date,Inflation_QQ) %>%
#   mutate(Date = ymd(Date), Inflation_QQ = as.numeric(Inflation_QQ)) %>%
#   filter(Date >= "2010-04-01") %>%
#   arrange(Date)
# 
# Italy_inf_q <- read.csv("Data_Files/Italy-inflation-qq.csv", skip = 6, header = FALSE)
# colnames(Italy_inf_q)[1:2] <- c("Date","Inflation_QQ")
# Italy_inf_q <- Italy_inf_q %>%
#   select(Date,Inflation_QQ) %>%
#   mutate(Date = ymd(Date), Inflation_QQ = as.numeric(Inflation_QQ)) %>%
#   filter(Date >= "2010-04-01") %>%
#   arrange(Date)
# 
# Spain_inf_q <- read.csv("Data_Files/Spain-inflation-qq.csv", skip = 6, header = FALSE)
# colnames(Spain_inf_q)[1:2] <- c("Date","Inflation_QQ")
# Spain_inf_q <- Spain_inf_q %>%
#   select(Date,Inflation_QQ) %>%
#   mutate(Date = ymd(Date), Inflation_QQ = as.numeric(Inflation_QQ)) %>%
#   filter(Date >= "2010-04-01") %>%
#   arrange(Date)
# 
# Global_CP <- read.csv("Data_Files/Commodity_Prices.csv")
# colnames(Global_CP)[1:2] <- c("Date","CP_Index")
# Global_CP <- Global_CP %>%
#   select(Date,CP_Index) %>%
#   mutate(Date = ymd(Date), CP_Index = as.numeric(CP_Index)) %>%
#   filter(Date >= "2010-04-01") %>%
#   arrange(Date)
# 
# Global_CP_QQ <- read.csv("Data_Files/GCP-QQ.csv")
# colnames(Global_CP_QQ)[1:2] <- c("Date","GCP_QQ")
# Global_CP_QQ <- Global_CP_QQ %>%
#   select(Date,GCP_QQ) %>%
#   mutate(Date = ymd(Date), GCP_QQ = as.numeric(GCP_QQ)) %>%
#   filter(Date >= "2010-04-01") %>%
#   arrange(Date)
# Global_CP <- merge(Global_CP, Global_CP_QQ, by = "Date")
# 
# Global_CP_YY <- read.csv("Data_Files/GCP-YY.csv")
# colnames(Global_CP_YY)[1:2] <- c("Date","GCP_YY")
# Global_CP_YY <- Global_CP_YY %>%
#   select(Date,GCP_YY) %>%
#   mutate(Date = ymd(Date), GCP_YY = as.numeric(GCP_YY)) %>%
#   filter(Date >= "2010-04-01") %>%
#   arrange(Date)
# Global_CP <- merge(Global_CP, Global_CP_YY, by = "Date")

#NSS parameters
Germany_NSS <- read.csv("Data_Files/Germany-NSS-params.csv",skip = 5)
Germany_NSS <- Germany_NSS %>% select_if(~ !any(is.na(.))) 
colnames(Germany_NSS) <- c("Date","Beta0","Beta1","Beta2","Beta3","Tau1","Tau2")
Germany_NSS <- Germany_NSS %>% mutate(Date = ym(Date)) %>% filter(Date >= "2000-01-01") %>% filter(Date <= "2020-01-01")

# US_NSS <- read.csv("Data_Files/USnss.csv")
# colnames(US_NSS) <- c("Date","Beta0","Beta1","Beta2","Beta3","Tau1","Tau2")
# US_NSS$Date <- ymd(US_NSS$Date)
# US_NSS <- US_NSS %>%
#   mutate(year = year(Date), month = month(Date)) %>%
#   group_by(year, month) %>%
#   filter(row_number() == 1) %>%
#   ungroup() %>%
#   select(-year,-month)
# US_NSS$Date <- floor_date(US_NSS$Date, "month")
