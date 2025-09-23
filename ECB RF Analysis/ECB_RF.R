#clear the environment
rm(list=ls())
# ------------------------------------------------------------------------
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
# -------Load/install packages--------------------------------------------
listofpackages = c("lubridate","zoo","readr","xts","ggplot2","dygraphs","plyr","dplyr",
                  "tidyverse","highcharter","readxl")
for (j in listofpackages){
 if(sum(installed.packages()[, 1] == j) == 0) {
   install.packages(j)
 }
 library(j, character.only = T, quietly = TRUE, warn.conflicts = FALSE)
}

####GET GERMANY YIELD CURVE----------------------------------------------------
N = 60
source("NSS.R")
L <- nss_yields("Data_Files/gnss.csv",N*3)
rawYields <- L$rawYields/100
plot_dates <- L$plot_dates
Yields <- as.matrix(rawYields)
Yields <- xts(Yields,plot_dates)
Yields <- Yields[seq(from = 3, to = 285, by = 3),seq(from = 3, to = N*3, length.out = N)]
colnames(Yields) <- paste("n",seq(1,N),sep = "_")
T = nrow(Yields)

####Get EA Yield Curve---------------------------------------------------------
L <- nss_yields("Data_Files/EA_YC_clean.csv",N*3)
rawYields <- L$rawYields/100
plot_dates <- L$plot_dates
nss_yields_EA.ts <- xts(rawYields,plot_dates)
nss_yields_EA.ts <- nss_yields_EA.ts[seq(from = 1, to = 237, by = 3),seq(from = 3, to = N*3, length.out = N)]
colnames(nss_yields_EA.ts) <- paste("n",seq(1,N),sep = "_")
T = nrow(nss_yields_EA.ts)

####Get US Yield Curve---------------------------------------------------------
#load('data/raw/yields1.rda')
load('yields1.rda')
nss_yields_US.ts <- nss_yields
rm(nss_yields)


rm(L,df,rawYields)


#Set Dates for analysis--------------------------------------------------------
current_dates_EA <- index(Yields)

####Get US lags--------------------------------------------------------------
US_1 <- lag.xts(nss_yields_US.ts$n_1[current_dates_EA],1)
colnames(US_1) <- "US_1"
US_2 <- lag.xts(nss_yields_US.ts$n_1[current_dates_EA],2)
colnames(US_2) <- "US_2"
US_3 <- lag.xts(nss_yields_US.ts$n_1[current_dates_EA],3)
colnames(US_3) <- "US_3"
US_4 <- lag.xts(nss_yields_US.ts$n_1[current_dates_EA],4)
colnames(US_4) <- "US_4"
Germany <- Yields$n_1[current_dates_EA]
colnames(Germany) <- "Germany"
Germany_1 <- lag.xts(Yields$n_1[current_dates_EA],1)
colnames(Germany_1) <- "Germany_1"


###### Get US macrodataset -----------------------------------------------------
#load('data/raw/rawdata1.rda')
load('rawdata1.rda')

# Inflation: π (pi)
rawdata.ts$LP     <- log(rawdata.ts$PCECTPI)
rawdata.ts$pi_US     <- (rawdata.ts$LP - lag.xts(rawdata.ts$LP, 4))
rawdata.ts$pi_gap <- rawdata.ts$pi_US - rawdata.ts$PTR/100

# Output gap: y_gap
rawdata.ts$y      <- log(rawdata.ts$GDPC1)
rawdata.ts$y_pot  <- log(rawdata.ts$GDPPOT)
rawdata.ts$y_gap_US  <- (rawdata.ts$y - rawdata.ts$y_pot)
rawdata.ts$dy_pot <- (rawdata.ts$y_pot - lag.xts(rawdata.ts$y_pot, 4))
data_US.ts <- rawdata.ts
rm(rawdata.ts)

US_macro = read_csv("macro_data.csv")
US_macro$date = floor_date(US_macro$date, unit = "month")
US_macro.ts = xts(US_macro[2:8],order.by = US_macro$date)
colnames(US_macro.ts)[3:4] = c("y_gap_US","pi_US")
rm(US_macro)


##########Get EA Macrodataset---------------------------------------------------
#data_EA <- read_csv("atsm_eu/data/data_gdp_and_inflation_US_EA.csv") %>% filter(country == "EA")
data_EA <- read_csv("Data_Files/AMECO6.csv") %>% filter(CNTRY == "EA19")
data_EA <- data_EA %>% filter(CODE=="OVGD" | CODE=="OVGDP")
data_EA <- data_EA[-2,12:ncol(data_EA)]
data_EA <- t(data_EA)
colnames(data_EA) <- c("GDPC1","GDPPOT")
data_EA <- as.data.frame(cbind(Date = rownames(data_EA), data_EA))
rownames(data_EA) <- seq(1,nrow(data_EA))
data_EA$Date <- lubridate::ymd(data_EA$Date,truncated = 2L)
data_EA <- na.omit(data_EA)
start_date <- data_EA$Date[1]
end_date <- data_EA$Date[nrow(data_EA)]
all_dates <- as.data.frame(seq(start_date,end_date,by="quarters"))
colnames(all_dates) <- c("Date")
data_EA <- merge(data_EA,all_dates, all.y = true())
data_EA$Date <- data_EA$Date + months(2)
data_EA$GDPC1 <- na.approx(data_EA$GDPC1)
data_EA$GDPPOT <- na.approx(data_EA$GDPPOT)
data_EA <- data_EA[-nrow(data_EA),]
rm(all_dates,start_date,end_date)
data_EA.ts <- xts(data_EA[,2:3],order.by = data_EA$Date)
rm(data_EA)
data_EA.ts$y      <- log(data_EA.ts$GDPC1)
data_EA.ts$y_pot  <- log(data_EA.ts$GDPPOT)
data_EA.ts$y_gap_EA  <- (data_EA.ts$y - data_EA.ts$y_pot)
data_EA.ts$dy_pot <- (data_EA.ts$y_pot - lag.xts(data_EA.ts$y_pot, 4))
#tmp <- read_csv("atsm_eu/data/data_gdp_and_inflation_US_EA.csv") %>% filter(country == "EA")
tmp <- read_csv("Data_Files/data_gdp_and_inflation_US_EA.csv") %>% filter(country == "EA")
tmp.ts <- xts(tmp[,2:ncol(tmp)], order.by = tmp$date)
tmp.ts <- tmp.ts$cpi
data_EA.ts <- merge.xts(data_EA.ts,tmp.ts[current_dates_EA,])
data_EA.ts$LP_EA    <- log(data_EA.ts$cpi)
data_EA.ts$pi_EA     <- (data_EA.ts$LP_EA - lag.xts(data_EA.ts$LP_EA, 4))
rm(tmp,tmp.ts)


####Get Fed Asset Growth--------------------------------
fed_assets <- read_csv("Fed Assets.csv")
colnames(fed_assets) <- c("Date","Assets")
tmp <- aggregate(Assets~quarter + year, transform(fed_assets, 
                                             quarter = quarters(Date), year = as.integer(format(Date, '%Y'))), mean)
tmp <- tmp[order(tmp$year), ]
tmp$Date <- paste0(tmp$year,tmp$quarter)
tmp <- tmp %>% select(Date,Assets)
tmp$Date = as.Date(as.yearqtr(tmp$Date, format = "%YQ%q")) + months(2)
fed_assets = tmp
fed_assets.ts <- xts(fed_assets$Assets,order.by = fed_assets$Date)
colnames(fed_assets.ts) <- c("Assets")
rm(tmp,fed_assets)
fed_assets.ts <- merge.xts(fed_assets.ts,data_US.ts$GDPC1, join = "inner")
fed_assets.ts$Asset_GDP_US <- fed_assets.ts$Assets/(1000*fed_assets.ts$GDPC1)


####Get ECB Asset Growth----------------------------------------
ecb_assets <- read_csv("ECB Assets.csv")
colnames(ecb_assets) <- c("Date","Assets")
tmp <- aggregate(Assets~quarter + year, transform(ecb_assets, 
                                                  quarter = quarters(Date), year = as.integer(format(Date, '%Y'))), mean)
tmp <- tmp[order(tmp$year), ]
tmp$Date <- paste0(tmp$year,tmp$quarter)
tmp <- tmp %>% select(Date,Assets)
tmp$Date = as.Date(as.yearqtr(tmp$Date, format = "%YQ%q")) + months(2)
ecb_assets = tmp
rm(tmp)
ecb_assets <- ecb_assets %>% mutate(Asset_Growth = (Assets/lag(Assets) - 1)*100)
ecb_assets.ts <- xts(ecb_assets[,2:3],order.by = ecb_assets$Date)
ecb_assets.ts <- merge.xts(ecb_assets.ts,data_EA.ts$GDPC1, join = "inner")
ecb_assets.ts$Asset_GDP_EA <- ecb_assets.ts$Assets/(1000*ecb_assets.ts$GDPC1)
assets_diff.ts <- ecb_assets.ts$Asset_GDP_EA - fed_assets.ts$Asset_GDP_US

#EXPECTED INFLATION AND GROWTH----------------------------------------
US_GI <- read_csv("Data_Files/USA_growth_inf.csv")
colnames(US_GI) <- c("Date","Growth_CY","Growth_NY","Inflation_CY","Inflation_NY")
US_GI$Growth_exp <- (1 - month(US_GI$Date)/12)*US_GI$Growth_CY + (month(US_GI$Date)/12)*US_GI$Growth_NY
US_GI$Inflation_exp <- (1 - month(US_GI$Date)/12)*US_GI$Inflation_CY + (month(US_GI$Date)/12)*US_GI$Inflation_NY
US_GI.ts <- xts(US_GI[2:7]/100, order.by = US_GI$Date)

EA_GI <- read_csv("Data_Files/Euro_Zone_growth_inf.csv")
colnames(EA_GI) <- c("Date","Growth_CY","Growth_NY","Inflation_CY","Inflation_NY")
EA_GI$Growth_exp <- (1 - month(EA_GI$Date)/12)*EA_GI$Growth_CY + (month(EA_GI$Date)/12)*EA_GI$Growth_NY
EA_GI$Inflation_exp <- (1 - month(EA_GI$Date)/12)*EA_GI$Inflation_CY + (month(EA_GI$Date)/12)*EA_GI$Inflation_NY
EA_GI.ts <- xts(EA_GI[2:7]/100, order.by = EA_GI$Date)

rm(EA_GI,US_GI)
ep = endpoints(US_GI.ts,on = "months", k = 3)
US_GI.ts = period.apply(US_GI.ts,INDEX=ep,FUN=mean)
ep = endpoints(EA_GI.ts,on = "months", k = 3)
EA_GI.ts = period.apply(EA_GI.ts,INDEX=ep,FUN=mean)
colnames(US_GI.ts) <- paste0(colnames(US_GI.ts),"_US")
colnames(EA_GI.ts) <- paste0(colnames(EA_GI.ts),"_EA")


#Merge all to make the final dataase---------------------------------------
df <- merge(Germany,Germany_1)
df <- merge.xts(df,US_1)
df <- merge(df,US_2)
df <- merge(df,US_3)
df <- merge(df,US_4)
df <- df[5:nrow(df),]
df <- merge.xts(df,ecb_assets.ts$Asset_GDP_EA, join = "inner")
df <- merge.xts(df,fed_assets.ts$Asset_GDP_US, join = "inner")
df <- merge.xts(df,data_EA.ts$y_gap_EA, join = "inner")
df <- merge.xts(df,US_macro.ts$y_gap_US, join = "inner")
df <- merge.xts(df,data_EA.ts$pi_EA, join = "inner")
df <- merge.xts(df,data_US.ts$pi_US, join = "inner")
df$Asset_Diff <- df$Asset_GDP_EA - df$Asset_GDP_US
df <- merge.xts(df,EA_GI.ts[,5:6], join = "inner")
df <- merge.xts(df,US_GI.ts[,5:6], join = "inner")
df$Growth_exp_diff <- df$Growth_exp_EA - df$Growth_exp_US
df$Inflation_exp_diff <- df$Inflation_exp_EA - df$Inflation_exp_US

mod_rf <- lm(Germany~US_3 + Germany_1 + Asset_Diff + 
               Inflation_exp_EA + Growth_exp_US, data = df)
summary(mod_rf)
u <- xts(resid(mod_rf), order.by = index(df))
plot(u, col = "brown")

