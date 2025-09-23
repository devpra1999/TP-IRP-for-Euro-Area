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

####GET GERMANY YIELD CURVE--------------------------------
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

####Get EA Yield Curve
L <- nss_yields("Data_Files/EA_YC_clean.csv",N*3)
rawYields <- L$rawYields/100
plot_dates <- L$plot_dates
nss_yields_EA.ts <- xts(rawYields,plot_dates)
nss_yields_EA.ts <- nss_yields_EA.ts[seq(from = 1, to = 237, by = 3),seq(from = 3, to = N*3, length.out = N)]
colnames(nss_yields_EA.ts) <- paste("n",seq(1,N),sep = "_")
T = nrow(nss_yields_EA.ts)

####Get US Yield Curve----------------------------------
load('data/raw/yields1.rda')
#load('yields1.rda')
nss_yields_US.ts <- nss_yields
rm(nss_yields)


rm(L,df,rawYields)


#Set Dates for analysis----------------------------------
current_dates_EA <- index(Yields)

####Get US lags-------------------------------------
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


###### Get US macrodataset ------------------------
load('data/raw/rawdata1.rda')
#load('rawdata1.rda')

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


##########Get EA Macrodataset------------------------
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
tmp <- read_csv("atsm_eu/data/data_gdp_and_inflation_US_EA.csv") %>% filter(country == "EA")
tmp.ts <- xts(tmp[,2:ncol(tmp)], order.by = tmp$date)
tmp.ts <- tmp.ts$cpi
data_EA.ts <- merge.xts(data_EA.ts,tmp.ts[current_dates_EA,])
data_EA.ts$LP_EA    <- log(data_EA.ts$cpi)
data_EA.ts$pi_EA     <- (data_EA.ts$LP_EA - lag.xts(data_EA.ts$LP_EA, 4))
rm(tmp,tmp.ts)


####Get Fed Asset Growth--------------------------------
fed_assets <- read_csv("Fed Assets.csv")
colnames(fed_assets) <- c("Date","Assets")
fed_assets$Asset_Growth <- fed_assets$Assets/lag(fed_assets$Assets) - 1
# tmp <- aggregate(Asset_Growth~quarter + year, transform(fed_assets, 
#                                                         quarter = quarters(Date), year = as.integer(format(Date, '%Y'))), mean)
tmp <- aggregate(Assets~quarter + year, transform(fed_assets, 
                                             quarter = quarters(Date), year = as.integer(format(Date, '%Y'))), mean)
tmp <- tmp[order(tmp$year), ]
tmp$Date <- paste0(tmp$year,tmp$quarter)
tmp <- tmp %>% select(Date,Assets)
#tmp <- tmp %>% select(Date, Asset_Growth)
tmp$Date = as.Date(as.yearqtr(tmp$Date, format = "%YQ%q")) + months(2)
fed_assets = tmp
fed_assets.ts <- xts(fed_assets$Assets,order.by = fed_assets$Date)
colnames(fed_assets.ts) <- c("Assets")
#fed_assets.ts <- xts(fed_assets$Asset_Growth,order.by = fed_assets$Date)
rm(tmp,fed_assets)
load('data/raw/rawdata1.rda')
fed_assets.ts <- merge.xts(fed_assets.ts,rawdata.ts$GDPC1, join = "inner")
fed_assets.ts$Asset_GDP_US <- fed_assets.ts$Assets/(1000*fed_assets.ts$GDPC1)
rm(rawdata.ts)


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
# data_EA <- read_csv("atsm_eu/data/data_gdp_and_inflation_US_EA.csv") %>% filter(country == "EA")
# data_EA.ts <- xts(data_EA[,3:length(data_EA)],order.by = data_EA$date)
ecb_assets.ts <- merge.xts(ecb_assets.ts,data_EA.ts$GDPC1, join = "inner")
ecb_assets.ts$Asset_GDP_EA <- ecb_assets.ts$Assets/(1000*ecb_assets.ts$GDPC1)
assets_diff.ts <- ecb_assets.ts$Asset_GDP_EA - fed_assets.ts$Asset_GDP_US
assets_diff.ts <- scale(assets_diff.ts)/100


# LR_inf <- read_csv("atsm_eu/data/data_ecb_inflation_LR.csv")
# LR_inf.ts <- xts(LR_inf$inflation_LR,order.by = LR_inf$date)
# MY_df <- read_csv("atsm_eu/data/data_euro_demographic_structure.csv")
# MY_df.ts <- xts(MY_df$MY,order.by = MY_df$date)
# data_EA.ts <- merge(data_EA.ts,LR_inf.ts,MY_df.ts)
# rm(data_EA,LR_inf,LR_inf.ts,MY_df,MY_df.ts)
# 
# US_names <- c("GDPC1","PCECTPI","GDPPOT","gdp_growth","pi","dy_pot","PTR","MY")
# colnames(data_EA.ts) <- US_names
# data_EA.ts$PTR <- data_EA.ts$PTR/100
# rm(US_names)
# 
# data_EA.ts$y      <- log(data_EA.ts$GDPC1)
# data_EA.ts$y_pot  <- log(data_EA.ts$GDPPOT)
# data_EA.ts$y_gap  <- (data_EA.ts$y - data_EA.ts$y_pot)
# data_EA.ts$dy_pot <- (data_EA.ts$y_pot - lag.xts(data_EA.ts$y_pot, 4))

#EXPECTED INFLATION & GROWTH
# Germany_GI <- read_csv("Data_Files/Germany_growth_inf.csv")
# Germany_GI.ts <- xts(Germany_GI[2:5]/100, order.by = Germany_GI$Date)
# France_GI <- read_csv("Data_Files/France_growth_inf.csv")
# France_GI.ts <- xts(France_GI[2:5]/100, order.by = France_GI$Date)
# Italy_GI <- read_csv("Data_Files/Italy_growth_inf.csv")
# Italy_GI.ts <- xts(Italy_GI[2:5]/100, order.by = Italy_GI$Date)
# Spain_GI <- read_csv("Data_Files/Spain_growth_inf.csv")
# Spain_GI.ts <- xts(Spain_GI[2:5]/100, order.by = Spain_GI$Date)
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
# Germany_GI.ts = period.apply(Germany_GI.ts,INDEX=ep,FUN=mean)
# France_GI.ts = period.apply(France_GI.ts,INDEX=ep,FUN=mean)
# Italy_GI.ts = period.apply(Italy_GI.ts,INDEX=ep,FUN=mean)
# Spain_GI.ts = period.apply(Spain_GI.ts,INDEX=ep,FUN=mean)
# colnames(Germany_GI.ts) <- paste0(colnames(Germany_GI.ts),"_EA")
# colnames(France_GI.ts) <- paste0(colnames(France_GI.ts),"_EA")
# colnames(Italy_GI.ts) <- paste0(colnames(Italy_GI.ts),"_EA")
# colnames(Spain_GI.ts) <- paste0(colnames(Spain_GI.ts),"_EA")
# colnames(US_GI.ts) <- paste0(colnames(US_GI.ts),"_US")
# EA_GI.ts <- (Germany_GI.ts + France_GI.ts + Italy_GI.ts + Spain_GI.ts)/4
# rm(ep,Germany_GI.ts,Spain_GI.ts,France_GI.ts,Italy_GI.ts)

df <- merge(Germany,Germany_1)
df <- merge.xts(df,US_1)
df <- merge(df,US_2)
df <- merge(df,US_3)
df <- merge(df,US_4)
df <- df[5:nrow(df),]
rm(Germany,Germany_1,US_1,US_2,US_3,US_4)

tmp <- read_excel("Refinitiv_10Y.xlsx", sheet = "Fragmentation", skip = 1)
tmp$Fragmentation = apply(as.matrix(tmp[2:length(tmp)]),1,sd)
tmp$Code <- floor_date(tmp$Code + days(4), "month")
frag <- xts(tmp$Fragmentation, order.by = tmp$Code)
rm(tmp)
df <- merge.xts(df,frag, join = "inner")
rm(frag)
#df <- merge.xts(df,assets_diff.ts, join = "inner")
df <- merge.xts(df,ecb_assets.ts$Asset_GDP, join = "inner")
df <- merge.xts(df,fed_assets.ts$Asset_GDP, join = "inner")
df <- merge.xts(df,data_EA.ts$y_gap_EA, join = "inner")
df <- merge.xts(df,US_macro.ts$y_gap_US, join = "inner")
df <- merge.xts(df,data_EA.ts$pi_EA, join = "inner")
df <- merge.xts(df,data_US.ts$pi_US, join = "inner")
#df <- merge.xts(df,data_US.ts$y_gap, join = "inner")
df$Asset_Diff <- df$Asset_GDP_EA - df$Asset_GDP_US
df <- merge.xts(df,EA_GI.ts[,5:6], join = "inner")
df <- merge.xts(df,US_GI.ts[,5:6], join = "inner")
df$Growth_exp_diff <- df$Growth_exp_EA - df$Growth_exp_US
df$Inflation_exp_diff <- df$Inflation_exp_EA - df$Inflation_exp_US
# df$growth_3_diff <- df$growth_3_EA - df$growth_3_US
# df$inf_3_diff <- df$inf_3_EA - df$inf_3_US
df$frag[index(df)<"2015-01-01"] = 0
df$frag[index(df)>"2022-07-01"] = 0

#load("reac_fun_new.rda")

mod_rf <- lm(Germany~US_3 + Germany_1 + Asset_Diff + pi_EA + pi_US + 
               Growth_exp_diff + Inflation_exp_diff, data = df)
mod_rf <- lm(Germany~US_3 + Germany_1 + Asset_Diff + 
               Inflation_exp_EA + Growth_exp_US, data = df)
mod_rf <- lm(Germany~US_3 + Asset_Diff + pi_EA + pi_US+
               growth_3_EA + growth_3_US + inf_3_EA + inf_3_US, data = df)
mod_rf <- lm(Germany~US_3 + Asset_Diff + 
               growth_12_EA + growth_12_US + inf_12_EA + inf_12_US, data = df)
mod_rf <- lm(Germany~US_3 + Asset_Diff + pi_EA + pi_US+
               growth_3_EA + growth_12_EA, data = df)
mod_rf <- lm(Germany~US_3 + Germany_1 + Asset_Diff + 
               growth_12_EA + growth_12_US + inf_12_EA + inf_12_US, data = df)
summary(mod_rf)
#save(df,file = "reac_fun_new.rda")

u <- xts(resid(mod_rf), order.by = index(df))
plot(u, col = "brown")
tseries::adf.test(u, k = 0)

plot(df$y_gap)
lines(df$y_gap.1,col = "blue", lwd = 2)

df$Asset_GDP_Growth <- df$Asset_GDP/lag.xts(df$Asset_GDP) - 1

plot(df$Germany, main = "Germany vs US_3rd_lag", ylim = c(-0.1,0.1))
lines(df$US_3, col = "blue", lwd = 2)
lines(df$Asset_GDP, col = "red", lwd = 2, ylim = c(-0.06,0.06))
#lines(rollmean(df$Asset_GDP_Growth/10,10,align = "right"), col = "brown", lwd = 2)
xts::addLegend("topright", legend.names = c("Germany","US_3","ECB - Fed balance sheet"),
                          col = c("black","blue","red"), lty = c(1,1,1), lwd = c(2,2,2))
zero_line = xts(rep(0,nrow(u_3)),order.by = index(u_3))
plot(u_3, col = "blue", ylim = c(-0.03,0.03))
lines(zero_line, col = "black", lty = 2)

tseries::adf.test(u_3, k = 0)

df <- read_excel("Refinitiv_10Y.xlsx", sheet = "Fragmentation", skip = 1)
df$Fragmentation = apply(as.matrix(df[2:length(df)]),1,sd)
df$Code <- floor_date(df$Code + days(4), "month")
frag <- xts(df$Fragmentation, order.by = df$Code)
df <- merge.xts(US_3,frag, join = "inner")
plot(US_3*100)
lines(frag)

df <- merge.xts(df, tmp$Assets_Growth/100, join = "inner")
df$frag[index(df)<"2015-01-01"] = 0
df$frag[index(df)>"2022-07-01"] = 0


#Growth rate

fed_assets <- read_csv("Fed Assets.csv")
colnames(fed_assets) <- c("Date","Assets")
tmp <- aggregate(Assets~Date, transform(fed_assets, Date = as.integer(format(Date, '%Y'))), mean)
tmp$Date = ymd(tmp$Date, truncated = 2L) - months(1)
fed_assets = tmp
fed_assets$Assets_Growth <- fed_assets$Assets/lag(fed_assets$Assets) - 1
fed_assets.ts <- xts(fed_assets[,2:3],order.by = fed_assets$Date)
rm(tmp,fed_assets)

ecb_assets <- read_csv("ECB Assets.csv")
colnames(ecb_assets) <- c("Date","Assets")
tmp <- aggregate(Assets~Date, transform(ecb_assets, Date = as.integer(format(Date, '%Y'))), mean)
tmp$Date = ymd(tmp$Date, truncated = 2L) - months(1)
ecb_assets = tmp
ecb_assets$Assets_Growth <- ecb_assets$Assets/lag(ecb_assets$Assets) - 1
ecb_assets.ts <- xts(ecb_assets[,2:3],order.by = ecb_assets$Date)
rm(tmp,ecb_assets)

assets_diff.ts <- ecb_assets.ts$Assets_Growth - fed_assets.ts$Assets_Growth
assets_diff.ts <- scale(assets_diff.ts)

tmp = merge.xts(u_3, assets_diff.ts, join = "outer")
tmp = na.locf(tmp)
plot(tmp$u_3)
lines(tmp$Assets_Growth/100, col = "blue", lwd = 2)
