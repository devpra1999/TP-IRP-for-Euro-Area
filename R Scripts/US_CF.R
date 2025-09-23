
### FOMC Forecasts-----------------------------------------------------------
#df1 <- read_tsv("Data_Files/FOMC_fed_proj_new.txt",col_names = TRUE,na = ".")
#Download from https://alfred.stlouisfed.org/series/downloaddata?seid=FEDTARMD
df <- read_csv("Data_Files/FEDTARMD_vintage.csv",col_names = TRUE,na = ".")
curr_rates <- read_csv("Data_Files/Fed_Rates.csv")  %>% filter(DATE >= "2015-12-01")
curr_rates <- curr_rates[seq(1,nrow(curr_rates), by = 3),]
curr_rates <- curr_rates[c(-18,-21),] #COVID dates with no FOMC projections
df_long <- read_csv("Data_Files/FOMC_fed_proj_long.csv") %>% filter(DATE >= "2015-12-16")
df_long <- df_long[-20,]
for (i in 2:length(df)){
  #Extract year of observation
  y <- year(df_long$DATE[i-1])
  #Given start year is 2015
  df[(y-2015+10),i] <- df_long$FEDTARMDLR[i-1]
}
for (j in 2:length(df$observation_date)){
  df$observation_date[j] <- df$observation_date[j-1] + years(1)
}

# forecasts <- matrix(nrow = dim(df)[2]-1, ncol = 10)
# for (i in 2:length(df)){
#   dat <- df[[colnames(df)[i]]]
#   indices <- which(!is.na(dat))
#   num <- length(indices) - 1
#   f <- min(indices)
#   l <- max(indices)
#   forecasts[i-1,1:2] <- t(as.matrix(dat[f:(f+1)]))
#   forecasts[i-1,3:10] <- na.approx(dat[f+2:l])
#   #forecasts[i-1,] <-na.spline(dat[f:l])
# }
# forecasts <- as.data.frame(forecasts)
# #forecasts <- as.data.frame(t(apply(df[,2:length(df)],2,na.approx)))
# forecasts$Date <- floor_date(df_long$DATE,"month")
# T <- 10
# forecasts$sum_Er_cf <- forecasts$V1 +
#   (1 - 1/T)*(forecasts$V2 - forecasts$V1) +
#   (1 - 2/T)*(forecasts$V3 - forecasts$V2) +
#   (1 - 3/T)*(forecasts$V4 - forecasts$V3) +
#   (1 - 4/T)*(forecasts$V5 - forecasts$V4) +
#   (1 - 5/T)*(forecasts$V6 - forecasts$V5) +
#   (1 - 6/T)*(forecasts$V7 - forecasts$V6) +
#   (1 - 7/T)*(forecasts$V8 - forecasts$V7) +
#   (1 - 8/T)*(forecasts$V9 - forecasts$V8) +
#   (1 - 9/T)*(forecasts$V10 - forecasts$V9)
# forecasts$Date <- floor_date(forecasts$Date,"month")

forecasts <- matrix(nrow = dim(df)[2]-1, ncol = 40)
forecasts <- as.data.frame(forecasts)
#forecasts$Date <- floor_date(df_long$DATE,"month")
forecasts$Date <- df_long$DATE
for (i in 2: length(df)){
  dat <- df[[colnames(df)[i]]]
  indices <- which(!is.na(dat))
  num <- length(indices) - 1
  if (month(forecasts$Date[i-1])==3){n=4}
  if (month(forecasts$Date[i-1])==6){n=3}
  if (month(forecasts$Date[i-1])==9){n=2}
  if (month(forecasts$Date[i-1])==12){n=1}
  for (j in 1:num){
     forecasts[i-1,n+4*(j-1)]<- dat[indices[j]]
  }
  forecasts[i-1,40] <- dat[indices[num+1]]
  forecasts[i-1,1] <- as.numeric(curr_rates[i-1,2])
  forecasts[i-1,1:40] <- na.approx(as.numeric(forecasts[i-1,1:40]))
}
forecasts$sum_Er_cf <- apply(forecasts[,1:40], 1 , mean)

### Survey Forecasts-----------------------------------------------------------
df <- read_xlsx("Data_Files/US_TBILL.xlsx")

### Merge------------------------------------------------------------------------
US_yield <- read_csv("Data_Files/US-long-yield.csv",col_names = c("Date","Yield"),skip = 1)
US_data <- merge(US_yield,forecasts) %>% filter(Date >= "2015-12-01")
US_data$TP <- US_data$Yield - US_data$sum_Er_cf
# N = 40
# ttm = c(1:N)*(1/4)
# source("NSS.R")
# L <- nss_yields("Data_Files/USnss.csv",N*3)
# rawYields <- L$rawYields
# plot_dates <- L$plot_dates
# US_data <- as.data.frame(cbind(plot_dates,rawYields[,120]))
# colnames(US_data) <- c("Date","Yield")
# US_data$Date <- as.Date(US_data$Date)
# US_data <- merge(US_data,forecasts,all = TRUE) %>% filter(Date >= "2015-12-01")
# US_data$TP <- US_data$Yield - US_data$sum_Er_cf
