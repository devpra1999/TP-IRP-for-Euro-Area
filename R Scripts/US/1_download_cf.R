################################################################################
###
### AUTHOR:       Carlo Favero and Rubén Fernández-Fuertes
### DATE:         Fall 2023
### DESCRIPTION:  This script creates a frozen version of the database to be used 
###               for the rest of the paper. This programme can then be run to 
###               update the database  but the rest of the paper should work on 
###               the frozen version
### OUTPUT:       two databases: one for the macro variables and one for the term
###               structure    
################################################################################

## -----------------------------------------------------------------------------
rm(list=ls())                                                                   # Clear the environment 

## -----------------------------------------------------------------------------
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))                     # Set the appropriate environment to work in every machine

## -----------------------------------------------------------------------------
listofpackages <- c("tidyverse","ellipse","reshape2","xts","xlsx","readxl",     # Install and load the required packages
"quantmod")   
source("preparePackages.R")
preparePackages(listofpackages)

## -----------------------------------------------------------------------------
# Nelson, Svensson, Siegel yield curve parametrization
nss <- function(n, params) {
  Beta0 <- params$BETA0
  Beta1 <- params$BETA1
  Beta2 <- params$BETA2
  Beta3 <- params$BETA3
  Tau1  <- params$TAU1
  Tau2  <- params$TAU2
  Tau1
  return(Beta0 + Beta1 * (1 - exp(-n / Tau1)) / (n / Tau1) +
           Beta2 * ((1 - exp(-n / Tau1)) / (n / Tau1) - exp(-n / Tau1)) +
           Beta3 * ((1 - exp(-n / Tau2)) / (n / Tau2) - exp(-n / Tau2)))
}



## -----------------------------------------------------------------------------
# Load the tickers name from the fred_codes.csv where we save a bucnch of useful
# variables from the fRED
tickersFRED <- read.csv("data/fred_codes.csv", stringsAsFactors = FALSE)
tickersFRED <- tickersFRED$code
# Import data from FRED database (getSymbols is a function from quantmod)
getSymbols.FRED(tickersFRED,
                 env = globalenv(),
                 return.class = "xts",
                 index.class ='date'
                 )
# Construct the macrodata xts dataframe. That will be consisting of:
#   1. GDPPOT: Potential Grossz Domestic Product
#   2. GDPC1: Real Gross Domestic Product
#   3. PCECTPI: Personal Consumption Expenditures Excluding Food and Energy 
xts_list <- list(GDPC1, GDPPOT, PCECTPI)
rawdata.ts <- Reduce(function(x, y) merge(x, y, join = 'outer'), xts_list)
rm(GDPC1, GDPPOT, PCECTPI, FEDFUNDS)
rm(xts_list)

# Realign dates
date <-seq(as.Date("1947-03-01"),length=nrow(rawdata.ts), by="quarters")
rawdata.df <- data.frame(Data = coredata(rawdata.ts))                           # This creates a dataframe from the time series data with the appropriate date window
rawdata1.ts <- xts(rawdata.df, order.by=date)                                   # Create the new timeseries object  
colnames(rawdata1.ts)<-colnames(rawdata.ts)                                     # Appropriate column names
rm(rawdata.ts)
rawdata.ts<-rawdata1.ts
rm(rawdata1.ts,rawdata.df)

# Take the Treasury Bill (3-Month) at the quarterly frequancy
TB3MO <- apply.quarterly(DGS3MO, last)
 
# Realign Dates (as before)
dates <-seq(as.Date("1981-09-01"),length=nrow(TB3MO), by="quarters")
TB3MO.df <- data.frame( Data = coredata(TB3MO))
TB3MO.ts <- xts(TB3MO.df, order.by=dates)
rm(TB3MO.df)
#fill the NAs in three cases in which the last obs of the quarter was NA (manually)
TB3MO.ts["1991-03-01"]<- DGS3MO["1991-03-28"]
TB3MO.ts["2002-03-01"]<- DGS3MO["2002-03-28"]
TB3MO.ts["2013-03-01"]<- DGS3MO["2013-03-28"]
TB3MO.ts["2018-03-01"]<- DGS3MO["2018-03-29"]
xts_list <- list(rawdata.ts,TB3MO.ts)
rawdata.ts <- Reduce(function(x, y) merge(x, y, join = 'outer'), xts_list)
rawdata.ts$DGS3MO["2023-12-01"] <- NA
rm(DGS3MO, TB3MO, TB3MO.ts, xts_list)


## -----------------------------------------------------------------------------
# We are missing MY, PTR (not on FRED) and the data from Gurkanyak we import them
# EXCEL files
### PTR
FRED_data <- read.delim("data/FRED_PTR/HISTDATA.TXT", sep = ",")
dates <-seq(as.Date("1968-03-01"),length=nrow(FRED_data), by="quarters")
PTR <- xts(FRED_data$PTR, order.by=dates)
rawdata.ts <- merge(rawdata.ts, PTR, join = 'outer')
rm(PTR, FRED_data)


### MY
MYdata<- read_excel('data/MY_long.xlsx', sheet=1, col_names=TRUE)
dates <-seq(as.Date("1947-03-01"),length=nrow(MYdata), by="quarters")
MY <- xts(MYdata$MY, order.by=dates)
rawdata.ts <- merge(rawdata.ts, MY, join = 'outer')
rm(MY, MYdata)


### YIELDS
### long-term yields from Gurkanyak and three-month rate from Bauer-Rudebusch database
### use the betas and the taus in the file to construct yields at all needed maturities
### name them in terms of quarters and add them to the database
### the file with all the info is yields_GURK.xlsx

yields<- read_excel('data/yields_GURK.xlsx', sheet=1, col_names=TRUE)
dates <-seq(as.Date("1971-12-01"),length=nrow(yields), by="quarters")

yields.ts <- xts(yields, order.by=dates)
yields.ts <- yields.ts[, -1]


# Take just the parameters
params.cols <- grep("^(BETA|TAU)", colnames(yields.ts))
params.ts <- yields.ts[, params.cols]
save(params.ts,file="data/yields_GURK_params.rda")
write.csv(data.frame(params.ts), "data/yields_GURK_params.csv", row.names = TRUE)

# Take US_Rf to add it at the end
US_RF.ts <- yields.ts[,"US_RF"]
rawdata.ts <- merge(rawdata.ts, US_RF.ts, join = 'outer')
# Take just the yields and save their names as "nX" where X is a number
yields.cols <- grep("^SVENY", colnames(yields.ts))
yields.ts <- yields.ts[, yields.cols]
colnames(yields.ts) <- gsub("^SVENY", "y", colnames(yields.ts))
new_colnames <- sapply(str_extract_all(colnames(yields.ts), "\\d+"), function(x) {
  paste0("y", as.integer(x) * 4)
})
colnames(yields.ts) <- new_colnames

# Since computations are a nightmare with time series, we convert everything to numeric
# and do everything with this class
Beta0 <- as.numeric(params.ts$BETA0)
Beta1 <- as.numeric(params.ts$BETA1)
Beta2 <- as.numeric(params.ts$BETA2)
Beta3 <- as.numeric(params.ts$BETA3)
Tau1  <- as.numeric(params.ts$TAU1)
Tau2  <- as.numeric(params.ts$TAU2)
N=60
ttm = c(1:N)*(1/4)
maturities<-c(1:N)
for (n in maturities){
  new_yield <-(Beta0 + Beta1 * (1 - exp(-0.25*n / Tau1)) / (0.25*n / Tau1) +
                 Beta2 * ((1 - exp(-0.25*n / Tau1)) / (0.25*n / Tau1) - exp(-0.25*n / Tau1)) +
                 Beta3 * ((1 - exp(-0.25*n / Tau2)) / (0.25*n / Tau2) - exp(-0.25*n / Tau2)))
  # Again to yields
  new_yield <- xts(new_yield, order.by=index(yields.ts))
  # Fancy header name
  colname   <- paste0("yNS", n)
  # Merge it with respect to the old dataset
  yields.ts <- merge(yields.ts, new_yield, join="left")
  colnames(yields.ts)[ncol(yields.ts)] <- colname
}
nss_yields<-0.01*yields.ts[,16:75]
colnames(nss_yields) <- paste("n",seq(1,N),sep = "_")
rm(list="params.ts","US_RF.ts","yields","yields.ts","new_yield")

#rawdata.ts <- merge(rawdata.ts, yields.ts[,-1], join = 'outer')
#rawdata.ts <- merge(rawdata.ts, US_RF.ts, join = 'outer')
#rm(yields.ts, yields)

save(rawdata.ts,file="data/raw/rawdata1.rda")
save(nss_yields,file="data/raw/yields1.rda")