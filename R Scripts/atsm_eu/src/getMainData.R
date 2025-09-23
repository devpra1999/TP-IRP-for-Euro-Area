################################################################################
###
### AUTHOR:       Carlo Favero and Rubén Fernández-Fuertes
### DATE:         Fall 2023
### DESCRIPTION:  This script creates the main dataset that is used in our paper.
###               We call it "main.rda" because of obvious reasons.
### OUTPUT:       main.rda
###
################################################################################

## -----------------------------------------------------------------------------
source("atsm_eu/src/auxFuncs.R")

## -----------------------------------------------------------------------------
## IMPORTANT:
trend_factors <- c('MY', 'dpot_real_gdp', 'inflation_LR')
cycle_factors <- c('real_gdp_gap', 'inflation_gap')

## -----------------------------------------------------------------------------
## MACRO DATA:
## Read the CSV file and convert to xts, excluding the 'date' column
macroData.df    <- readData("data_gdp_and_inflation_US_EA")
EA_macroData.df <- macroData.df[macroData.df$country=="EA",][!colnames(macroData.df) %in% c('country')]
US_macroData.df <- macroData.df[macroData.df$country=="US",][!colnames(macroData.df) %in% c('country')]
EA_macroData.ts <- convertToTS(EA_macroData.df, "EA")
US_macroData.ts <- convertToTS(US_macroData.df, "US")
EA_macroData.ts$EA_real_gdp_gap <- log(EA_macroData.ts$EA_real_gdp) - log(EA_macroData.ts$EA_pot_real_gdp)
US_macroData.ts$US_real_gdp_gap <- log(US_macroData.ts$US_real_gdp) - log(US_macroData.ts$US_pot_real_gdp)
rm(EA_macroData.df,US_macroData.df)


## -----------------------------------------------------------------------------
## LONG RUN INFLATION EXPECTATIONS:
EA_inflationLR.df <- readData("data_ecb_inflation_LR")
US_inflationLR.df <- readData("data_fed_inflation_LR") 
EA_inflationLR.ts <- convertToTS(EA_inflationLR.df, "EA") / 100
US_inflationLR.ts <- convertToTS(US_inflationLR.df, "US") / 100

EA_inflationLR.ts <- getPureNames(EA_inflationLR.ts, "EA")
save(EA_inflationLR.ts, file ="atsm_eu/data/rda/EA_inflationLR.rda")
US_inflationLR.ts <- getPureNames(US_inflationLR.ts, "EA")
save(US_inflationLR.ts, file ="atsm_eu/data/rda/US_inflationLR.rda")
EA_inflationLR.ts <- addCountryToHeader(EA_inflationLR.ts, "EA")
rm(EA_inflationLR.df, US_inflationLR.df)


## -----------------------------------------------------------------------------
## DEMOGRAPHICS:
EA_MY.df <- readData("data_euro_demographic_structure")
EA_MY.df <- EA_MY.df[!colnames(EA_MY.df) %in% c('EA_country')]
US_MY.df <- readData("data_us_demographic_structure")
US_MY.df <- US_MY.df[!colnames(US_MY.df) %in% c('US_country')]
EA_MY.ts <- convertToTS(EA_MY.df, "EA")
US_MY.ts <- convertToTS(US_MY.df, "US")
EA_MY.ts <- getPureNames(EA_MY.ts, "EA")
save(EA_MY.ts, file="atsm_eu/data/rda/EA_MY.rda")
EA_MY.ts <- addCountryToHeader(EA_MY.ts, "EA")
rm(EA_MY.df, US_MY.df)

## -----------------------------------------------------------------------------
## YIELD CURVE: Please, note that these data are already annualised. So no need of x4
EA_yieldCurve.df <- readData("data_ger_yield_curve") 
US_yieldCurve.df <- readData("data_us_yield_curve") 
EA_yieldCurve.ts <- convertToTS(EA_yieldCurve.df, "EA") 
US_yieldCurve.ts <- convertToTS(US_yieldCurve.df, "US") 
colnames(EA_yieldCurve.ts) <- sapply(colnames(EA_yieldCurve.ts), function(x) sub("_X", "_n", x))
colnames(US_yieldCurve.ts) <- sapply(colnames(US_yieldCurve.ts), function(x) sub("_X", "_n", x))
EA_yieldCurve.ts <- getPureNames(EA_yieldCurve.ts, "EA")
save(EA_yieldCurve.ts, file="atsm_eu/data/rda/EA_yieldCurve.rda")
EA_yieldCurve.ts <- addCountryToHeader(EA_yieldCurve.ts, "EA") 
rm(EA_yieldCurve.df, US_yieldCurve.df)


## -----------------------------------------------------------------------------
## MAIN DATASET:

main.ts <- merge(EA_macroData.ts, 
                EA_inflationLR.ts,
                EA_MY.ts,
                EA_yieldCurve.ts$EA_n1,
                US_macroData.ts, 
                US_inflationLR.ts,
                US_MY.ts,
                US_yieldCurve.ts$US_n1,
                all=TRUE)



main.ts$EA_inflation_gap <- main.ts$EA_dcpi - main.ts$EA_inflation_LR 
main.ts$US_inflation_gap <- main.ts$US_dcpi - main.ts$US_inflation_LR 


## -----------------------------------------------------------------------------
## EXTEND DATA:
main.ts$US_dpot_real_gdp["2026-03-01/2038-06-01"]<-0.0175
main.ts$US_inflation_LR["2023-09-01/2038-06-01"]<-0.02
main.ts$EA_dpot_real_gdp["2025-06-01/2038-06-01"]<-0.0131
main.ts$EA_inflation_LR["2023-09-01/2038-06-01"]<-0.02

## -----------------------------------------------------------------------------
## TREND::
main.ts$US_r <- main.ts$US_n1/4 # Quarterly rate (not annualised)
main.ts$EA_r <- main.ts$EA_n1/4
US_trend_eq <- US_r ~ 0 + US_MY    + US_dpot_real_gdp  + US_inflation_LR
US_cycle_eq <- US_u  ~ 0 + US_lag_u + US_real_gdp_gap   + US_inflation_gap
EA_trend_eq <- EA_r ~ 0 + EA_MY    + EA_dpot_real_gdp  + EA_inflation_LR
EA_cycle_eq <- EA_u  ~ 0 + EA_lag_u + EA_real_gdp_gap   + EA_inflation_gap

# Save the equations in a systme
cross_trend <- list(US_trend_eq, EA_trend_eq)
cross_cycle <- list(US_cycle_eq, EA_cycle_eq)

source("atsm_eu/src/getSURETrend.R")
source("atsm_eu/src/saveResults.R")
#source("src/getSURETrend.R")
#source("src/saveResults.R")

# Define the restriction matrix (MY and dpot are the same)
R <- matrix(c(1, 0, 0, -1, 0, 0,
              0, 1, 0, 0, -1, 0),
            byrow=TRUE, nrow=2, ncol=6)

# Get the trends for both the US and EA
output           <- getSURETrend(cross_trend, main.ts, R)
rm(cross_cycle, cross_trend)
# Take the trend just using trend = bX, where b is the vector of coefficients
# and X is the time series matrix of factors
EA_main.matrix            <- as.matrix(main.ts[, paste0("EA_", trend_factors)])
EA_trend_factors          <- as.matrix(output$EA_trend_coeff)
EA_trend.matrix           <- EA_main.matrix %*% t(EA_trend_factors)
EA_trend.ts               <- xts(coredata(EA_trend.matrix), order.by=index(main.ts))
EA_macroData.ts$EA_r      <- main.ts$EA_r
EA_macroData.ts$EA_r_star <- EA_trend.ts
EA_macroData.ts$EA_u      <- EA_macroData.ts$EA_r- EA_macroData.ts$EA_r_star
EA_macroData.ts$MY        <- main.ts$EA_MY
EA_macroData.ts$PTR       <- main.ts$EA_inflation_LR
EA_macroData.ts           <- getPureNames(EA_macroData.ts, "EA")
save(EA_macroData.ts, file="atsm_eu/data/rda/EA_macroData.rda")
rm(EA_main.matrix, EA_trend.matrix)

US_main.matrix            <- as.matrix(main.ts[, paste0("US_", trend_factors)])
US_trend_factors          <- as.matrix(output$US_trend_coeff)
US_trend.matrix           <- US_main.matrix %*% t(US_trend_factors)
US_trend.ts               <- xts(coredata(US_trend.matrix), order.by=index(main.ts))
US_macroData.ts$US_r      <- main.ts$US_r
US_macroData.ts$US_r_star <- US_trend.ts
US_macroData.ts$US_u      <- US_macroData.ts$US_r- US_macroData.ts$US_r_star

US_macroData.ts           <- getPureNames(US_macroData.ts, "US")
save(US_macroData.ts, file="atsm_eu/data/rda/US_macroData.rda")
rm(US_main.matrix, US_trend.matrix)

main.ts$EA_u <- EA_macroData.ts$u
main.ts$US_u <- US_macroData.ts$u
save(main.ts, file="atsm_eu/data/rda/main.rda")


## -----------------------------------------------------------------------------
## EXCESS RETURNS

EA_yieldCurve.ts.base_name <- "EA_n"
yield.ts.col <- function(q, base_name){
  return(paste0(base_name,q))
}
N = 60
for (q in 2:N){
  colname_y       <- yield.ts.col(q,EA_yieldCurve.ts.base_name)
  colname_prev_y  <- yield.ts.col(q-1,EA_yieldCurve.ts.base_name) 
  colname_hpr     <- paste0("EA_n",  sprintf("%1d", q-1))
  colname_hper    <- paste0("EA_n", sprintf("%1d", q-1))
  
  # Calculating hpr values based on provided formula
  new_hpr     <- lag.xts(EA_yieldCurve.ts[, colname_y]) - (q-1) * (EA_yieldCurve.ts[, colname_prev_y] - lag.xts(EA_yieldCurve.ts[, colname_y]))
  new_hpr     <- new_hpr/4
  colnames(new_hpr) <- colname_hpr
  if (q==2) {
    EA_hpr.ts <- new_hpr
  } else{
    EA_hpr.ts <- merge(EA_hpr.ts, setNames(new_hpr, colname_hpr))
  }
  
  # Calculating hper values based on provided formula
  new_hper   <- EA_hpr.ts[, colname_hpr] -  lag.xts(EA_yieldCurve.ts$EA_n1)/4
  colnames(new_hper) <- colname_hper
  if (q==2) {
    EA_hper.ts <- new_hper
  } else {
    EA_hper.ts <- merge(EA_hper.ts, setNames(new_hper, colname_hper))
    }
  
}
rm(new_hper, new_hpr)

EA_hpr.ts   <- getPureNames(EA_hpr.ts, "EA")
EA_hper.ts  <- getPureNames(EA_hper.ts, "EA")
#save(EA_hpr.ts,file="data/rda/EA_hpr.rda")
#save(EA_hper.ts,file="data/rda/EA_hper.rda")

save(EA_hpr.ts,file="atsm_eu/data/rda/EA_hpr.rda")
save(EA_hper.ts,file="atsm_eu/data/rda/EA_hper.rda")

## -----------------------------------------------------------------------------
## EA 10Y
big4_y.10Y.df <- readData("data_ea_10y_big4")
big4_y.10Y.ts <- convertToTS(big4_y.10Y.df)
rm(big4_y.10Y.df)
save(big4_y.10Y.ts,file="atsm_eu/data/rda/10y_big4rda")


