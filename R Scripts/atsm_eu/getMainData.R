################################################################################
###
### AUTHOR:       Carlo Favero and Rubén Fernández-Fuertes
### DATE:         Fall 2023
### DESCRIPTION:  This script creates the main dataset that is used in our paper.
###               We call it "main.rda" because of obvious reasons.
### OUTPUT:       main.rda
###
################################################################################

source("atsm_eu/src/auxFuncs.R")

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


## -----------------------------------------------------------------------------
## LONG RUN INFLATION EXPECTATIONS:
EA_inflationLR.df <- readData("data_ecb_inflation_LR")
US_inflationLR.df <- readData("data_fed_inflation_LR")
EA_inflationLR.ts <- convertToTS(EA_inflationLR.df, "EA")
US_inflationLR.ts <- convertToTS(US_inflationLR.df, "US")

EA_inflationLR.ts <- getPureNames(EA_inflationLR.ts, "EA")
save(EA_inflationLR.ts, file ="data/rda/EA_inflationLR.rda")
EA_inflationLR.ts <- addCountryToHeader(EA_inflationLR.ts, "EA")


## -----------------------------------------------------------------------------
## DEMOGRAPHICS:
EA_MY.df <- readData("data_euro_demographic_structure")
EA_MY.df <- EA_MY.df[!colnames(EA_MY.df) %in% c('EA_country')]
US_MY.df <- readData("data_us_demographic_structure")
US_MY.df <- US_MY.df[!colnames(US_MY.df) %in% c('US_country')]
EA_MY.ts <- convertToTS(EA_MY.df, "EA")
US_MY.ts <- convertToTS(US_MY.df, "US")
EA_MY.ts <- getPureNames(EA_MY.ts, "EA")
save(EA_MY.ts, file="data/rda/EA_MY.rda")
EA_MY.ts <- addCountryToHeader(EA_MY.ts, "EA")

## -----------------------------------------------------------------------------
## YIELD CURVE:
EA_yieldCurve.df <- readData("data_ger_yield_curve")
US_yieldCurve.df <- readData("data_us_yield_curve")
EA_yieldCurve.ts <- convertToTS(EA_yieldCurve.df, "EA")
US_yieldCurve.ts <- convertToTS(US_yieldCurve.df, "US")
colnames(EA_yieldCurve.ts) <- sapply(colnames(EA_yieldCurve.ts), function(x) sub("_X", "_n", x))
colnames(US_yieldCurve.ts) <- sapply(colnames(US_yieldCurve.ts), function(x) sub("_X", "_n", x))
EA_yieldCurve.ts <- getPureNames(EA_yieldCurve.ts, "EA")
save(EA_yieldCurve.ts, file="data/rda/EA_yieldCurve.rda")
EA_yieldCurve.ts <- addCountryToHeader(EA_yieldCurve.ts, "EA")


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

# Select time window
main.ts <- main.ts["1980-03-01/2023-09-1"]


## -----------------------------------------------------------------------------
## TREND::
US_trend_eq <- US_n1 ~ 0 + US_MY    + US_dpot_real_gdp  + US_inflation_LR
US_cycle_eq <- US_u  ~ 0 + US_lag_u + US_real_gdp_gap   + US_inflation_gap
EA_trend_eq <- EA_n1 ~ 0 + EA_MY    + EA_dpot_real_gdp  + EA_inflation_LR
EA_cycle_eq <- EA_u  ~ 0 + EA_lag_u + EA_real_gdp_gap   + EA_inflation_gap

# Save the equations in a systme
cross_trend <- list(US_trend_eq, EA_trend_eq)
cross_cycle <- list(US_cycle_eq, EA_cycle_eq)

source("src/getSURETrend.R")
source("src/saveResults.R")

# Define the restriction matrix (MY and dpot are the same)
R <- matrix(c(1, 0, 0, -1, 0, 0,
              0, 1, 0, 0, -1, 0),
            byrow=TRUE, nrow=2, ncol=6)

# Get the trends for both the US and EA
output           <- getSURETrend(cross_trend, main.ts, R)
# Take the trend just using trend = bX, where b is the vector of coefficients
# and X is the time series matrix of factors
main.matrix      <- as.matrix(main.ts[, c('EA_MY', 'EA_dpot_real_gdp', 'EA_inflation_LR')])
EA_trend_factors <- as.matrix(output$EA_trend_coeff)
trend.matrix <- main.matrix %*% t(EA_trend_factors)
trend.ts     <- xts(coredata(trend.matrix), order.by=index(main.ts))
EA_macroData.ts$EA_r_star <- trend.ts
EA_macroData.ts$EA_r      <- EA_yieldCurve.ts$EA_n1
EA_macroData.ts$EA_u      <- EA_macroData.ts$EA_r - EA_macroData.ts$EA_r_star
EA_macroData.ts <- getPureNames(EA_macroData.ts, "EA")
save(main.ts, file="data/rda/main.rda")

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
  new_hper   <- EA_hpr.ts[, colname_hpr] - 1/4 * lag.xts(EA_yieldCurve.ts$EA_n1)
  colnames(new_hper) <- colname_hper
  if (q==2) {
    EA_hper.ts <- new_hper
  } else {
    EA_hper.ts <- merge(EA_hper.ts, setNames(new_hper, colname_hper))
    }
  
}

save(EA_hpr.ts,file="data/rda/EA_hpr.rda")
save(EA_hper.ts,file="data/rda/EA_hper.rda")


