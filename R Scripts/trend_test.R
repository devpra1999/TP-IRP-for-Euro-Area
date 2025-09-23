# load("atsm_eu/data/rda/EA_macroData.rda")
# rawdata1.ts <- EA_macroData.ts 
# rawdata1.ts$LP     <- log(rawdata1.ts$cpi)
# rawdata1.ts$pi     <- (rawdata1.ts$LP - lag.xts(rawdata1.ts$LP, 4))
# rawdata1.ts$pi_gap <- rawdata1.ts$pi - rawdata1.ts$PTR/100

#----------EU TREND-----------------------
u_EU.ts <- data_EA.ts$cycle[current_dates_EA]
tseries::adf.test(u_EU.ts,k=0)

#--------US TREND--------------------------
r_US.ts <- data_US.ts$trend
u_US.ts <- data_US.ts$cycle
tseries::adf.test(u_US.ts[current_dates_US],k=0)
u_t1.ts <- EA_yieldCurve.ts$n1 - r_US.ts
tseries::adf.test(u_t1.ts,k=0)

##------POOLING---------------------------
load("atsm_eu/data/rda/EA_macroData.rda")
current_time_window <- "2001-03-01/2023-06-01"
r_pool.ts <- EA_macroData.ts$r_star[current_time_window]
u_t2.ts <- EA_yieldCurve.ts$n1 - r_pool.ts
tseries::adf.test(u_t2.ts,k=0)

#-----------US trend on short sample (2000-2023-------------)
r_US_short.ts <- xts(fitted(longrun_short),order.by = current_dates)
u_US_short.ts <- xts(resid(longrun_short),order.by = current_dates)
tseries::adf.test(u_US_short.ts,k=0)

plot(r_US.ts)
lines(r_US_short.ts, col = "blue") #US trend on short sample
lines(r_EU_short.ts, col = "red") #EU trend on short sample
lines(r_pool.ts, col = "orange") #Pooled trend
