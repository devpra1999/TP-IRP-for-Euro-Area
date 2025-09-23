###################
#
### author: cafavero
###################


## -----------------------------------------------------------------------------
load("data/main.rda")

## -----------------------------------------------------------------------------
## Select the time window
#time_window <- "1980-03-01/2023-06-01"
#data.ts <- data.ts[time_window]

## -----------------------------------------------------------------------------
## Estimate the model
if(!"trend" %in% colnames(data.ts)){
  data.ts$rf <- data.ts$yNSS1/4
  longrun<-lm(rf~MY+dy_pot+PTR-1,data=data.ts["1980-03-01/2023-06-01"])
  # Extend the exogenous variables until 2038
  data.ts$dy_pot["2026-03-01/2038-06-01"]<-0.0175
  data.ts$PTR["2023-09-01/2038-06-01"]<-0.02
  data.ts$trend<-longrun$coefficients[1]*data.ts$MY+longrun$coefficients[2]*data.ts$dy_pot+longrun$coefficients[3]*data.ts$PTR
  #trend <- ts(fitted(longrun), start=c(1980,1), frequency = 4)
  #trend <- xts(trend, order.by = index(data.ts))
  #data.ts <- merge(data.ts, trend)
  colnames(data.ts)[ncol(data.ts)] <- "trend"
  summary(longrun)
  
  
  u <-data.ts$US_RFcc-data.ts$trend#ts(resid(longrun), start=c(1980,1), frequency = 4)
  adf.test(na.omit(u))
  residuals_xts <- xts(u, order.by = index(data.ts))
  #trend <- xts(trend, order.by = index(data.ts))
  data.ts <- merge(data.ts, residuals_xts)
  colnames(data.ts)[ncol(data.ts)] <- "cycle"

  data.ts$cycle_Lag1 <- lag.xts(data.ts$cycle, 1)
  
  # Run a dynamic linear regression
  shortrun <- lm(cycle ~ cycle_Lag1+pi_gap+y_gap-1, data = data.ts)
  summary(shortrun)
  
  save(data.ts,file="data/main.rda")

}



