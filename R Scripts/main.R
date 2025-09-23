preparePredList <- function(name){
  train_period <- "1980-03-01/2025-09-01"
  forecast_window.length <- floor((2035-2025)*4)
  forecast_window.dates <-seq(as.Date("2025-09-01"),length=forecast_window.length, by="quarters")
  
  X_train.ts  <- X_US.ts["1980-03-01/2025-09-01"]
  X_train.var <- VAR(X_train.ts, p = 1, type = "const") 
  X_train.for <- predict(X_train.var, n.ahead = forecast_window.length)
  X_in.mat    <- fitted(X_train.var) 
  X_for.mat   <- cbind(X_train.for$fcst$PC1[,1],X_train.for$fcst$PC2[,1],X_train.for$fcst$PC3[,1],
                       X_train.for$fcst$PC4[,1],X_train.for$fcst$PC5[,1])
  dates <- c(index(X_train.ts),forecast_window.dates)
  pred_list <- list()
  for (n in c(1, 40)) {
    
    if (name=="FF"){
      # Create prediction
      y_cycle_OoS <- t(-(A[n]+t(B[, n])%*%t(X_for.mat))/ttm[n])
      y_cycle_IN  <- t(-(A[n]+t(B[, n])%*%t(X_in.mat))/ttm[n])
      
      # Convert prediction to xts time series
      y_cycle_OoS.ts <- xts((y_cycle_OoS), order.by = forecast_window.dates) # assuming dates_for remains constant
      y_cycle_IN.ts <- xts(y_cycle_IN, order.by = index(X_train.ts)[-1])
      
      y_OoS.ts <- merge(y_cycle_OoS.ts, Y_trend.ts[,1],join="outer")
      y_IN.ts  <- merge(y_cycle_IN.ts, Y_trend.ts[,1],join="outer")
      
      colnames(y_OoS.ts) <- c("cycle", "trend")
      colnames(y_IN.ts) <- c("cycle", "trend")
      
      y_OoS.ts <- y_OoS.ts$cycle + y_OoS.ts$trend
      y_IN.ts  <- y_IN.ts$cycle + y_IN.ts$trend
    } else {
      # Create prediction
      y_OoS <- t(-(A[n]+t(B[, n])%*%t(X_for.mat))/ttm[n])
      y_IN  <- t(-(A[n]+t(B[, n])%*%t(X_in.mat))/ttm[n])
      
      # Convert prediction to xts time series
      y_OoS.ts <- xts((y_OoS), order.by = forecast_window.dates) # assuming dates_for remains constant
      y_IN.ts <- xts(y_IN, order.by = index(X_train.ts)[-1])
      
    }
    
    # Merge time series
    pred.ts <- xts(merge(nss_yields_US.ts[dates, paste0("n_", n)], na.omit(y_OoS.ts), join = 'outer'), dates)
    pred.ts <- xts(merge(pred.ts, na.omit(y_IN.ts), join = 'outer'), dates)
    pred.ts <- pred.ts["1980-09-01/2033-12-01"]
    
    # Set column names
    colnames(pred.ts) <- c(paste0("n", n), paste0("n", n, "_OoS"), paste0("n", n, "_IN"))
    
    # Append to the results list
    pred_list[[paste0("n", n)]] <- pred.ts
    
  }
  return(pred_list)
}
# scaleData <- function(data) {
#   # List of column names to be checked and potentially scaled
#   columns_to_scale <- c("dy", "dy_pot", "y_gap", "pi_gap", "pi", "PTR")
#   
#   # Iterate through each column
#   for (col in columns_to_scale) {
#     # Check if the column exists in the data
#     if (col %in% names(data)) {
#       # Check if max of column > 1 and scale if necessary
#       if (max(data[,col], na.rm = TRUE) > 1) {
#         data[,col] <- data[,col] / 100
#       }
#     }
#   }
#   return(data)
# }
# 
# #source('src/getRawData.R')
# source('src/getMainData.R')
# 
# # UPDATE 1: Wednesday 22nd November, 2023.
# # Adding 2023Q3 PTR and pi_gap
# data.ts["2023-09-01", "PTR"] = 2.22
# data.ts["2023-09-01", "pi_gap"] = data.ts["2023-09-01", "pi"] - data.ts["2023-09-01", "PTR"]
# data.ts <- scaleData(data.ts)

## STEP 1: ACM model
source("ACM_US_new.R")
X.var.ACM <- my_var_model_US
termpremia.ts.ACM <- termpremia_US.ts
yield.ts.ACM <- fittedYields_US.ts
MP.ts.ACM <- yield.ts.ACM - termpremia.ts.ACM

l_ACM <- preparePredList("ACM") 

## -----------------------------------------------------------------------------
## STEP 2: Our alternative model
source("US_new.R")

X.var.FF <- my_var_model_US
termpremia.ts.FF <- termpremia_US.ts
yield.ts.FF <- fittedYields_US.ts
MP.ts.FF <- yield.ts.FF - termpremia.ts.FF

l_FF <- preparePredList("FF")
