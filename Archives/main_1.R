
scaleData <- function(data) {
  # List of column names to be checked and potentially scaled
  columns_to_scale <- c("dy", "dy_pot", "y_gap", "pi_gap", "pi", "PTR")
  
  # Iterate through each column
  for (col in columns_to_scale) {
    # Check if the column exists in the data
    if (col %in% names(data)) {
      # Check if max of column > 1 and scale if necessary
      if (max(data[,col], na.rm = TRUE) > 1) {
        data[,col] <- data[,col] / 100
      }
    }
  }
  return(data)
}

source('src/getRawData.R')
source('src/getMainData.R')

# UPDATE 1: Wednesday 22nd November, 2023.
# Adding 2023Q3 PTR and pi_gap
data.ts["2023-09-01", "PTR"] = 2.22
data.ts["2023-09-01", "pi_gap"] = data.ts["2023-09-01", "pi"] - data.ts["2023-09-01", "PTR"]
data.ts <- scaleData(data.ts)




## -----------------------------------------------------------------------------
## STEP 1: Replicate ACM (2013) results on US data:
source("mains/mainTrendyTS.R")
source("src/auxFuncs.R")


X.var.ACM <- X.var
termpremia.ts.ACM <- termpremia.ts
MP.ts.ACM <- y_rf.ts.fit
yield.ts.ACM <- y.ts.fit


pred_list_ACM <- preparePredList("ACM")


## -----------------------------------------------------------------------------
## STEP 2: Our alternative model
source("mains/mainDetrendedTS.R")
source("src/auxFuncs.R")
X.var.FF <- X.var
termpremia.ts.FF <- termpremia.ts
yield.ts.FF <- y.ts.fit
MP.ts.FF <- y.ts.fit - termpremia.ts.FF

pred_list_FF <- preparePredList("FF")
