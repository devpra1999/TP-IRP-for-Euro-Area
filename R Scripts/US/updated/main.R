
## -----------------------------------------------------------------------------
## Load the dataset if not existing
source("src/checkMainData.R")
prepareMainRDA()


# UPDATE 1: Wednesday 22nd November, 2023.
# Adding 2023Q3 PTR and pi_gap
data.ts["2023-09-01", "PTR"] = 2.22
data.ts["2023-09-01", "pi_gap"] = data.ts["2023-09-01", "pi"] - data.ts["2023-09-01", "PTR"]
save(data.ts, file="data/main.rda")

## -----------------------------------------------------------------------------
## STEP 1: Replicate ACM (2013) results on US data:
source("mains/mainTrendyTS.R")
source("src/auxFuncs.R")

X.var.ACM <- X.var
termpremia.ts.ACM <- termpremia.ts
MP.ts.ACM <- y_rf.ts.fit
yield.ts.ACM <- y.ts.fit

# Out of Sample Forecasts
pred_list_ACM <- preparePredList("ACM")


## -----------------------------------------------------------------------------
## STEP 2: Our alternative model
source("mains/mainDetrendedTS.R")
source("src/auxFuncs.R")
X.var.FF <- X.var
termpremia.ts.FF <- termpremia.ts
yield.ts.FF <- y.ts.fit
MP.ts.FF <- y.ts.fit - termpremia.ts.FF

# Out of Sample Forecasts
pred_list_FF <- preparePredList("FF")
