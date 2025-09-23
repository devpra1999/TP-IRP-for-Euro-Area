## -----------------------------------------------------------------------------
#rm(list=ls())                                                                   # Clear the environment 

## -----------------------------------------------------------------------------
#setwd(dirname(rstudioapi::getActiveDocumentContext()$path))                     # Set the appropriate environment to work in every machine

## -----------------------------------------------------------------------------
#listofpackages <- c("psych","pracma","zoo","lubridate","dplyr","highcharter",
#                    "ggplot2","matrixcalc","matlib","xlsx","readxl","readr","xts",
#                    "vars", "systemfit", "stargazer", "tidyverse", "xtable", "xts", "zoo")
#source("aux_scripts/preparePackages.R")
#preparePackages(listofpackages)
#rm(listofpackages, preparePackages)



## -----------------------------------------------------------------------------
## STEP 1: Replicate ACM (2013) results on US data :
source("US/trendyTS.R")
source("US/auxil.R")

#plotTermStructure()
#plotHPER()

X.var.ACM <- X.var
termpremia.ts.ACM <- termpremia.ts
MP.ts.ACM <- y_rf.ts.fit
yield.ts.ACM <- y.ts.fit

#tableXvar(X.var.ACM, "ACM")
#R2.table.ACM <- tableR2(system_hper_r_fit, system_hper_unr_fit)
#R2.table.ACM$model <- "ACM"
#write.csv(R2.table.ACM, file="data/aux/R2_table_ACM.csv", row.names = FALSE)

# Principal Components (PC)
#plotPC("ACM")

#for (year in c(1,10)){
#  plotYieldvsTrend(year*4, "ACM")
#  plotYieldvsFittedYield(year*4, "ACM")
#}

# Out of Sample Forecasts
pred_list_ACM <- preparePredList("ACM")
#pred_plot <- plotOoSYields(pred_list, "ACM")

# Term premia
#plotTermPremia("ACM")


## -----------------------------------------------------------------------------
## STEP 2: Our alternative model
source("US/detrendedTS.R")
source("US/auxil.R")
X.var.FF <- X.var
termpremia.ts.FF <- termpremia.ts
yield.ts.FF <- y.ts.fit
MP.ts.FF <- y.ts.fit - termpremia.ts.FF


#tableXvar(X.var.FF, "FF")
#R2.table.FF <- tableR2(system_hper_r_fit, system_hper_unr_fit)
#R2.table.FF$model <- "FF"
#write.csv(R2.table.FF, file="data/aux/R2_table_FF.csv", row.names=FALSE)

# Principal Components (PC)
#plotPC("FF")

# Actual vs Yield and Yield vs Trend
#for (year in c(1,10)){
#  plotYieldvsTrend(year*4, "FF")
#plotYieldvsFittedYield(year*4, "FF")
#}

# Out of Sample Forecasts
pred_list_FF <- preparePredList("FF")
#pred.plot <- plotOoSYields(pred_list, "FF")

# Term premia
#plotTermPremia("FF")


## -----------------------------------------------------------------------------
## STEP 3: Merge and save
#rm(list=ls())
#source("auxil.R")
#R2.table.ACM <- read.csv("data/aux/R2_table_ACM.csv")
#R2.table.FF <- read.csv("data/aux/R2_table_FF.csv")


#R2.table <- rbind(R2.table.ACM, R2.table.FF)
#tableR2toLATEX(R2.table)


## -----------------------------------------------------------------------------
## STEP 4: Plot all forecasts together

#plotForecasts()

#### -----------------------------------------------------------------------------
## TESTS:

