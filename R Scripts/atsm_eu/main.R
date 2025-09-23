

# ## -----------------------------------------------------------------------------
# ## STEP 1: Estimate the Term Structure Model
# source("atsm_eu/mains/mainTS.R")
# 
# ## -----------------------------------------------------------------------------
# ## STEP 2: Measure the term premium for the biggest 4 countries
# source("atsm_eu/mains/mainTP.R")

source("atsm_eu/src/getMainData.R")
rm(list=ls()[! ls() %in% c("big4_y.10Y.ts")])

source("ACM.R")
source("atsm_eu/EA_new.R")
plot_dates.FF = current_dates
X.FF.ts <- X_EA.ts
termpremia.FF.ts <- termpremia_EA.ts
yield.ts.FF <- fittedYields_EA.ts
MP.ts.FF <- yield.ts.FF - termpremia.FF.ts
RiskFreeYields.FF.ts <- MP.ts.FF
RiskFreeYields.FF <- MP.ts.FF
TP.ts.FF <- na.omit(sweep(big4_y.10Y.ts[index(X.FF.ts)], 1, MP.ts.FF[,40]*100, FUN = "-"))
colnames(TP.ts.FF) <- gsub("_n10","", colnames(TP.ts.FF))

data_frame <- data.frame(Date=index(TP.ts.FF), as.data.frame(coredata(TP.ts.FF)) %>% select(DE,FR,ES,IT))

# Now you can melt your data frame
data_long <- melt(data_frame, id.vars = "Date")

termpremia_Germany.FF = filter(data_long.ACM,variable == "DE")$value
termpremia_France.FF = filter(data_long.ACM,variable == "FR")$value
termpremia_Spain.FF = filter(data_long.ACM,variable == "ES")$value
termpremia_Italy.FF = filter(data_long.ACM,variable == "IT")$value


