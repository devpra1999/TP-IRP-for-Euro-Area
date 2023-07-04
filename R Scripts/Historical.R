
#Get all the DATA FILES ready - long yields, short rates, consensus forecasts
#source("Getting Data.R")

#Read all the data files
source("Loading Data.R")

#Fetch the function for building database - each country has a different database
source("Build_Database.R")

#A list for the yields of all the different countries
#The list elements are data frames
long_yield_list <- list(Y_G_10,Y_F_10,Y_S_10,Y_I_10)
country_list <- c("Germany","France","Spain","Italy")
df_list <- vector(mode = "list", length = length(country_list))

#Run a loop to compute term premia for the different countries
for (z in 1:length(long_yield_list)){
  #Call the function to build the database for the current country
  L <- build_df(long_yield_list[z],Y_st)
  df <- L$main_data
  Rate_df <- L$rate_data
  #Run the scripts for COMPUTING TERM PREMIA using our model & consensus forecasts
  source("Model for expected rates.R")
  source("Using Updated Consensus Forecasts.R")
  
  #Save the table for future reference
  df_list[[z]] <- df
}

#Tables for the different countries---------------------------------------------
#CHANGE COLUMN NAMES TO THE RESPECTIVE COUNTRY
for (i in 1:length(country_list)){
  df_list[[i]] <- plyr::rename(df_list[[i]],c(Yield = paste("Yield",country_list[i],sep="_"),
                                          Spread = paste("Spread",country_list[i],sep="_"),
                                          TP = paste("TP",country_list[i],sep="_"),
                                          TP_cf = paste("TP_cf",country_list[i],sep="_")))
  
  temp <- paste("df",country_list[i],sep = "_")
  assign(temp,df_list[[i]])
}

#MASTER DATASET-------------------------------------------------------------------
master_df <- Reduce(function (...) { merge(..., by = , all = FALSE) },df_list)
col_order <- c("Date", "Rate", "sum_Er", "sum_Er_cf","Yield_Germany", "Spread_Germany",
               "TP_Germany", "TP_cf_Germany", "Yield_France", "Spread_France", "TP_France",
               "TP_cf_France", "Yield_Spain", "Spread_Spain", "TP_Spain", "TP_cf_Spain",
               "Yield_Italy", "Spread_Italy", "TP_Italy", "TP_cf_Italy", "Q1_forecast",
               "Q2_forecast", "Q3_forecast", "Q4_forecast", "Q5_forecast", "Q6_forecast",
               "Q7_forecast", "Q8_forecast", "Q9_forecast", "Q10_forecast", "Q11_forecast",
               "Q12_forecast")

master_df <- master_df[,col_order]
master_df <- master_df %>% filter(!is.na(Yield_Germany),
                                  !is.na(Yield_France),
                                  !is.na(Yield_Spain),
                                  !is.na(Yield_Italy))

#rm(list=ls()[! ls() %in% c("master_df","Rate_df", "mod", "macro_table")])


