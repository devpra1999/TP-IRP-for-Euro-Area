#clear the environment 
rm(list=ls()) 
## ------------------------------------------------------------------------
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
## -------

# load packages 
#We use the lubridate package for (partial) date conversions
#dplyr for table manipulations
# check highcharter which is the equivalent of ggplot for data in .xts format 
listofpackages = c("lubridate","zoo","ggplot2","dygraphs","plyr","dplyr","tidyverse",
                   "highcharter","webshot","htmlwidgets")

for (j in listofpackages){
  if(sum(installed.packages()[, 1] == j) == 0) {
    install.packages(j)
  }
  library(j, character.only = T)
}
library(highcharter)

#Get all the DATA FILES ready - long yields, short rates, consensus forecasts
source("Getting Data.R")

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
  df <- build_df(long_yield_list[z],Y_st)
  
  #Run the scripts for COMPUTING TERM PREMIA using our model & consensus forecasts
  source("Model for expected rates.R")
  source("Using Updated Consensus Forecasts.R")
  
  #Save the table for future reference
  df_list[[z]] <- df
  
  #Make and save the PLOTS for the different countries in separate files
  hc <- highchart() %>%
    hc_add_series(df, "line", hcaes(x = Date, y = Spread), name = "Term Spread") %>%
    hc_add_series(df, "line", hcaes(x = Date, y = TP),
                  name = "Term Premia", dashStyle = "dash", color = "red", lineWidth = 2) %>%
    hc_add_series(df, "scatter", hcaes(x = Date, y = TP_cf),
                  name = "Consensus_TP", marker = list(symbol = "circle", lineWidth = 0, radius = 2)) %>%
    hc_title(text = paste("Decomposition of yield for", country_list[z])) %>%
    hc_xAxis(type = "datetime", title = list(text = "Date")) %>%
    hc_yAxis(title = list(text = "Yield & Composition")) %>%
    hc_legend(enabled = TRUE)
}
  #hc <- hc %>% hc_exporting(enabled = TRUE)
  #htmlwidgets::saveWidget(widget = hc,
  #                        file = paste("../Plots/Historical/",country_list[z],".html",sep=""),
  #                        selfcontained = TRUE)
  #webshot::webshot(url = paste("../Plots/Historical/",country_list[z],".html",sep=""), 
  #                 file = paste("../Plots/Historical/Yield_Decomposition_",country_list[z],".png",sep=""),
  #                 delay = 2)
  

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
col_order <- c("Date","Rate","Yield_Germany","Spread_Germany","TP_Germany","TP_cf_Germany",
               "Yield_France","Spread_France","TP_France","TP_cf_France",
               "Yield_Spain","Spread_Spain","TP_Spain","TP_cf_Spain",
               "Yield_Italy","Spread_Italy","TP_Italy","TP_cf_Italy",
               "sum_Er","sum_Er_cf","l1","l2","L1_forecast","L2_forecast","L3_forecast","L4_forecast")
master_df <- master_df[,col_order]


#Cross-country comparison plot of 10 YEAR YIELDS--------------------------------
gg <- ggplot(master_df, aes(x = Date)) +
  geom_line(aes(y = Yield_Germany, color = "Germany")) +
  geom_line(aes(y = Yield_France, color = "France")) +
  geom_line(aes(y = Yield_Spain, color = "Spain")) +
  geom_line(aes(y = Yield_Italy, color = "Italy")) +
  labs(title = "10-Y Government Bond Yields", x = "Date", y = "Yield") +
  ylim(min(master_df$Yield_Germany, na.rm = TRUE), max(master_df$Yield_Spain, na.rm = TRUE) + 0.5) +
  geom_hline(yintercept = 0, linetype = "dotted") +
  theme_bw() +
  theme(legend.position = "top", legend.title = element_blank(), plot.title = element_text(hjust = 0.5)) +
  scale_color_manual(
    values = c("blue", "brown", "green", "red"),
    labels = c("Germany", "France", "Spain", "Italy")
  )

# Save the ggplot as a PNG image file
ggsave("../Plots/Historical/Long Term Government Bond Yields.png", gg, width = 10, height = 6, dpi = 300)


#Cross-country comparison plot of TERM PREMIA ESTIMATES-------------------------

#Obtained using the PREDICTIVE MODEL
gg <- ggplot(master_df, aes(x = Date)) +
  geom_line(aes(y = TP_Germany, color = "Germany")) +
  geom_line(aes(y = TP_France, color = "France")) +
  geom_line(aes(y = TP_Spain, color = "Spain")) +
  geom_line(aes(y = TP_Italy, color = "Italy")) +
  labs(title = "Term Premia Estimates", x = "Date", y = "Term Premia") +
  ylim(min(master_df$TP_Germany, na.rm = TRUE), max(master_df$Yield_Spain, na.rm = TRUE)) +
  geom_hline(yintercept = 0, linetype = "dotted") +
  scale_color_manual(
    values = c("blue", "brown", "green", "red"),
    labels = c("Germany", "France", "Spain", "Italy")
  ) +
  theme_bw() +
  theme(legend.position = "top", legend.title = element_blank(), plot.title = element_text(hjust = 0.5))

ggsave("../Plots/Historical/Model Term Premia estimates.png", gg, width = 10, height = 6, dpi = 300)


#Obtained using CONSENSUS FORECASTS

master_df_cf_filtered <- master_df

# Filter out NA values from the duplicate dataset
master_df_cf_filtered <- master_df_cf_filtered %>%
  filter(!is.na(TP_cf_Germany),
         !is.na(TP_cf_France),
         !is.na(TP_cf_Spain),
         !is.na(TP_cf_Italy))

# Create a ggplot object
gg <- ggplot(master_df_cf_filtered, aes(x = Date)) +
  geom_line(aes(y = TP_cf_Germany, color = "Germany"), size = 0.5) +
  geom_line(aes(y = TP_cf_France, color = "France"), size = 0.5) +
  geom_line(aes(y = TP_cf_Spain, color = "Spain"), size = 0.5) +
  geom_line(aes(y = TP_cf_Italy, color = "Italy"), size = 0.5) +
  geom_point(aes(y = TP_cf_Germany, color = "Germany"), shape = 20) +
  geom_point(aes(y = TP_cf_France, color = "France"), shape = 20) +
  geom_point(aes(y = TP_cf_Spain, color = "Spain"), shape = 20) +
  geom_point(aes(y = TP_cf_Italy, color = "Italy"), shape = 20) +
  labs(title = "Term Premia Estimates", x = "Date", y = "Term Premia") +
  ylim(min(master_df$TP_cf_Germany, na.rm = TRUE), max(master_df$TP_cf_Spain, na.rm = TRUE)) +
  xlim(min(master_df$Date), max(master_df$Date) + 365) +
  geom_hline(yintercept = 0, linetype = "dotted") +
  theme_bw() +
  theme(legend.position = "top", legend.title = element_blank(), plot.title = element_text(hjust = 0.5)) +
  scale_color_manual(
    values = c("blue", "brown", "green", "red"),
    labels = c("Germany", "France", "Spain", "Italy")
  )

ggsave("../Plots/Historical/Consensus Term Premia estimates.png", gg, width = 10, height = 6, dpi = 300)

