#clear the environment 
rm(list=ls()) 
## ------------------------------------------------------------------------
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
## -------

# load packages 
#We use the lubridate package for (partial) date conversions
#dplyr for table manipulations
# check highcharter which is the equivalent of ggplot for data in .xts format 
listofpackages = c("lubridate","zoo","ggplot2","dygraphs","plyr","dplyr","tidyverse", "highcharter")

for (j in listofpackages){
  if(sum(installed.packages()[, 1] == j) == 0) {
    install.packages(j)
  }
  library(j, character.only = T)
}


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
  png(paste("../Plots/Historical/Yield_decomposition_",country_list[z],".png",sep=""))
  plot(df$Date,df$Spread, type = "l", ylab = "Yield & Composition", xlab = "Date",
       main = paste("Decomposition of yield for",country_list[z],sep = " "),
       ylim = c(-2,7))
  lines(df$Date,df$TP, lty = "dashed", col = "red",lwd = 2)
  #lines(df$Date,(df$Spread - df$TP), lty = "dashed", col = "blue")
  lines(df$Date,rep(0,length(df$Date)), lty = "dotted")
  points(df$Date,df$TP_cf, lwd = 2, pch = 20)
  legend("topright",
         legend = c("Term Spread","Term Premia","Consensus_TP"),
         lty = c("solid","dashed",NA),
         col = c("black","red","black"),
         pch = c(NA,NA,20),
         lwd = c(1,2,2)
  )
  dev.off()
  
  #Make dataframe for storing model term premia estimates
  if (z==1){
    TP_df <- as.data.frame(cbind(df$Date,df$sum_Er,df$TP))
    TP_df_cf <- as.data.frame(cbind(df$Date,df$sum_Er,df$TP_cf))
    Y_df <- as.data.frame(cbind(df$Date,df$Yield))
  }
  else{
    TP_df <- cbind(TP_df,df$TP)
    TP_df_cf <- cbind(TP_df_cf,df$TP_cf)
    Y_df <- as.data.frame(cbind(Y_df,df$Yield))
  }
  
}

#Tables for the different countries---------------------------------------------
for (i in 1:length(country_list)){
  temp <- paste("df",country_list[i],sep = "_")
  assign(temp,df_list[[i]])
}



#Tables for term premia and yields-----------------------------------------------
colnames(TP_df) <- c("Date","sum_Er","Germany","France","Spain","Italy")
colnames(TP_df_cf) <- c("Date","sum_Er","Germany","France","Spain","Italy")
colnames(Y_df) <- c("Date","Germany","France","Spain","Italy")
TP_df$Date <- as.Date(TP_df$Date)
TP_df_cf$Date <- as.Date(TP_df_cf$Date)
Y_df$Date <- as.Date(Y_df$Date)


#Cross-country comparison plot of 10 YEAR YIELDS--------------------------------
png("../Plots/Historical/Long Term Government Bond Yields.png")
plot(Y_df$Date,Y_df$Germany, type = "l", col = "blue", lwd = 2, 
     main = "10-Y Government Bond Yields", ylab = "Yield", xlab = "Date",
     ylim = c(min(Y_df$Germany, na.rm = TRUE),max(Y_df$Spain,na.rm = TRUE)+0.5)
)
lines(Y_df$Date,Y_df$France, col = "brown", lwd = 2)
lines(Y_df$Date,Y_df$Spain, col = "green", lwd = 2)
lines(Y_df$Date,Y_df$Italy, col = "red", lwd = 2)
abline(h = 0, lty = "dotted")

legend("topright",
       legend = c("Germany","France","Spain","Italy"),
       col = c("blue","brown","green","red"),
       lwd = c(2,2,2,2),
       lty = c("solid","solid","solid","solid")
)
dev.off()


#Cross-country comparison plot of TERM PREMIA ESTIMATES-------------------------

#Obtained using the PREDICTIVE MODEL
png("../Plots/Historical/Model Term Premia estimates.png")
plot(TP_df$Date,TP_df$Germany, type = "l", col = "blue", lwd = 2, 
     main = "Term Premia Estimates", ylab = "Term Premia", xlab = "Date",
     ylim = c(min(TP_df$Germany, na.rm = TRUE),max(TP_df$Spain,na.rm = TRUE))
)
lines(TP_df$Date,TP_df$France, col = "brown", lwd = 2)
lines(TP_df$Date,TP_df$Spain, col = "green", lwd = 2)
lines(TP_df$Date,TP_df$Italy, col = "red", lwd = 2)
abline(h = 0, lty = "dotted")

legend("topright",
       legend = c("Germany","France","Spain","Italy"),
       col = c("blue","brown","green","red"),
       lwd = c(2,2,2,2),
       lty = c("solid","solid","solid","solid")
)
dev.off()


#Obtained using CONSENSUS FORECASTS

TP_df_cf <- drop_na(TP_df_cf)
png("../Plots/Historical/Consensus Term Premia estimates.png")
plot(TP_df_cf$Date,TP_df_cf$Germany, col = "blue", pch = 20, type = "b",
     main = "Term Premia Estimates", ylab = "Term Premia", xlab = "Date",
     ylim = c(min(TP_df_cf$Germany, na.rm = TRUE),max(TP_df_cf$Spain,na.rm = TRUE)),
     xlim = c(min(TP_df_cf$Date),max(TP_df_cf$Date)+365)
)
points(TP_df_cf$Date,TP_df_cf$France, col = "brown", pch = 20, type = "b")
points(TP_df_cf$Date,TP_df_cf$Spain, col = "green", pch = 20, type = "b")
points(TP_df_cf$Date,TP_df_cf$Italy, col = "red", pch = 20, type = "b")
abline(h=0, lty = "dotted")
legend("topright",
       legend = c("Germany","France","Spain","Italy"),
       col = c("blue","brown","green","red"),
       pch = c(20,20,20,20)
)
dev.off()

