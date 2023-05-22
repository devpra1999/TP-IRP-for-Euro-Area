#Get all the DATA FILES ready - long yields, short rates, consensus forecasts
source("./R Scripts/Getting Data.R")

#Fetch the function for building database - each country has a different database
source("./R Scripts/Build_Database.R")

#A list for the yields of all the different countries
#The list elements are data frames
long_yield_list <- list(Y_G_10,Y_F_10,Y_S_10,Y_I_10)
country_list <- c("Germany","France","Spain","Italy")

#Run a loop to compute term premia for the different countries
for (z in 1:length(long_yield_list)){
  #Call the function to build the database for the current country
  df <- build_df(long_yield_list[z])
  
  #Run the scripts for COMPUTING TERM PREMIA using our model & consensus forecasts
  source("./R Scripts/Model for expected rates.R")
  source("./R Scripts/Using Updated Consensus Forecasts.R")
  
  #Make and save the PLOTS for the different countries in separate files
  png(paste("./Plots/Yield_decomposition_",country_list[z],".png",sep=""))
  plot(df$Date,df$Spread, type = "l", ylab = "Yield & Composition", xlab = "Date",
       main = paste("Decomposition of yield for",country_list[z],sep = " "),
       ylim = c(min(df$Spread, na.rm = TRUE)-0.5,max(df$Spread,na.rm = TRUE)+1.5))
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
    TP_df <- as.data.frame(cbind(df$Date,df$ESTR,df$TP_cf))
    TP_df_cf <- as.data.frame(cbind(df$Date,df$ESTR,df$TP_cf))
    }
  else{
    TP_df <- cbind(TP_df,df$TP)
    TP_df_cf <- cbind(TP_df_cf,df$TP_cf)
  }
  
}

colnames(TP_df) <- c("Date","ESTR","Germany","France","Spain","Italy")
colnames(TP_df_cf) <- c("Date","ESTR","Germany","France","Spain","Italy")
TP_df$Date <- as.Date(TP_df$Date)
TP_df_cf$Date <- as.Date(TP_df_cf$Date)

#Make a cross-country comparison plot of term premia estimates

#Obtained using the predictive model
png("./Plots/Model Term Premia estimates.png")
plot(TP_df$Date,TP_df$Germany, type = "l", col = "blue", lwd = 2, 
     main = "Term Premia Estimates", ylab = "Term Premia", xlab = "Date",
     ylim = c(min(TP_df$Germany, na.rm = TRUE),max(TP_df$Spain,na.rm = TRUE))
)
lines(TP_df$Date,TP_df$France, col = "brown", lwd = 2)
lines(TP_df$Date,TP_df$Spain, col = "green", lwd = 2)
lines(TP_df$Date,TP_df$Italy, col = "red", lwd = 2)

lines(TP_df$Date,rep(0,length(TP_df$Date)), lty = "dotted")
legend("topright",
       legend = c("Germany","France","Spain","Italy"),
       col = c("blue","brown","green","red"),
       lwd = c(2,2,2,2),
       lty = c("solid","solid","solid","solid")
)
dev.off()

TP_df_cf <- drop_na(TP_df_cf)
#Obtained using consensus forecasts
png("./Plots/Consensus Term Premia estimates.png")
plot(TP_df_cf$Date,TP_df_cf$Germany, col = "blue", pch = 20, type = "b",
     main = "Term Premia Estimates", ylab = "Term Premia", xlab = "Date",
     ylim = c(min(TP_df_cf$Germany, na.rm = TRUE),max(TP_df_cf$Spain,na.rm = TRUE))
)
points(TP_df_cf$Date,TP_df_cf$France, col = "brown", pch = 20, type = "b")
points(TP_df_cf$Date,TP_df_cf$Spain, col = "green", pch = 20, type = "b")
points(TP_df_cf$Date,TP_df_cf$Italy, col = "red", pch = 20, type = "b")
lines(TP_df_cf$Date,rep(0,length(TP_df_cf$Date)), pch = "dotted")
legend("topright",
       legend = c("Germany","France","Spain","Italy"),
       col = c("blue","brown","green","red"),
       pch = c(20,20,20,20)
)
dev.off()
