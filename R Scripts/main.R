#Get all the data files ready
source("./R Scripts/Getting Data.R")

#Fetch the function for building database
source("./R Scripts/Build_Database.R")

#A list for the yields of all the different countries
#The list elements are dataframes
long_yield_list <- list(Y_G_10,Y_F_10,Y_S_10,Y_I_10)

#Run a loop to compute term premia for the different countries
for (z in 1:length(long_yield_list)){
  #Call the function to build the database for the current country
  df <- build_df(long_yield_list[z])
  
  #Run the scripts for computing term premia using our model & consesnsus forecasts
  source("./R Scripts/Model for expected rates.R")
  source("./R Scripts/Using Updated Consensus Forecasts.R")
  
  #Make and save the plots for the different countries in separate files
  if (z==1){png("./Plots/Yield_decomposition_Germany.png")}
  if (z==2){png("./Plots/Yield_decomposition_France.png")}
  if (z==3){png("./Plots/Yield_decomposition_Spain.png")}
  if (z==4){png("./Plots/Yield_decomposition_Italy.png")}
  plot(df$Date,df$Yield, type = "l", ylab = "Yield & Composition", xlab = "Date",
       main = "Decomposition of yield", ylim = c(min(df$Yield, na.rm = TRUE)-2,max(df$Yield,na.rm = TRUE)))
  lines(df$Date,df$TP, lty = "dashed", col = "red",lwd = 2)
  lines(df$Date,(df$Yield - df$TP), lty = "dashed", col = "blue")
  lines(df$Date,rep(0,length(df$Date)), lty = "dotted")
  points(df$Date,df$TP_cf, lwd = 2, pch = 20)
  legend("topright",
         legend = c("Bond Yield","Term Premia","Short-term","Consensus_TP"),
         lty = c("solid","dashed","dashed",NA),
         col = c("black","red","blue","black"),
         pch = c(NA,NA,NA,20),
         lwd = c(1,2,1,2)
  )
  dev.off()
}


