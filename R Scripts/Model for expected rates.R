#We'll start with a simple model dy_t+1 = rho*dy_t + e_t+1
#Or equivalently dy_t = rho*dy_t-1 + e_t

#Estimation of coefficient for the lag (rho)
mod <- lm(df$l1 ~ df$l2 - 1)
rho = mod$coefficients[1]

#Forecasts for the expected short rates using rho
#How many time periods -> T
T = 40

#An array to store the cumulative sum of expected short rate movements
sum_Er <- rep(0,length(df$l1))

#A variable to store the current rate movement
#Will be used to estimate the future rate movement using our rho parameter
curr <- df$l1

#Loop to find the total weighted expected sum of movements
for (i in 1:T){
  sum_Er <- sum_Er + (1-i/T)*rho*curr
  curr <- rho*curr
}

#Compute the term premia
df$TP <- df$Spread - sum_Er

#Compute the common expected short term rate component of the yield
df$ESTR <- df$Rate + sum_Er


#Plotting the term premia
#plot(df$Date,df$TP, type = "l", ylab = "TP", xlab = "Date",
#     main = "Term Premia")
#lines(df$Date,rep(0,length(df$Date)), lty = "dotted")

#Decomposing the long term yield
#plot(df$Date,df$Yield, type = "l", ylab = "Yield & Composition", xlab = "Date",
#     main = "Decomposition of yield", ylim = c(min(df$Yield,na.rm=TRUE)-2,max(df$Yield,na.rm=TRUE)))
#lines(df$Date,df$TP, lty = "dashed", col = "red", lwd = 2)
#lines(df$Date,(df$Yield - df$TP), lty = "dashed", col = "blue")
#lines(df$Date,rep(0,length(df$Date)), lty = "dotted")
#legend("topright",
#       legend = c("Bond Yield","Term Premia","Short-term"),
#       lty = c("solid","dashed","dashed"),
#       col = c("black","red","blue"),
#       lwd = c(1,2,1)
#)




