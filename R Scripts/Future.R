#Create subset for the period 2015-2023 which we will plot
temp <- df %>% filter(Date >= "2018-01-01") %>% filter(Date <= "2023-04-01")
temp <- drop_na(temp,Yield)
lr <- tail(temp,1) #Last Row

png("./Plots/Future/Interest Rate Evolution.png")
#Plot historical rate data for the subsetted period
plot(temp$Date, temp$Rate, type = "l", xlim = c(as.Date("2018-01-01"),as.Date("2025-04-01")),
     ylim = c(-1,5), lwd = 2)
abline(v = tail(temp,1)$Date, lty = "dashed")
abline(h = 0, lty = "dotted")


#Create vectors for future date, interest rate movements and interest rates -  using model
fut_date <- seq(lr$Date, as.Date("2025-04-01"), by = "quarter")
n <- length(fut_date)
fut_move_mod <- rep(NA,n)
fut_rate_mod <- rep(NA,n)
move_curr <- lr$l1
rate_curr <- lr$Rate
for (i in 1:n){
  fut_move_mod[i] <- move_curr
  fut_rate_mod[i] <- rate_curr
  move_curr <- rho*move_curr
  rate_curr <- rate_curr + move_curr
}

#Plotting expected short term rates according to model
lines(fut_date,fut_rate_mod, lty = "dashed", lwd = 2, col = "blue")


#Data for consensus forecasts for interest rates
fut_rate_consensus <- rep(NA,n)
fut_rate_consensus[2:5] <- as.numeric(as.vector(lr[10:13]))

#Plotting expected short term rates according to consensus forecasts
points(fut_date,fut_rate_consensus, pch = 20, lwd = 2, col = "red")

legend("topleft",
       legend = c("Historical","Model Estimate","Consensus Forecast"),
       col = c("black","blue","red"),
       lwd = c(2,2,2),
       lty = c("solid","dashed",NA),
       pch = c(NA,NA,20)
)
dev.off()

#Term premia estimates
TP_mod <- lr$TP
TP_consensus <- lr$TP_cf

