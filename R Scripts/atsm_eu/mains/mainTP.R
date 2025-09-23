load("atsm_eu/data/rda/10y_big4.rda")
load("atsm_eu/data/rda/main.rda")

# Phi = phi
# X.ts = X
# # Define the inputs: A1, B1, Phi, Mu, and the size n for the identity matrix I
# n <- 40
# MP <- 0
# #result <- predict(X.var, n.ahead = n )
# #X.for.ts <- cbind(
# #    result$fcst$PC1[, "fcst"],
# #    result$fcst$PC2[, "fcst"],
# #    result$fcst$PC3[, "fcst"],
# #    result$fcst$PC4[, "fcst"],
# #    result$fcst$PC5[, "fcst"]
# #    )
# #
# #for j in c(1:n){
# #MP <- -A[1] -  t(X.for.ts[n,]) %*% B[,1]
# #}
# 
# # Create the identity matrix I with dimensions based on Phi
# I <- diag(nrow(Phi))
# 
# # Compute the terms of the equation
# Term1 <- - 1/n * A[1]*4
# 
# # Calculate the inverse term and the third term of the equation
# sumPhis <- matrix(0, nrow = nrow(Phi), ncol = ncol(Phi))
# for (i in 1:(n-1)) {
#   sumPhis <- sumPhis + matrix.power(Phi,i)
# }
# 
# # Initialize the summation term
# Term2 <- matrix(0, nrow = 1, ncol = ncol(t(X.ts)))
# for (i in 1:n) {
#   scalar_part <- t(B[,1]) %*% sumPhis %*% mu
#   scalar_part <- matrix(scalar_part, nrow = 1, ncol = dim(t(X.ts))[2])
#   Term2 <- Term2 +  scalar_part + t(B[,1]) %*% matrix.power(Phi,i) %*% t(X.ts)
# }
# Term2 <- -1/n * Term2 * 4
# 
# 
# 
# # Combine all terms to get the result
# MP <- Term1 + as.numeric(Term2)

#Computing model expected short rates
#We make a Txn matrix with each row containing 1 to "n" quarters ahead model implied rates
n <- 40 #Time till which forecasts are made
T <- nrow(X)
ESTR.FF <- matrix(0,T,n)
for (i in 1:n){
  ESTR.FF[,i] <- (delta0 + t(delta1 %*% matrix.power(phi,i) %*% t(X)))/ttm[1]
  #Correction term - adding it does not make a difference - accounts for mu
  correction <- delta1 %*% mu
  for (j in 1:(i-1)){
    correction <- correction + delta1 %*% matrix.power(phi,j) %*% mu
  }
  ESTR.FF[,i] <- ESTR.FF[,i] + matrix(correction,1,T)/ttm[1]
}

#Monetary Policy Component from FF model as per our methodology 
#For the current cyclic rate we need to subtract the trend from the fitted yields at the 1st period
MP <- (apply(ESTR.FF[,1:39],1,sum) + fittedYields.FF[,1] - y_trend.matrix[1:length(plot_dates.FF),1])/40 
MP.ts <- xts(MP, order.by=as.Date(current_dates))
MP.ts <- MP.ts + y_trend.matrix[1:length(current_dates),1]

#The MP.ts above is the average of   r_1,t to r_1,t+39
#The MP.2.ts below is the average of r_1,t+1 to r_1,t+40

MP <- apply(ESTR.FF,1,mean)
MP.2.ts <- xts(MP, order.by=as.Date(current_dates))
#Need to add either the average trend or the trend at period 1
MP.2.ts <- MP.2.ts + apply(y_trend.matrix[1:length(current_dates),1:40],1,mean)
# MP.2.ts <- MP.2.ts + y_trend.matrix[1:length(current_dates),1]


# Print the result
plot(MP.ts)
TP.ts <- na.omit(sweep(big4_y.10Y.ts[index(X)], 1, RiskFreeYields.FF.ts[,40]*100, FUN = "-"))
colnames(TP.ts) <- gsub("_n10","", colnames(TP.ts))

data_frame <- data.frame(Date=index(TP.ts), as.data.frame(coredata(TP.ts)) %>% select(DE,FR,ES,IT))

# Now you can melt your data frame
data_long <- melt(data_frame, id.vars = "Date")

# Proceed with the ggplot code
ggplot(data_long, aes(x = Date, y = value, color = variable, group = variable)) +
  geom_line() +
  theme_minimal() +
  scale_color_manual(values = c("ES" = "red", "FR" = "blue", "DE" = "black", "IT" = "green4")) +
  labs(x = "", y = "", title = "") +
  #scale_x_date(date_breaks = "4 years", date_labels = "%Y") +
  scale_x_date(limits = range(data_long$Date), date_breaks = "4 years", date_labels = "%Y") +
  theme(legend.position = "bottom")


termpremia_Germany.FF = filter(data_long,variable == "DE")$value
termpremia_France.FF = filter(data_long,variable == "FR")$value
termpremia_Spain.FF = filter(data_long,variable == "ES")$value
termpremia_Italy.FF = filter(data_long,variable == "IT")$value

start <- ymd(plot_dates.FF[T])

#End date - "Y" years ahead
Y <- 3
end <- ymd(plot_dates.FF[T]) %m+% period(paste(Y,"years"))
plot_dates_proj.FF <- seq(as.Date(start),as.Date(end), length.out = Y*4 + 1)


X.ts = xts(X,current_dates)
train_period <- "2001-03-01/2023-06-01"
forecast_window.length <- floor((2033-2023)*4)
forecast_window.dates <-seq(as.Date("2023-09-01"),length=forecast_window.length, by="quarters")

X_train.ts  <- X.ts["2001-03-01/2023-06-01"]
X_train.var <- VAR(X_train.ts, p = 1, type = "const")
X_train.for <- predict(X_train.var, n.ahead = forecast_window.length)
X_in.mat    <- fitted(X_train.var)
X_for.mat   <- cbind(X_train.for$fcst$PC1[,1],X_train.for$fcst$PC2[,1],X_train.for$fcst$PC3[,1],
                     X_train.for$fcst$PC4[,1],X_train.for$fcst$PC5[,1])
dates <- c(index(X_train.ts),forecast_window.dates)
pred_list <- list()
n = 1
y_cycle_OoS <- t(-(A[n]+t(B[, n])%*%t(X_for.mat))/ttm[n])
y_cycle_IN  <- t(-(A[n]+t(B[, n])%*%t(X_in.mat))/ttm[n])

# Convert prediction to xts time series
y_cycle_OoS.ts <- xts((y_cycle_OoS), order.by = forecast_window.dates) # assuming dates_for remains constant
y_cycle_IN.ts <- xts(y_cycle_IN, order.by = index(X_train.ts)[-1])
y_OoS.ts <- merge(y_cycle_OoS.ts, y_trend.ts[,n],join="outer")
y_IN.ts  <- merge(y_cycle_IN.ts, y_trend.ts[,n],join="outer")

colnames(y_OoS.ts) <- c("cycle", "trend")
colnames(y_IN.ts) <- c("cycle", "trend")

y_OoS.ts <- y_OoS.ts$cycle + y_OoS.ts$trend
y_IN.ts  <- y_IN.ts$cycle + y_IN.ts$trend
pred.ts <- xts(merge(y.ts[dates, paste0("n", n)], na.omit(y_OoS.ts), join = 'outer'), dates)
pred.ts <- xts(merge(pred.ts, na.omit(y_IN.ts), join = 'outer'), dates)
pred.ts <- pred.ts["2001-03-01/2033-06-01"]

# Set column names
colnames(pred.ts) <- c(paste0("n", n), paste0("n", n, "_OoS"), paste0("n", n, "_IN"))
pred.ts.FF <- pred.ts
