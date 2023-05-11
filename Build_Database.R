#Building Database
#Merge 10 year bond yield and short term rate
df <- merge(Y_G_10,Y_st, by = "Date")
df <- df %>% select(Date, Yield, Rate) %>% filter(Date >= "1999-01-01")

#Compute the spreads (long bond yield - short term rate)
df$Spread <- df$Yield - df$Rate
plot(df$Date,df$Spread, type = "l")
lines(df$Date,rep(0,length(df$Date)), lty = "dotted")

#Compute the first difference of short-term rates (dy_t+1)
#Time period taken = dt
dt = 1
df$l1 <- df$Rate - lag(df$Rate,dt)
df$l2 <- lag(df$Rate,dt) - lag(df$Rate,2*dt)
