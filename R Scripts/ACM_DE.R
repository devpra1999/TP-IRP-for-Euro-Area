N = 60
source("NSS.R")
L <- nss_yields("Data_Files/gnss.csv",N*3)
rawYields <- L$rawYields/100
plot_dates <- L$plot_dates
Yields <- as.matrix(rawYields)
Yields <- xts(Yields,plot_dates)
Yields <- Yields[seq(from = 3, to = 285, by = 3),seq(from = 3, to = N*3, length.out = N)]
colnames(Yields) <- paste("n",seq(1,N),sep = "_")
T = nrow(Yields)


Yields_dt <- sweep(Yields, 1, Yields$n_1, FUN = "-")
plot(Yields[,c("n_4","n_10","n_20","n_30","n_40")])
plot(Yields_dt[,c("n_4","n_10","n_20","n_30","n_40")])
rm(L,rawYields,Yields_dt)


