#l_ACM <- pred_list_ACM
#l_FF <- pred_list_FF

lr_US <- tail(US_data %>% drop_na(V1),1)
FOMC_dates <- seq(lr_US$Date, as.Date(lr_US$Date + years(9)), by = "year")
FOMC_proj <- as.numeric(lr_US[,3:12])

#US_3M <- read_csv("Data_Files/US_3_month.csv", col_names = c("Date","Yield_3M"), na = ".", col_types = "Dn")
US_3M <- data.frame(DATE=index(data_US.ts), coredata(data_US.ts)) %>% filter(DATE >= "2020-09-01") %>% select(DATE,n_1)
colnames(US_3M) <- c("Date","Yield_3M")


Methods <- c("ACM","FF","FOMC")
Yield_10Y <- c(as.numeric(tail(yield.ts.ACM[,40],1)*100),
                 as.numeric(tail(yield.ts.FF[,40],1)*100), lr_US$Yield)
Term_Premia <- c(as.numeric(tail(termpremia.ts.ACM[,40],1)*100),
                   as.numeric(tail(termpremia.ts.FF[,40],1)*100), lr_US$TP)
MP_Component <- c(as.numeric(tail(MP.ts.ACM[,40],1)*100),
                   as.numeric(tail(MP.ts.FF[,40],1)*100), lr_US$sum_Er_cf)
main_table_US <- data.frame(Methods,Yield_10Y,Term_Premia,MP_Component)
