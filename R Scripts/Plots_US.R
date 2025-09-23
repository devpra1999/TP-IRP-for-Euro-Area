s1 <- as.data.frame(cbind(index(yield.ts.ACM),as.numeric(yield.ts.ACM[,40])*100))
names(s1) <- c("x","y")
s1$x <- as.Date(s1$x)
s1$y <- round(s1$y,2)
s2 <- as.data.frame(cbind(index(MP.ts.ACM),as.numeric(MP.ts.ACM[,40])*100))
names(s2) <- c("x","y")
s2$x <- as.Date(s2$x)
s2$y <- round(s2$y,2)
s3 <- as.data.frame(cbind(index(termpremia.ts.ACM),as.numeric(termpremia.ts.ACM[,40])*100))
names(s3) <- c("x","y")
s3$x <- as.Date(s3$x)
s3$y <- round(s3$y,2)
s4 <- as.data.frame(cbind(index(termpremia.ts.ACM),rep(0,length(index(termpremia.ts.ACM)))))
names(s4) <- c("x","y")
s4$x <- as.Date(s4$x)
#s5 <- as.data.frame(cbind(plot_dates,MP_ACM*100))
#names(s5) <- c("x","y")
#s5$x <- as.Date(s5$x)
#s5$y <- round(s5$y,2)
US_ACM_Decomp <- highchart() %>%
  hc_add_series(s1, "line", hcaes(x, y), name = "Model Implied Yield", color = "black", marker = list(enabled = FALSE)) %>%
  hc_add_series(s2, "line", hcaes(x, y), name = "MP Component", color = "red", marker = list(enabled = FALSE)) %>%
  hc_add_series(s3, "line", hcaes(x, y), name = "Term Premia", color = "blue", marker = list(enabled = FALSE)) %>%
  hc_add_series(s4, "line", hcaes(x, y), name = "", dashStyle = "dot", color = "black",
                showInLegend = FALSE) %>%
  #hc_add_series(s5, "line", hcaes(x, y), name = "Monetary Policy", color = "grey",
  #              dashStyle = "dash") %>%
  hc_title(text = "10Y Yield Decomposition using ACM Model - US") %>%
  hc_xAxis(type = "datetime", title = list(text = "Date"), gridLineWidth = 1) %>%
  hc_yAxis(title = list(text = "Yield & Term Premia"), min = min(s1$y,s2$y,s3$y), max = max(s1$y,s2$y,s3$y)) %>%
  hc_navigator(enabled = TRUE) %>%
  hc_scrollbar(enabled = TRUE) %>%
  hc_legend(enabled = TRUE) %>%
  hc_boost(enabled = TRUE) %>%
  hc_exporting(enabled = TRUE)


s1 <- as.data.frame(cbind(index(yield.ts.FF),as.numeric(yield.ts.FF[,40])*100))
names(s1) <- c("x","y")
s1$x <- as.Date(s1$x)
s1$y <- round(s1$y,2)
s2 <- as.data.frame(cbind(index(MP.ts.FF),as.numeric(MP.ts.FF[,40])*100))
names(s2) <- c("x","y")
s2$x <- as.Date(s2$x)
s2$y <- round(s2$y,2)
s3 <- as.data.frame(cbind(index(termpremia.ts.FF),as.numeric(termpremia.ts.FF[,40])*100))
names(s3) <- c("x","y")
s3$x <- as.Date(s3$x)
s3$y <- round(s3$y,2)
s4 <- as.data.frame(cbind(index(termpremia.ts.FF),rep(0,length(index(termpremia.ts.FF)))))
names(s4) <- c("x","y")
s4$x <- as.Date(s4$x)
#s5 <- as.data.frame(cbind(plot_dates,MP_ACM*100))
#names(s5) <- c("x","y")
#s5$x <- as.Date(s5$x)
#s5$y <- round(s5$y,2)
US_FF_Decomp <- highchart() %>%
  hc_add_series(s1, "line", hcaes(x, y), name = "Model Implied Yield", color = "black", marker = list(enabled = FALSE)) %>%
  hc_add_series(s2, "line", hcaes(x, y), name = "MP Companent", color = "red", marker = list(enabled = FALSE)) %>%
  hc_add_series(s3, "line", hcaes(x, y), name = "Term Premia", color = "blue", marker = list(enabled = FALSE)) %>%
  hc_add_series(s4, "line", hcaes(x, y), name = "", dashStyle = "dot", color = "black",
                showInLegend = FALSE) %>%
  #hc_add_series(s5, "line", hcaes(x, y), name = "Monetary Policy", color = "grey",
  #              dashStyle = "dash") %>%
  hc_title(text = "10Y Yield Decomposition using FF Model - US") %>%
  hc_xAxis(type = "datetime", title = list(text = "Date"), gridLineWidth = 1) %>%
  hc_yAxis(title = list(text = "Yield & Term Premia"), min = min(s1$y,s2$y,s3$y), max = max(s1$y,s2$y,s3$y)) %>%
  hc_navigator(enabled = TRUE) %>%
  hc_scrollbar(enabled = TRUE) %>%
  hc_legend(enabled = TRUE) %>%
  hc_boost(enabled = TRUE) %>%
  hc_exporting(enabled = TRUE)


US_FOMC_Decomp <- highchart() %>%
  hc_add_series(US_data[complete.cases(US_data), ], "line", hcaes(Date, Yield), name = "Yield",
                color = "black", marker = list(symbol = "circle", radius = 2)) %>%
  hc_add_series(US_data[complete.cases(US_data), ], "line", hcaes(Date, sum_Er_cf), name = "MP Component", 
                color = "red", marker = list(symbol = "circle", radius = 2)) %>%
  hc_add_series(US_data[complete.cases(US_data), ], "line", hcaes(Date, TP), name = "Term Premia", 
                color = "blue", marker = list(symbol = "circle", radius = 2)) %>%
  hc_add_series(US_data[complete.cases(US_data), ], "line", hcaes(Date, rep(0, length(Date))), name = "", dashStyle = "dot", 
                color = "black", marker = list(symbol = "circle", radius = 0.1), showInLegend = FALSE) %>%
  hc_title(text = "10Y Yield Decomposition using FOMC Forecasts - US") %>%
  hc_xAxis(type = "datetime", title = list(text = "Date"), gridLineWidth = 1) %>%
  hc_yAxis(title = list(text = "Yield & Term Premia"), min = -2, max = 6) %>%
  # hc_navigator(enabled = TRUE) %>%
  # hc_scrollbar(enabled = TRUE) %>%
  hc_legend(enabled = TRUE) %>%
  hc_boost(enabled = TRUE) %>%
  hc_exporting(enabled = TRUE)


s1 <- as.data.frame(cbind(index(termpremia.ts.ACM),as.numeric(termpremia.ts.ACM[,40])*100))
names(s1) <- c("x","y")
s1$x <- as.Date(s1$x)
s1$y <- round(s1$y,2)
s1 <- s1 %>% filter(x >= as.Date("2010-01-01"))
s2 <- as.data.frame(cbind(index(termpremia.ts.FF),as.numeric(termpremia.ts.FF[,40])*100))
names(s2) <- c("x","y")
s2$x <- as.Date(s2$x)
s2$y <- round(s2$y,2)
s2 <- s2 %>% filter(x >= as.Date("2010-01-01"))
s3 <- US_data %>% select(Date,TP)
names(s3) <- c("x","y")
s3$x <- as.Date(s3$x)
s4 <- as.data.frame(cbind(s1$x,rep(0,length(s1$x))))
names(s4) <- c("x","y")
s4$x <- as.Date(s4$x)
US_TP_ACM_FF_FOMC <- highchart() %>%
  hc_add_series(s1, "line", hcaes(x, y), name = "ACM TP", color = "red", marker = list(enabled = FALSE)) %>%
  hc_add_series(s2, "line", hcaes(x, y), name = "FF TP", color = "blue", marker = list(enabled = FALSE)) %>%
  hc_add_series(s3, "line", hcaes(x, y), name = "FOMC TP", color = "green", marker = list(symbol = "circle", radius = 3)) %>%
  hc_add_series(s4, "line", hcaes(x, y), name = "", dashStyle = "dot", color = "black",
                marker = list(symbol = "circle", lineWidth = 0, radius = 0.1), showInLegend = FALSE) %>%
  hc_title(text = "ACM vs FF vs FOMC 10Y Term Premia - US") %>%
  hc_xAxis(type = "datetime", title = list(text = "Date"), gridLineWidth = 1) %>%
  hc_yAxis(title = list(text = "Term Premia"), min = min(s1$y,s2$y,s3$y), max = max(s1$y,s2$y,s3$y)) %>%
  hc_navigator(enabled = TRUE) %>%
  hc_scrollbar(enabled = TRUE) %>%
  hc_legend(enabled = TRUE) %>%
  hc_boost(enabled = TRUE) %>%
  hc_exporting(enabled = TRUE)

s1 <- as.data.frame(cbind(index(MP.ts.ACM),as.numeric(MP.ts.ACM[,40])*100))
names(s1) <- c("x","y")
s1$x <- as.Date(s1$x)
s1$y <- round(s1$y,2)
s1 <- s1 %>% filter(x >= as.Date("2010-01-01"))
s2 <- as.data.frame(cbind(index(MP.ts.FF),as.numeric(MP.ts.FF[,40])*100))
names(s2) <- c("x","y")
s2$x <- as.Date(s2$x)
s2$y <- round(s2$y,2)
s2 <- s2 %>% filter(x >= as.Date("2010-01-01"))
s3 <- US_data %>% select(Date,sum_Er_cf)
names(s3) <- c("x","y")
s3$x <- as.Date(s3$x)
s4 <- as.data.frame(cbind(s1$x,rep(0,length(s1$x))))
names(s4) <- c("x","y")
s4$x <- as.Date(s4$x)
US_MP_ACM_FF_FOMC <- highchart() %>%
  hc_add_series(s1, "line", hcaes(x, y), name = "ACM MP", color = "red") %>%
  hc_add_series(s2, "line", hcaes(x, y), name = "FF MP", color = "blue") %>%
  hc_add_series(s3, "line", hcaes(x, y), name = "FOMC MP", color = "green") %>%
  hc_add_series(s4, "line", hcaes(x, y), name = "", dashStyle = "dot", color = "black",
                marker = list(symbol = "circle", lineWidth = 0, radius = 0.1), showInLegend = FALSE) %>%
  hc_title(text = "ACM vs FF vs FOMC 10Y Monetary Policy - US") %>%
  hc_xAxis(type = "datetime", title = list(text = "Date"), gridLineWidth = 1) %>%
  hc_yAxis(title = list(text = "Monetary Policy Component"), min = min(s1$y,s2$y,s3$y), max = max(s1$y,s2$y,s3$y)) %>%
  hc_navigator(enabled = TRUE) %>%
  hc_scrollbar(enabled = TRUE) %>%
  hc_legend(enabled = TRUE) %>%
  hc_boost(enabled = TRUE) %>%
  hc_exporting(enabled = TRUE)

#Actual recent
#s1 <- as.data.frame(cbind(tail(index(na.omit(l_ACM$n1$n1)),12),as.numeric(tail(na.omit(l_ACM$n1$n1),12))*100))
s1 <- US_3M
names(s1) <- c("x","y")
s1$x <- as.Date(s1$x)
s1$y <- round(s1$y*100,2)
#ACM-fitted recent
s2 <- as.data.frame(cbind(tail(index(na.omit(l_ACM$n1$n1_IN)),12),as.numeric(tail(na.omit(l_ACM$n1$n1_IN),12))*100))
names(s2) <- c("x","y")
s2$x <- as.Date(s2$x)
s2$y <- round(s2$y,2)
#FF-fitted recent
s3 <- as.data.frame(cbind(tail(index(na.omit(l_FF$n1$n1_IN)),12),as.numeric(tail(na.omit(l_FF$n1$n1_IN),12))*100))
names(s3) <- c("x","y")
s3$x <- as.Date(s3$x)
s3$y <- round(s3$y,2)
#ACM-predicted
x <- c(index(tail(na.omit(l_ACM$n1$n1_IN),1)),index(na.omit(l_ACM$n1$n1_OoS)))
y <- c(tail(na.omit(l_ACM$n1$n1_IN),1)*100,na.omit(l_ACM$n1$n1_OoS)*100)
s4 <- as.data.frame(cbind(x,as.numeric(y)))
names(s4) <- c("x","y")
s4$x <- as.Date(s4$x)
s4$y <- round(s4$y,2)
#FF-predicted
x <- c(index(tail(na.omit(l_FF$n1$n1_IN),1)),index(na.omit(l_FF$n1$n1_OoS)))
y <- c(tail(na.omit(l_FF$n1$n1_IN),1)*100,na.omit(l_FF$n1$n1_OoS)*100)
s5 <- as.data.frame(cbind(x,as.numeric(y)))
names(s5) <- c("x","y")
s5$x <- as.Date(s5$x)
s5$y <- round(s5$y,2)
#FOMC forecasts
s6 <- as.data.frame(cbind(FOMC_dates,FOMC_proj))
names(s6) <- c("x","y")
s6$x <- as.Date(s6$x)
s6$y <- round(s6$y,2)

Projections_US <- highchart() %>%
  hc_add_series(s1, "line", hcaes(x, y), name = "Actual Recent", color = "black") %>%
  hc_add_series(s2, "line", hcaes(x, y), name = "ACM fitted", color = "orange") %>%
  hc_add_series(s3, "line", hcaes(x, y), name = "FF fitted", color = "blue") %>%
  hc_add_series(s4, "line", hcaes(x, y), name = "ACM forecast", dashStyle = "dash", color = "orange") %>%
  hc_add_series(s5, "line", hcaes(x, y), name = "FF forecast", dashStyle = "dash", color = "blue") %>%
  hc_add_series(s6, "line", hcaes(x, y), name = "FOMC forecast", dashStyle = "dash", color = "green") %>%
  hc_title(text = "Projected Short-Term Rates") %>%
  hc_xAxis(type = "datetime", title = list(text = "Date"), gridLineWidth = 1) %>%
  hc_yAxis(title = list(text = "3M Rates"), min = -1, max = 6) %>%
  hc_legend(enabled = TRUE) %>%
  hc_boost(enabled = TRUE) %>%
  hc_exporting(enabled = TRUE)



