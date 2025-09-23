s1 <- select(master_df,Date,Yield_Germany)
names(s1) <- c("x","y")
s2 <- select(master_df,Date,Yield_France)
names(s2) <- c("x","y")
s3 <- select(master_df,Date,Yield_Spain)
names(s3) <- c("x","y")
s4 <- select(master_df,Date,Yield_Italy)
names(s4) <- c("x","y")
Yield_Plot <- highchart() %>%
  hc_title(text = "10-Y Government Bond Yields") %>%
  hc_xAxis(type = "datetime", dateTimeLabelFormats = list(month = "%b %Y"), gridLineWidth = 1) %>%
  hc_yAxis(title = list(text = "Yield")) %>%
  hc_add_series(data = s1, hcaes(x, y), type = "line", color = "blue", name = "Germany") %>%
  hc_add_series(data = s2, hcaes(x, y), type = "line", color = "brown", name = "France") %>%
  hc_add_series(data = s3, hcaes(x, y), type = "line", color = "green", name = "Spain") %>%
  hc_add_series(data = s4, hcaes(x, y), type = "line", color = "red", name = "Italy") %>%
  hc_plotOptions(series = list(marker = list(enabled = FALSE))) %>%
  hc_navigator(enabled = TRUE) %>%
  hc_scrollbar(enabled = TRUE) %>%
  hc_legend(layout = "horizontal", align = "center", verticalAlign = "top") %>%
  hc_colors(c("blue", "brown", "green", "red")) %>%
  hc_boost(enabled = TRUE) %>%
  hc_exporting(enabled = TRUE)

s1 <- select(master_df,Date,Yield_Germany)
names(s1) <- c("x","y")
s2 <- select(master_df,Date,TP_cf_Germany)
names(s2) <- c("x","y")
s2$y <- round(s2$y,2)
s3 <- master_df %>% select(Date,Rate,sum_Er_cf) %>% mutate(MPC = Rate + sum_Er_cf) %>% select(Date,MPC)
names(s3) <- c("x","y")
s3$y <- round(s3$y,2)
s4 <- master_df %>% select(Date) %>% mutate(y = rep(0,length(Date)))
names(s4) <- c("x","y")
Germany_Term_Premia <- highchart() %>%
  hc_add_series(s1, "line", hcaes(x, y), name = "Yield", color = "black",
                marker = list(symbol = "circle", lineWidth = 0, radius = 2)) %>%
  hc_add_series(s2, "line", hcaes(x, y),
                name = "Consensus Term Premia", color = "blue",connectNulls = TRUE, dashStyle = "dash",
                marker = list(symbol = "circle", lineWidth = 0, radius = 2)) %>%
  hc_add_series(s3, "line", hcaes(x, y),
                name = "Monetary Policy Component", dashStyle = "dash", color = "red",
                marker = list(symbol = "circle", lineWidth = 0, radius = 2), connectNulls = TRUE) %>%
  hc_add_series(s4, "line", hcaes(x, y),
                name = "", dashStyle = "dot", color = "black", marker = list(enabled = FALSE),
                showInLegend = FALSE) %>%
  hc_title(text = "10Y Yield Decomposition using ECB SPF Forecasts - Germany") %>%
  hc_xAxis(type = "datetime", title = list(text = "Date"), gridLineWidth = 1) %>%
  hc_yAxis(title = list(text = "Yield & Term Premia"), min = -1, max = 7) %>%
  # hc_navigator(enabled = TRUE) %>%
  # hc_scrollbar(enabled = TRUE) %>%
  hc_legend(enabled = TRUE) %>%
  hc_boost(enabled = TRUE) %>%
  hc_exporting(enabled = TRUE)

s1 <- select(master_df,Date,Yield_France)
names(s1) <- c("x","y")
s2 <- select(master_df,Date,TP_cf_France)
names(s2) <- c("x","y")
s2$y <- round(s2$y,2)
s3 <- master_df %>% select(Date,Rate,sum_Er_cf) %>% mutate(MPC = Rate + sum_Er_cf) %>% select(Date,MPC)
names(s3) <- c("x","y")
s3$y <- round(s3$y,2)
s4 <- master_df %>% select(Date) %>% mutate(y = rep(0,length(Date)))
names(s4) <- c("x","y")
France_Term_Premia <- highchart() %>%
  hc_add_series(s1, "line", hcaes(x, y), name = "Yield", color = "black",
                marker = list(symbol = "circle", lineWidth = 0, radius = 2)) %>%
  hc_add_series(s2, "line", hcaes(x, y),
                name = "Consensus Term Premia", color = "blue",connectNulls = TRUE, dashStyle = "dash",
                marker = list(symbol = "circle", lineWidth = 0, radius = 2)) %>%
  hc_add_series(s3, "line", hcaes(x, y),
                name = "Monetary Policy Component", dashStyle = "dash", color = "red",
                marker = list(symbol = "circle", lineWidth = 0, radius = 2), connectNulls = TRUE) %>%
  hc_add_series(s4, "line", hcaes(x, y),
                name = "", dashStyle = "dot", color = "black", marker = list(enabled = FALSE),
                showInLegend = FALSE) %>%
  hc_title(text = "France") %>%
  hc_xAxis(type = "datetime", title = list(text = "Date"), gridLineWidth = 1) %>%
  hc_yAxis(title = list(text = "Yield & Term Premia"), min = -1, max = 7) %>%
  # hc_navigator(enabled = TRUE) %>%
  # hc_scrollbar(enabled = TRUE) %>%
  hc_legend(enabled = TRUE) %>%
  hc_boost(enabled = TRUE) %>%
  hc_exporting(enabled = TRUE)

s1 <- select(master_df,Date,Yield_Italy)
names(s1) <- c("x","y")
s2 <- select(master_df,Date,TP_cf_Italy)
names(s2) <- c("x","y")
s2$y <- round(s2$y,2)
s3 <- master_df %>% select(Date,Rate,sum_Er_cf) %>% mutate(MPC = Rate + sum_Er_cf) %>% select(Date,MPC)
names(s3) <- c("x","y")
s3$y <- round(s3$y,2)
s4 <- master_df %>% select(Date) %>% mutate(y = rep(0,length(Date)))
names(s4) <- c("x","y")
Italy_Term_Premia <- highchart() %>%
  hc_add_series(s1, "line", hcaes(x, y), name = "Yield", color = "black",
                marker = list(symbol = "circle", lineWidth = 0, radius = 2)) %>%
  hc_add_series(s2, "line", hcaes(x, y),
                name = "Consensus Term Premia", color = "blue",connectNulls = TRUE, dashStyle = "dash",
                marker = list(symbol = "circle", lineWidth = 0, radius = 2)) %>%
  hc_add_series(s3, "line", hcaes(x, y),
                name = "Monetary Policy Component", dashStyle = "dash", color = "red",
                marker = list(symbol = "circle", lineWidth = 0, radius = 2), connectNulls = TRUE) %>%
  hc_add_series(s4, "line", hcaes(x, y),
                name = "", dashStyle = "dot", color = "black", marker = list(enabled = FALSE),
                showInLegend = FALSE) %>%
  hc_title(text = "10Y Yield Decomposition using ECB SPF Forecasts - Italy") %>%
  hc_xAxis(type = "datetime", title = list(text = "Date"), gridLineWidth = 1) %>%
  hc_yAxis(title = list(text = "Yield & Term Premia"), min = -1, max = 7) %>%
  # hc_navigator(enabled = TRUE) %>%
  # hc_scrollbar(enabled = TRUE) %>%
  hc_legend(enabled = TRUE) %>%
  hc_boost(enabled = TRUE) %>%
  hc_exporting(enabled = TRUE)


s1 <- select(master_df,Date,Yield_Spain)
names(s1) <- c("x","y")
s2 <- select(master_df,Date,TP_cf_Spain)
names(s2) <- c("x","y")
s2$y <- round(s2$y,2)
s3 <- master_df %>% select(Date,Rate,sum_Er_cf) %>% mutate(MPC = Rate + sum_Er_cf) %>% select(Date,MPC)
names(s3) <- c("x","y")
s3$y <- round(s3$y,2)
s4 <- master_df %>% select(Date) %>% mutate(y = rep(0,length(Date)))
names(s4) <- c("x","y")
Spain_Term_Premia <- highchart() %>%
  hc_add_series(s1, "line", hcaes(x, y), name = "Yield", color = "black",
                marker = list(symbol = "circle", lineWidth = 0, radius = 2)) %>%
  hc_add_series(s2, "line", hcaes(x, y),
                name = "Consensus Term Premia", color = "blue",connectNulls = TRUE, dashStyle = "dash",
                marker = list(symbol = "circle", lineWidth = 0, radius = 2)) %>%
  hc_add_series(s3, "line", hcaes(x, y),
                name = "Monetary Policy Component", dashStyle = "dash", color = "red",
                marker = list(symbol = "circle", lineWidth = 0, radius = 2), connectNulls = TRUE) %>%
  hc_add_series(s4, "line", hcaes(x, y),
                name = "", dashStyle = "dot", color = "black", marker = list(enabled = FALSE),
                showInLegend = FALSE) %>%
  hc_title(text = "Spain") %>%
  hc_xAxis(type = "datetime", title = list(text = "Date"), gridLineWidth = 1) %>%
  hc_yAxis(title = list(text = "Yield & Term Premia"), min = -1, max = 7) %>%
  # hc_navigator(enabled = TRUE) %>%
  # hc_scrollbar(enabled = TRUE) %>%
  hc_legend(enabled = TRUE) %>%
  hc_boost(enabled = TRUE) %>%
  hc_exporting(enabled = TRUE)

#The monetary policy component so derived has peaks at two places due to the
#peaks in the 1 month yields in NSS (and subsequently ACM fitted 1 month yields) 

#ACM

#Germany ACM TP
s1 <- as.data.frame(cbind(plot_dates.ACM,as.numeric(big4_y.10Y.ts[plot_dates.ACM]$DE_n10)))
names(s1) <- c("x","y")
s1$x <- as.Date(s1$x)
s1$y <- round(s1$y,2)
s2 <- as.data.frame(cbind(as.Date(plot_dates.ACM),as.numeric(RiskFreeYields.ACM.ts[,40]*100)))
names(s2) <- c("x","y")
s2$x <- as.Date(s2$x)
s2$y <- round(s2$y,2)
s3 <- as.data.frame(cbind(as.Date(plot_dates.ACM),termpremia_Germany.ACM))
names(s3) <- c("x","y")
s3$x <- as.Date(s3$x)
s3$y <- round(s3$y,2)
s4 <- as.data.frame(cbind(as.Date(plot_dates.ACM),rep(0,length(plot_dates.ACM))))
names(s4) <- c("x","y")
s4$x <- as.Date(s4$x)

Germany_Term_Premia_ACM <- highchart() %>%
  hc_add_series(s1, "line", hcaes(x, y), name = "10Y Yield", color = "black") %>%
  hc_add_series(s2, "line", hcaes(x, y), name = "Risk Neutral 10Y Yield", color = "red") %>%
  hc_add_series(s3, "line", hcaes(x, y), name = "10Y Term Premia", color = "blue") %>%
  hc_add_series(s4, "line", hcaes(x, y), name = "", dashStyle = "dot", color = "black",
                showInLegend = FALSE) %>%
  hc_title(text = "10Y Yield Decomposition using ACM Model - Germany") %>%
  hc_xAxis(type = "datetime", title = list(text = "Date"), gridLineWidth = 1) %>%
  hc_yAxis(title = list(text = "Yield & Term Premia"), min = -1, max = 7) %>%
  hc_navigator(enabled = TRUE) %>%
  hc_scrollbar(enabled = TRUE) %>%
  hc_legend(enabled = TRUE) %>%
  hc_boost(enabled = TRUE) %>%
  hc_exporting(enabled = TRUE)

#France ACM TP
s1 <- as.data.frame(cbind(plot_dates.ACM,as.numeric(big4_y.10Y.ts[plot_dates.ACM]$FR_n10)))
names(s1) <- c("x","y")
s1$x <- as.Date(s1$x)
s1$y <- round(s1$y,2)
s2 <- as.data.frame(cbind(as.Date(plot_dates.ACM),as.numeric(RiskFreeYields.ACM.ts[,40]*100)))
names(s2) <- c("x","y")
s2$x <- as.Date(s2$x)
s2$y <- round(s2$y,2)
s3 <- as.data.frame(cbind(as.Date(plot_dates.ACM),termpremia_France.ACM))
names(s3) <- c("x","y")
s3$x <- as.Date(s3$x)
s3$y <- round(s3$y,2)
s4 <- as.data.frame(cbind(as.Date(plot_dates.ACM),rep(0,length(plot_dates.ACM))))
names(s4) <- c("x","y")
s4$x <- as.Date(s4$x)

France_Term_Premia_ACM <- highchart() %>%
  hc_add_series(s1, "line", hcaes(x, y), name = "10Y Yield", color = "black") %>%
  hc_add_series(s2, "line", hcaes(x, y), name = "Risk Neutral 10Y Yield", color = "red") %>%
  hc_add_series(s3, "line", hcaes(x, y), name = "10Y Term Premia", color = "blue") %>%
  hc_add_series(s4, "line", hcaes(x, y), name = "", dashStyle = "dot", color = "black",
                showInLegend = FALSE) %>%
  hc_title(text = "10Y Yield Decomposition using ACM Model - France") %>%
  hc_xAxis(type = "datetime", title = list(text = "Date"), gridLineWidth = 1) %>%
  hc_yAxis(title = list(text = "Yield & Term Premia"), min = -1, max = 7) %>%
  hc_navigator(enabled = TRUE) %>%
  hc_scrollbar(enabled = TRUE) %>%
  hc_legend(enabled = TRUE) %>%
  hc_boost(enabled = TRUE) %>%
  hc_exporting(enabled = TRUE)


#Spain ACM TP
s1 <- as.data.frame(cbind(plot_dates.ACM,as.numeric(big4_y.10Y.ts[plot_dates.ACM]$ES_n10)))
names(s1) <- c("x","y")
s1$x <- as.Date(s1$x)
s1$y <- round(s1$y,2)
s2 <- as.data.frame(cbind(as.Date(plot_dates.ACM),as.numeric(RiskFreeYields.ACM.ts[,40]*100)))
names(s2) <- c("x","y")
s2$x <- as.Date(s2$x)
s2$y <- round(s2$y,2)
s3 <- as.data.frame(cbind(as.Date(plot_dates.ACM),termpremia_Spain.ACM))
names(s3) <- c("x","y")
s3$x <- as.Date(s3$x)
s3$y <- round(s3$y,2)
s4 <- as.data.frame(cbind(as.Date(plot_dates.ACM),rep(0,length(plot_dates.ACM))))
names(s4) <- c("x","y")
s4$x <- as.Date(s4$x)

Spain_Term_Premia_ACM <- highchart() %>%
  hc_add_series(s1, "line", hcaes(x, y), name = "10Y Yield", color = "black") %>%
  hc_add_series(s2, "line", hcaes(x, y), name = "Risk Neutral 10Y Yield", color = "red") %>%
  hc_add_series(s3, "line", hcaes(x, y), name = "10Y Term Premia", color = "blue") %>%
  hc_add_series(s4, "line", hcaes(x, y), name = "", dashStyle = "dot", color = "black",
                showInLegend = FALSE) %>%
  hc_title(text = "10Y Yield Decomposition using ACM Model - Spain") %>%
  hc_xAxis(type = "datetime", title = list(text = "Date"), gridLineWidth = 1) %>%
  hc_yAxis(title = list(text = "Yield & Term Premia"), min = -1, max = 7) %>%
  hc_navigator(enabled = TRUE) %>%
  hc_scrollbar(enabled = TRUE) %>%
  hc_legend(enabled = TRUE) %>%
  hc_boost(enabled = TRUE) %>%
  hc_exporting(enabled = TRUE)

#Italy ACM TP
s1 <- as.data.frame(cbind(plot_dates.ACM,as.numeric(big4_y.10Y.ts[plot_dates.ACM]$IT_n10)))
names(s1) <- c("x","y")
s1$x <- as.Date(s1$x)
s1$y <- round(s1$y,2)
s2 <- as.data.frame(cbind(as.Date(plot_dates.ACM),as.numeric(RiskFreeYields.ACM.ts[,40]*100)))
names(s2) <- c("x","y")
s2$x <- as.Date(s2$x)
s2$y <- round(s2$y,2)
s3 <- as.data.frame(cbind(as.Date(plot_dates.ACM),termpremia_Italy.ACM))
names(s3) <- c("x","y")
s3$x <- as.Date(s3$x)
s3$y <- round(s3$y,2)
s4 <- as.data.frame(cbind(as.Date(plot_dates.ACM),rep(0,length(plot_dates.ACM))))
names(s4) <- c("x","y")
s4$x <- as.Date(s4$x)

Italy_Term_Premia_ACM <- highchart() %>%
  hc_add_series(s1, "line", hcaes(x, y), name = "10Y Yield", color = "black") %>%
  hc_add_series(s2, "line", hcaes(x, y), name = "Risk Neutral 10Y Yield", color = "red") %>%
  hc_add_series(s3, "line", hcaes(x, y), name = "10Y Term Premia", color = "blue") %>%
  hc_add_series(s4, "line", hcaes(x, y), name = "", dashStyle = "dot", color = "black",
                showInLegend = FALSE) %>%
  hc_title(text = "10Y Yield Decomposition using ACM Model - Italy") %>%
  hc_xAxis(type = "datetime", title = list(text = "Date"), gridLineWidth = 1) %>%
  hc_yAxis(title = list(text = "Yield & Term Premia"), min = -1, max = 7) %>%
  hc_navigator(enabled = TRUE) %>%
  hc_scrollbar(enabled = TRUE) %>%
  hc_legend(enabled = TRUE) %>%
  hc_boost(enabled = TRUE) %>%
  hc_exporting(enabled = TRUE)

#FF
#s1 <- as.data.frame(cbind(index(y_EU.ts.FF),as.numeric(y_EU.ts.FF[,40])*100))
#names(s1) <- c("x","y")
#s1$x <- as.Date(s1$x)
#s1$y <- round(s1$y,2)
#s2 <- as.data.frame(cbind(index(MP_EU.ts.FF),as.numeric(MP_EU.ts.FF[,40])*100))
#names(s2) <- c("x","y")
#s2$x <- as.Date(s2$x)
#s2$y <- round(s2$y,2)
#s3 <- as.data.frame(cbind(index(termpremia_EU.ts.FF),as.numeric(termpremia_EU.ts.FF[,40])*100))
#names(s3) <- c("x","y")
#s3$x <- as.Date(s3$x)
#s3$y <- round(s3$y,2)
#s4 <- as.data.frame(cbind(index(y_EU.ts.FF),rep(0,dim(y_EU.ts.FF)[1])))
#names(s4) <- c("x","y")
#s4$x <- as.Date(s4$x)

#Germany FF TP
# s1 <- as.data.frame(cbind(plot_dates.FF,as.numeric(big4_y.10Y.ts[plot_dates.FF]$DE_n10)))
# names(s1) <- c("x","y")
# s1$x <- as.Date(s1$x)
# s1$y <- round(s1$y,2)
# s2 <- as.data.frame(cbind(as.Date(plot_dates.FF),as.numeric(RiskFreeYields.FF[,40]*100)))
# names(s2) <- c("x","y")
# s2$x <- as.Date(s2$x)
# s2$y <- round(s2$y,2)
# s3 <- as.data.frame(cbind(as.Date(plot_dates.FF),termpremia_Germany.FF))
# names(s3) <- c("x","y")
# s3$x <- as.Date(s3$x)
# s3$y <- round(s3$y,2)
# s4 <- as.data.frame(cbind(as.Date(plot_dates.FF),rep(0,length(plot_dates.FF))))
# names(s4) <- c("x","y")
# s4$x <- as.Date(s4$x)
# 
# Germany_Term_Premia_FF <- highchart() %>%
#   hc_add_series(s1, "line", hcaes(x, y), name = "10Y Yield", color = "black") %>%
#   hc_add_series(s2, "line", hcaes(x, y), name = "Risk Neutral 10Y Yield", color = "red") %>%
#   hc_add_series(s3, "line", hcaes(x, y), name = "10Y Term Premia", color = "blue") %>%
#   hc_add_series(s4, "line", hcaes(x, y), name = "", dashStyle = "dot", color = "black",
#                 showInLegend = FALSE) %>%
#   hc_title(text = "10Y Yield Decomposition using FF Model - Germany") %>%
#   hc_xAxis(type = "datetime", title = list(text = "Date"), gridLineWidth = 1) %>%
#   hc_yAxis(title = list(text = "Yield & Term Premia"), min = -1, max = 7) %>%
#   hc_navigator(enabled = TRUE) %>%
#   hc_scrollbar(enabled = TRUE) %>%
#   hc_legend(enabled = TRUE) %>%
#   hc_boost(enabled = TRUE) %>%
#   hc_exporting(enabled = TRUE)
# 
# #France FF TP
# s1 <- as.data.frame(cbind(plot_dates.FF,as.numeric(big4_y.10Y.ts[plot_dates.FF]$FR_n10)))
# names(s1) <- c("x","y")
# s1$x <- as.Date(s1$x)
# s1$y <- round(s1$y,2)
# s2 <- as.data.frame(cbind(as.Date(plot_dates.FF),as.numeric(RiskFreeYields.FF[,40]*100)))
# names(s2) <- c("x","y")
# s2$x <- as.Date(s2$x)
# s2$y <- round(s2$y,2)
# s3 <- as.data.frame(cbind(as.Date(plot_dates.FF),termpremia_France.FF))
# names(s3) <- c("x","y")
# s3$x <- as.Date(s3$x)
# s3$y <- round(s3$y,2)
# s4 <- as.data.frame(cbind(as.Date(plot_dates.FF),rep(0,length(plot_dates.FF))))
# names(s4) <- c("x","y")
# s4$x <- as.Date(s4$x)
# 
# France_Term_Premia_FF <- highchart() %>%
#   hc_add_series(s1, "line", hcaes(x, y), name = "10Y Yield", color = "black") %>%
#   hc_add_series(s2, "line", hcaes(x, y), name = "Risk Neutral 10Y Yield", color = "red") %>%
#   hc_add_series(s3, "line", hcaes(x, y), name = "10Y Term Premia", color = "blue") %>%
#   hc_add_series(s4, "line", hcaes(x, y), name = "", dashStyle = "dot", color = "black",
#                 showInLegend = FALSE) %>%
#   hc_title(text = "10Y Yield Decomposition using FF Model - France") %>%
#   hc_xAxis(type = "datetime", title = list(text = "Date"), gridLineWidth = 1) %>%
#   hc_yAxis(title = list(text = "Yield & Term Premia"), min = -1, max = 7) %>%
#   hc_navigator(enabled = TRUE) %>%
#   hc_scrollbar(enabled = TRUE) %>%
#   hc_legend(enabled = TRUE) %>%
#   hc_boost(enabled = TRUE) %>%
#   hc_exporting(enabled = TRUE)
# 
# 
# #Spain FF TP
# s1 <- as.data.frame(cbind(plot_dates.FF,as.numeric(big4_y.10Y.ts[plot_dates.FF]$ES_n10)))
# names(s1) <- c("x","y")
# s1$x <- as.Date(s1$x)
# s1$y <- round(s1$y,2)
# s2 <- as.data.frame(cbind(as.Date(plot_dates.FF),as.numeric(RiskFreeYields.FF[,40]*100)))
# names(s2) <- c("x","y")
# s2$x <- as.Date(s2$x)
# s2$y <- round(s2$y,2)
# s3 <- as.data.frame(cbind(as.Date(plot_dates.FF),termpremia_Spain.FF))
# names(s3) <- c("x","y")
# s3$x <- as.Date(s3$x)
# s3$y <- round(s3$y,2)
# s4 <- as.data.frame(cbind(as.Date(plot_dates.FF),rep(0,length(plot_dates.FF))))
# names(s4) <- c("x","y")
# s4$x <- as.Date(s4$x)
# 
# Spain_Term_Premia_FF <- highchart() %>%
#   hc_add_series(s1, "line", hcaes(x, y), name = "10Y Yield", color = "black") %>%
#   hc_add_series(s2, "line", hcaes(x, y), name = "Risk Neutral 10Y Yield", color = "red") %>%
#   hc_add_series(s3, "line", hcaes(x, y), name = "10Y Term Premia", color = "blue") %>%
#   hc_add_series(s4, "line", hcaes(x, y), name = "", dashStyle = "dot", color = "black",
#                 showInLegend = FALSE) %>%
#   hc_title(text = "10Y Yield Decomposition using FF Model - Spain") %>%
#   hc_xAxis(type = "datetime", title = list(text = "Date"), gridLineWidth = 1) %>%
#   hc_yAxis(title = list(text = "Yield & Term Premia"), min = -1, max = 7) %>%
#   hc_navigator(enabled = TRUE) %>%
#   hc_scrollbar(enabled = TRUE) %>%
#   hc_legend(enabled = TRUE) %>%
#   hc_boost(enabled = TRUE) %>%
#   hc_exporting(enabled = TRUE)
# 
# #Italy FF TP
# s1 <- as.data.frame(cbind(plot_dates.FF,as.numeric(big4_y.10Y.ts[plot_dates.FF]$IT_n10)))
# names(s1) <- c("x","y")
# s1$x <- as.Date(s1$x)
# s1$y <- round(s1$y,2)
# s2 <- as.data.frame(cbind(as.Date(plot_dates.FF),as.numeric(RiskFreeYields.FF[,40]*100)))
# names(s2) <- c("x","y")
# s2$x <- as.Date(s2$x)
# s2$y <- round(s2$y,2)
# s3 <- as.data.frame(cbind(as.Date(plot_dates.FF),termpremia_Italy.FF))
# names(s3) <- c("x","y")
# s3$x <- as.Date(s3$x)
# s3$y <- round(s3$y,2)
# s4 <- as.data.frame(cbind(as.Date(plot_dates.FF),rep(0,length(plot_dates.FF))))
# names(s4) <- c("x","y")
# s4$x <- as.Date(s4$x)
# 
# Italy_Term_Premia_FF <- highchart() %>%
#   hc_add_series(s1, "line", hcaes(x, y), name = "10Y Yield", color = "black") %>%
#   hc_add_series(s2, "line", hcaes(x, y), name = "Risk Neutral 10Y Yield", color = "red") %>%
#   hc_add_series(s3, "line", hcaes(x, y), name = "10Y Term Premia", color = "blue") %>%
#   hc_add_series(s4, "line", hcaes(x, y), name = "", dashStyle = "dot", color = "black",
#                 showInLegend = FALSE) %>%
#   hc_title(text = "10Y Yield Decomposition using FF Model - Italy") %>%
#   hc_xAxis(type = "datetime", title = list(text = "Date"), gridLineWidth = 1) %>%
#   hc_yAxis(title = list(text = "Yield & Term Premia"), min = -1, max = 7) %>%
#   hc_navigator(enabled = TRUE) %>%
#   hc_scrollbar(enabled = TRUE) %>%
#   hc_legend(enabled = TRUE) %>%
#   hc_boost(enabled = TRUE) %>%
#   hc_exporting(enabled = TRUE)

#MP COMPARISON
s1 <- as.data.frame(cbind(plot_dates.ACM,RiskFreeYields.ACM[,40]*100))
names(s1) <- c("x","y")
s1$x <- as.Date(s1$x)
s1$y <- round(s1$y,2)
s1 <- s1 %>% filter(x >= as.Date("2010-01-01"))
s2 <- master_df %>% select(Date,Rate,sum_Er_cf) %>% mutate(MPC = Rate + sum_Er_cf) %>% select(Date,MPC)
names(s2) <- c("x","y")
s2$x <- as.Date(s2$x)
s2$y <- round(s2$y,2)
# s3 <- as.data.frame(cbind(plot_dates.FF,as.numeric(RiskFreeYields.FF[,40]*100)))
# names(s3) <- c("x","y")
# s3$x <- as.Date(s3$x)
# s3$y <- round(s3$y,2)
s3 <- s3 %>% filter(x >= as.Date("2010-01-01"))
s4 <- as.data.frame(cbind(s1$x,rep(0,length(s1$x))))
names(s4) <- c("x","y")
s4$x <- as.Date(s4$x)
ACM_CF_MP <- highchart() %>%
  hc_add_series(s1, "line", hcaes(x, y), name = "ACM MP", color = "brown",
                marker = list(symbol = "circle", lineWidth = 0, radius = 2)) %>%
  #hc_add_series(s3, "line", hcaes(x, y), name = "FF MP", color = "blue") %>%
  hc_add_series(s2, "line", hcaes(x, y), name = "Consensus MP", color = "red",
                marker = list(symbol = "circle", lineWidth = 0, radius = 2)) %>%
  hc_add_series(s4, "line", hcaes(x, y), name = "", dashStyle = "dot", color = "black",
                marker = list(enabled = FALSE), showInLegend = FALSE) %>%
  hc_title(text = "ACM vs Consensus 10Y Monetary Policy Component") %>%
  hc_xAxis(type = "datetime", title = list(text = "Date"), gridLineWidth = 1) %>%
  hc_yAxis(title = list(text = "Yield & Term Premia"), min = -2, max = 4) %>%
  hc_navigator(enabled = TRUE) %>%
  hc_scrollbar(enabled = TRUE) %>%
  hc_legend(enabled = TRUE) %>%
  hc_boost(enabled = TRUE) %>%
  hc_exporting(enabled = TRUE)

#GERMANY TP COMPARISON
s1 <- as.data.frame(cbind(plot_dates.ACM,termpremia_Germany.ACM))
names(s1) <- c("x","y")
s1$x <- as.Date(s1$x)
s1$y <- round(s1$y,2)
s1 <- s1 %>% filter(x >= as.Date("2010-01-01"))
s2 <- select(master_df,Date,TP_cf_Germany)
names(s2) <- c("x","y")
s2$y <- round(s2$y,2)
s2$x <- as.Date(s2$x)
# s3 <- as.data.frame(cbind(plot_dates.FF,termpremia_Germany.FF))
# names(s3) <- c("x","y")
# s3$x <- as.Date(s3$x)
# s3$y <- round(s3$y,2)
# s3 <- s3 %>% filter(x >= as.Date("2010-01-01"))
s4 <- as.data.frame(cbind(s1$x,rep(0,length(s1$x))))
names(s4) <- c("x","y")
s4$x <- as.Date(s4$x)
Germany_ACM_CF_TP <- highchart() %>%
  hc_add_series(s1, "line", hcaes(x, y), name = "ACM TP", color = "brown",
                marker = list(symbol = "circle", lineWidth = 0, radius = 2)) %>%
  #hc_add_series(s3, "line", hcaes(x, y), name = "FF TP", color = "blue") %>%
  hc_add_series(s2, "line", hcaes(x, y), name = "Consensus TP", color = "red",
                marker = list(symbol = "circle", lineWidth = 1, radius = 2)) %>%
  hc_add_series(s4, "line", hcaes(x, y), name = "", dashStyle = "dot", color = "black",
                marker = list(enabled = FALSE), 
                showInLegend = FALSE) %>%
  hc_title(text = "ACM vs FF vs Consensus 10Y Term Premia - Germany") %>%
  hc_xAxis(type = "datetime", title = list(text = "Date"), gridLineWidth = 1) %>%
  hc_yAxis(title = list(text = "Yield & Term Premia"), min = -2, max = 6) %>%
  hc_navigator(enabled = TRUE) %>%
  hc_scrollbar(enabled = TRUE) %>%
  hc_legend(enabled = TRUE) %>%
  hc_boost(enabled = TRUE) %>%
  hc_exporting(enabled = TRUE)


#FRANCE TP COMPARISON
s1 <- as.data.frame(cbind(plot_dates.ACM,termpremia_France.ACM))
names(s1) <- c("x","y")
s1$x <- as.Date(s1$x)
s1$y <- round(s1$y,2)
s1 <- s1 %>% filter(x >= as.Date("2010-01-01"))
s2 <- select(master_df,Date,TP_cf_France)
names(s2) <- c("x","y")
s2$y <- round(s2$y,2)
s2$x <- as.Date(s2$x)
# s3 <- as.data.frame(cbind(plot_dates.FF,termpremia_France.FF))
# names(s3) <- c("x","y")
# s3$x <- as.Date(s3$x)
# s3$y <- round(s3$y,2)
# s3 <- s3 %>% filter(x >= as.Date("2010-01-01"))
s4 <- as.data.frame(cbind(s1$x,rep(0,length(s1$x))))
names(s4) <- c("x","y")
s4$x <- as.Date(s4$x)
France_ACM_CF_TP <- highchart() %>%
  hc_add_series(s1, "line", hcaes(x, y), name = "ACM TP", color = "brown",
                marker = list(symbol = "circle", lineWidth = 0, radius = 2)) %>%
  #hc_add_series(s3, "line", hcaes(x, y), name = "FF TP", color = "blue") %>%
  hc_add_series(s2, "line", hcaes(x, y), name = "Consensus TP", color = "red",
                marker = list(symbol = "circle", lineWidth = 1, radius = 2)) %>%
  hc_add_series(s4, "line", hcaes(x, y), name = "", dashStyle = "dot", color = "black",
                marker = list(enabled = FALSE),
                showInLegend = FALSE) %>%
  hc_title(text = "ACM vs FF vs Consensus 10Y Term Premia - France") %>%
  hc_xAxis(type = "datetime", title = list(text = "Date"), gridLineWidth = 1) %>%
  hc_yAxis(title = list(text = "Yield & Term Premia"), min = -2, max = 6) %>%
  hc_navigator(enabled = TRUE) %>%
  hc_scrollbar(enabled = TRUE) %>%
  hc_legend(enabled = TRUE) %>%
  hc_boost(enabled = TRUE) %>%
  hc_exporting(enabled = TRUE)

#Spain TP COMPARISON
s1 <- as.data.frame(cbind(plot_dates.ACM,termpremia_Spain.ACM))
names(s1) <- c("x","y")
s1$x <- as.Date(s1$x)
s1$y <- round(s1$y,2)
s1 <- s1 %>% filter(x >= as.Date("2010-01-01"))
s2 <- select(master_df,Date,TP_cf_Spain)
names(s2) <- c("x","y")
s2$y <- round(s2$y,2)
s2$x <- as.Date(s2$x)
# s3 <- as.data.frame(cbind(plot_dates.FF,termpremia_Spain.FF))
# names(s3) <- c("x","y")
# s3$x <- as.Date(s3$x)
# s3$y <- round(s3$y,2)
# s3 <- s3 %>% filter(x >= as.Date("2010-01-01"))
s4 <- as.data.frame(cbind(s1$x,rep(0,length(s1$x))))
names(s4) <- c("x","y")
s4$x <- as.Date(s4$x)
Spain_ACM_CF_TP <- highchart() %>%
  hc_add_series(s1, "line", hcaes(x, y), name = "ACM TP", color = "brown",
                marker = list(symbol = "circle", lineWidth = 0, radius = 2)) %>%
  #hc_add_series(s3, "line", hcaes(x, y), name = "FF TP", color = "blue") %>%
  hc_add_series(s2, "line", hcaes(x, y), name = "Consensus TP", color = "red",
                marker = list(symbol = "circle", lineWidth = 1, radius = 2)) %>%
  hc_add_series(s4, "line", hcaes(x, y), name = "", dashStyle = "dot", color = "black",
                marker = list(enabled = FALSE),
                showInLegend = FALSE) %>%
  hc_title(text = "ACM vs FF vs Consensus 10Y Term Premia - Spain") %>%
  hc_xAxis(type = "datetime", title = list(text = "Date"), gridLineWidth = 1) %>%
  hc_yAxis(title = list(text = "Yield & Term Premia"), min = -1, max = 7) %>%
  hc_navigator(enabled = TRUE) %>%
  hc_scrollbar(enabled = TRUE) %>%
  hc_legend(enabled = TRUE) %>%
  hc_boost(enabled = TRUE) %>%
  hc_exporting(enabled = TRUE)


#ITALY TP COMPARISON
s1 <- as.data.frame(cbind(plot_dates.ACM,termpremia_Italy.ACM))
names(s1) <- c("x","y")
s1$x <- as.Date(s1$x)
s1$y <- round(s1$y,2)
s1 <- s1 %>% filter(x >= as.Date("2010-01-01"))
s2 <- select(master_df,Date,TP_cf_Italy)
names(s2) <- c("x","y")
s2$y <- round(s2$y,2)
s2$x <- as.Date(s2$x)
# s3 <- as.data.frame(cbind(plot_dates.FF,termpremia_Italy.FF))
# names(s3) <- c("x","y")
# s3$x <- as.Date(s3$x)
# s3$y <- round(s3$y,2)
# s3 <- s3 %>% filter(x >= as.Date("2010-01-01"))
s4 <- as.data.frame(cbind(s1$x,rep(0,length(s1$x))))
names(s4) <- c("x","y")
s4$x <- as.Date(s4$x)
Italy_ACM_CF_TP <- highchart() %>%
  hc_add_series(s1, "line", hcaes(x, y), name = "ACM TP", color = "brown",
                marker = list(symbol = "circle", lineWidth = 0, radius = 2)) %>%
  #hc_add_series(s3, "line", hcaes(x, y), name = "FF TP", color = "blue") %>%
  hc_add_series(s2, "line", hcaes(x, y), name = "Consensus TP", color = "red",
                marker = list(symbol = "circle", lineWidth = 1, radius = 2)) %>%
  hc_add_series(s4, "line", hcaes(x, y), name = "", dashStyle = "dot", color = "black",
                marker = list(enabled = FALSE),
                showInLegend = FALSE) %>%
  hc_title(text = "ACM vs FF vs Consensus 10Y Term Premia - Italy") %>%
  hc_xAxis(type = "datetime", title = list(text = "Date"), gridLineWidth = 1) %>%
  hc_yAxis(title = list(text = "Yield & Term Premia"), min = -1, max = 7) %>%
  hc_navigator(enabled = TRUE) %>%
  hc_scrollbar(enabled = TRUE) %>%
  hc_legend(enabled = TRUE) %>%
  hc_boost(enabled = TRUE) %>%
  hc_exporting(enabled = TRUE)

#T = length(plot_dates)
#Projections <- highchart() %>%
#  hc_xAxis(type = "datetime", dateTimeLabelFormats = list(month = "%b %Y")) %>%
#  hc_yAxis(title = list(text = "Interest Rate"), min = -1, max = 5) %>%
#  hc_add_series(data = recent_data, hcaes(x = Date, y = Rate),
#                type = "line", name = "Observed 3M rates", color = "black", lineWidth = 2) %>%
#  hc_add_series(data = data.frame(x = fut_date, y = fut_rate_mod), hcaes(x = x, y = y),
#                type = "line", name = "Naive Model estimate", color = "blue", lineWidth = 2, dashStyle = "Dash") %>%
#  hc_add_series(data = data.frame(x = fut_date, y = fut_rate_consensus), hcaes(x = x, y = y),
#                type = "line", name = "Consensus forecast", color = "red", lineWidth = 2, dashStyle = "Dash") %>%
#  hc_add_series(data = data.frame(x = plot_dates[(T-14):T], y = fittedYields[(T-14):T,1]*100), hcaes(x = x, y = y),
#                type = "line", name = "Historical_ACM (1M)", color = "black", lineWidth = 2, dashStyle = "Dash") %>%
#  hc_add_series(data = data.frame(x = plot_dates.FF[(T-5):T], y = fittedYields.FF[(T-5):T,1]*100), hcaes(x = x, y = y),
#                type = "line", name = "Historical_FF (3M)", color = "black", lineWidth = 2, dashStyle = "Dash") %>%
#  hc_add_series(data = data.frame(x = plot_dates_proj, y = append(ESTR[T,seq(3, by = 3, length.out = 12*Y)],fittedYields[T,1],after=0)*100), hcaes(x = x, y = y),
#                type = "line", name = "ACM_Projection (1M)", color = "brown", lineWidth = 2, dashStyle = "Dash") %>%
#  hc_add_series(s5, "line", hcaes(x, y), name = "FF forecast", dashStyle = "dash", color = "blue") %>%
#  hc_legend(
#    layout = "horizontal",
#    align = "center",
#    verticalAlign = "bottom",
#    itemWidth = 200,
#    itemStyle = list(textOverflow = "ellipsis")
#  ) %>%
#  hc_boost(enabled = TRUE) %>%
#  hc_exporting(enabled = TRUE)

# temp <- na.omit(pred.ts.FF$n1_OoS)
# temp2 <- na.omit(pred.ts.FF$n1_IN)
# x <- c(tail(index(temp2),1),head(index(temp),12))
# y <- c(as.numeric(tail(temp2$n1_IN,1)),as.numeric(head(temp$n1_OoS,12)))*100
# s1 <- as.data.frame(cbind(x,y))
# s1$x <- as.Date(s1$x)
# x <- tail(index(temp2),5)
# y <- as.numeric(tail(temp2$n1_IN,5))*100
# s2 <- as.data.frame(cbind(x,y))
# s2$x <- as.Date(s2$x)
temp <- na.omit(pred.ts.ACM$n1_OoS)
temp2 <- na.omit(pred.ts.ACM$n1_IN)
x <- c(tail(index(temp2),1),head(index(temp),12))
y <- c(as.numeric(tail(temp2$n1_IN,1)),as.numeric(head(temp$n1_OoS,12)))*100
s3 <- as.data.frame(cbind(x,y))
s3$x <- as.Date(s3$x)
x <- tail(index(temp2),5)
y <- as.numeric(tail(temp2$n1_IN,5))*100
s4 <- as.data.frame(cbind(x,y))
s4$x <- as.Date(s4$x)
T = length(plot_dates.ACM)
#T.FF = length(plot_dates.FF)
Projections <- highchart() %>%
  hc_xAxis(type = "datetime", dateTimeLabelFormats = list(month = "%b %Y"), gridLineWidth = 1) %>%
  hc_yAxis(title = list(text = "Interest Rate"), min = -1, max = 5) %>%
  hc_add_series(data = recent_data, hcaes(x = Date, y = Rate),
                type = "line", name = "Observed 3M rates", color = "black", lineWidth = 2) %>%
  #hc_add_series(data = data.frame(x = fut_date, y = fut_rate_mod), hcaes(x = x, y = y),
  #              type = "line", name = "Naive Model estimate", color = "orange", lineWidth = 2, dashStyle = "Dash") %>%
  hc_add_series(data = data.frame(x = fut_date, y = fut_rate_consensus), hcaes(x = x, y = y),
                type = "line", name = "Consensus forecast", color = "red", lineWidth = 2, dashStyle = "Dash") %>%
  #hc_add_series(data = data.frame(x = plot_dates.ACM[(T-4):T], y = fittedYields.ACM[(T-4):T,1]*100), hcaes(x = x, y = y),
  #              type = "line", name = "ACM Model Fitted", color = "brown", lineWidth = 2) %>%
  hc_add_series(data = s4, hcaes(x = x, y = y), type = "line", name = "ACM Model Fitted", color = "brown", lineWidth = 2) %>%
  #hc_add_series(data = s2, hcaes(x = x, y = y), type = "line", name = "FF Model Fitted", color = "blue", lineWidth = 2) %>%
  #hc_add_series(data = data.frame(x = plot_dates_proj_3, y = append(ESTR.ACM[T,1:12],fittedYields.ACM[T,1], after = 0)*100), hcaes(x = x, y = y),
  #              type = "line", name = "ACM Forecast", color = "brown", lineWidth = 2, dashStyle = "Dash") %>%
  hc_add_series(data = s3, hcaes(x = x, y = y),type = "line", name = "ACM Forecast", color = "brown", lineWidth = 2, dashStyle = "Dash") %>%
  #hc_add_series(data = s1, hcaes(x = x, y = y),type = "line", name = "FF Forecast", color = "blue", lineWidth = 2, dashStyle = "Dash") %>%
  hc_legend(
    layout = "horizontal",
    align = "center",
    verticalAlign = "bottom",
    itemWidth = 200,
    itemStyle = list(textOverflow = "ellipsis")
  ) %>%
  hc_boost(enabled = TRUE) %>%
  hc_exporting(enabled = TRUE)
