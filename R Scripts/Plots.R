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
  hc_xAxis(type = "datetime", dateTimeLabelFormats = list(month = "%b %Y")) %>%
  hc_yAxis(title = list(text = "Yield")) %>%
  hc_add_series(data = s1, hcaes(x, y), type = "line", color = "blue", name = "Germany") %>%
  hc_add_series(data = s2, hcaes(x, y), type = "line", color = "brown", name = "France") %>%
  hc_add_series(data = s3, hcaes(x, y), type = "line", color = "green", name = "Spain") %>%
  hc_add_series(data = s4, hcaes(x, y), type = "line", color = "red", name = "Italy") %>%
  hc_plotOptions(series = list(marker = list(enabled = FALSE))) %>%
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
  hc_add_series(s1, "line", hcaes(x, y), name = "Yield", color = "black") %>%
  hc_add_series(s2, "line", hcaes(x, y),
                name = "Consensus Term Premia", color = "blue",connectNulls = TRUE, dashStyle = "dash",
                marker = list(symbol = "circle", lineWidth = 0, radius = 2)) %>%
  hc_add_series(s3, "line", hcaes(x, y),
                name = "Monetary Policy Component", dashStyle = "dash", color = "red",
                marker = list(symbol = "circle", lineWidth = 0, radius = 2), connectNulls = TRUE) %>%
  hc_add_series(s4, "line", hcaes(x, y),
                name = "", dashStyle = "dot", color = "black",
                showInLegend = FALSE) %>%
  hc_title(text = "Germany") %>%
  hc_xAxis(type = "datetime", title = list(text = "Date")) %>%
  hc_yAxis(title = list(text = "Yield & Term Premia"), min = -2, max = 7) %>%
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
  hc_add_series(s1, "line", hcaes(x, y), name = "Yield", color = "black") %>%
  hc_add_series(s2, "line", hcaes(x, y),
                name = "Consensus Term Premia", color = "blue",connectNulls = TRUE, dashStyle = "dash",
                marker = list(symbol = "circle", lineWidth = 0, radius = 2)) %>%
  hc_add_series(s3, "line", hcaes(x, y),
                name = "Monetary Policy Component", dashStyle = "dash", color = "red",
                marker = list(symbol = "circle", lineWidth = 0, radius = 2), connectNulls = TRUE) %>%
  hc_add_series(s4, "line", hcaes(x, y),
                name = "", dashStyle = "dot", color = "black",
                showInLegend = FALSE) %>%
  hc_title(text = "France") %>%
  hc_xAxis(type = "datetime", title = list(text = "Date")) %>%
  hc_yAxis(title = list(text = "Yield & Term Premia"), min = -2, max = 7) %>%
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
  hc_add_series(s1, "line", hcaes(x, y), name = "Yield", color = "black") %>%
  hc_add_series(s2, "line", hcaes(x, y),
                name = "Consensus Term Premia", color = "blue",connectNulls = TRUE, dashStyle = "dash",
                marker = list(symbol = "circle", lineWidth = 0, radius = 2)) %>%
  hc_add_series(s3, "line", hcaes(x, y),
                name = "Monetary Policy Component", dashStyle = "dash", color = "red",
                marker = list(symbol = "circle", lineWidth = 0, radius = 2), connectNulls = TRUE) %>%
  hc_add_series(s4, "line", hcaes(x, y),
                name = "", dashStyle = "dot", color = "black",
                showInLegend = FALSE) %>%
  hc_title(text = "Italy") %>%
  hc_xAxis(type = "datetime", title = list(text = "Date")) %>%
  hc_yAxis(title = list(text = "Yield & Term Premia"), min = -2, max = 7) %>%
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
  hc_add_series(s1, "line", hcaes(x, y), name = "Yield", color = "black") %>%
  hc_add_series(s2, "line", hcaes(x, y),
                name = "Consensus Term Premia", color = "blue",connectNulls = TRUE, dashStyle = "dash",
                marker = list(symbol = "circle", lineWidth = 0, radius = 2)) %>%
  hc_add_series(s3, "line", hcaes(x, y),
                name = "Monetary Policy Component", dashStyle = "dash", color = "red",
                marker = list(symbol = "circle", lineWidth = 0, radius = 2), connectNulls = TRUE) %>%
  hc_add_series(s4, "line", hcaes(x, y),
                name = "", dashStyle = "dot", color = "black",
                showInLegend = FALSE) %>%
  hc_title(text = "Spain") %>%
  hc_xAxis(type = "datetime", title = list(text = "Date")) %>%
  hc_yAxis(title = list(text = "Yield & Term Premia"), min = -2, max = 7) %>%
  hc_legend(enabled = TRUE) %>%
  hc_boost(enabled = TRUE) %>%
  hc_exporting(enabled = TRUE)

