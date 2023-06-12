Yield_Plot <- highchart() %>%
  hc_title(text = "10-Y Government Bond Yields") %>%
  hc_xAxis(type = "datetime", dateTimeLabelFormats = list(month = "%b %Y")) %>%
  hc_yAxis(title = list(text = "Yield")) %>%
  hc_add_series(data = master_df, hcaes(x = Date, y = Yield_Germany), type = "line", color = "blue", name = "Germany") %>%
  hc_add_series(data = master_df, hcaes(x = Date, y = Yield_France), type = "line", color = "brown", name = "France") %>%
  hc_add_series(data = master_df, hcaes(x = Date, y = Yield_Spain), type = "line", color = "green", name = "Spain") %>%
  hc_add_series(data = master_df, hcaes(x = Date, y = Yield_Italy), type = "line", color = "red", name = "Italy") %>%
  hc_plotOptions(series = list(marker = list(enabled = FALSE))) %>%
  hc_legend(layout = "horizontal", align = "center", verticalAlign = "top") %>%
  hc_colors(c("blue", "brown", "green", "red")) %>%
  hc_exporting(enabled = TRUE)


Germany_Term_Premia <- highchart() %>%
  hc_add_series(master_df, "line", hcaes(x = Date, y = Spread_Germany), name = "Term Spread") %>%
  hc_add_series(master_df, "line", hcaes(x = Date, y = TP_Germany),
                name = "Term Premia", dashStyle = "dash", color = "red", lineWidth = 2) %>%
  hc_add_series(master_df, "scatter", hcaes(x = Date, y = TP_cf_Germany),
                name = "Consensus_TP", marker = list(symbol = "circle", lineWidth = 0, radius = 2)) %>%
  hc_title(text = " Germany") %>%
  hc_xAxis(type = "datetime", title = list(text = "Date")) %>%
  hc_yAxis(title = list(text = "Yield & Composition"), min = -2, max = 7) %>%
  hc_legend(enabled = TRUE) %>%
  hc_exporting(enabled = TRUE)

France_Term_Premia <- highchart() %>%
  hc_add_series(master_df, "line", hcaes(x = Date, y = Spread_France), name = "Term Spread") %>%
  hc_add_series(master_df, "line", hcaes(x = Date, y = TP_France),
                name = "Term Premia", dashStyle = "dash", color = "red", lineWidth = 2) %>%
  hc_add_series(master_df, "scatter", hcaes(x = Date, y = TP_cf_France),
                name = "Consensus_TP", marker = list(symbol = "circle", lineWidth = 0, radius = 2)) %>%
  hc_title(text = "France") %>%
  hc_xAxis(type = "datetime", title = list(text = "Date")) %>%
  hc_yAxis(title = list(text = "Yield & Composition"), min = -2, max = 7) %>%
  hc_legend(enabled = TRUE) %>%
  hc_exporting(enabled = TRUE)

Spain_Term_Premia <- highchart() %>%
  hc_add_series(master_df, "line", hcaes(x = Date, y = Spread_Spain), name = "Term Spread") %>%
  hc_add_series(master_df, "line", hcaes(x = Date, y = TP_Spain),
                name = "Term Premia", dashStyle = "dash", color = "red", lineWidth = 2) %>%
  hc_add_series(master_df, "scatter", hcaes(x = Date, y = TP_cf_Spain),
                name = "Consensus_TP", marker = list(symbol = "circle", lineWidth = 0, radius = 2)) %>%
  hc_title(text = "Spain") %>%
  hc_xAxis(type = "datetime", title = list(text = "Date")) %>%
  hc_yAxis(title = list(text = "Yield & Composition"), min = -2, max = 7) %>%
  hc_legend(enabled = TRUE) %>%
  hc_exporting(enabled = TRUE)
  
Italy_Term_Premia <- highchart() %>%
  hc_add_series(master_df, "line", hcaes(x = Date, y = Spread_Italy), name = "Term Spread") %>%
  hc_add_series(master_df, "line", hcaes(x = Date, y = TP_Italy),
                name = "Term Premia", dashStyle = "dash", color = "red", lineWidth = 2) %>%
  hc_add_series(master_df, "scatter", hcaes(x = Date, y = TP_cf_Italy),
                name = "Consensus_TP", marker = list(symbol = "circle", lineWidth = 0, radius = 2)) %>%
  hc_title(text = "Italy") %>%
  hc_xAxis(type = "datetime", title = list(text = "Date")) %>%
  hc_yAxis(title = list(text = "Yield & Composition"), min = -2, max = 7) %>%
  hc_legend(enabled = TRUE) %>%
  hc_exporting(enabled = TRUE)
