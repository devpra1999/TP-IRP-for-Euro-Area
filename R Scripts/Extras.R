Projection_ACM <- highchart() %>%
  hc_xAxis(type = "datetime", dateTimeLabelFormats = list(month = "%b %Y")) %>%
  hc_yAxis(title = list(text = "Interest Rate"), min = -1, max = 5) %>%
  hc_add_series(data = filter(master_df,Date >= (lr$Date - months(12))), hcaes(x = Date, y = Rate),
                type = "line", name = "Historical (3M)", color = "black", lineWidth = 2) %>%
  hc_add_series(data = data.frame(x = fut_date, y = fut_rate_mod), hcaes(x = x, y = y),
                type = "line", name = "Model estimate", color = "blue", lineWidth = 2, dashStyle = "Dash") %>%
  hc_add_series(data = data.frame(x = fut_date, y = fut_rate_consensus), hcaes(x = x, y = y),
                type = "line", name = "Consensus forecast", color = "red", lineWidth = 2, dashStyle = "Dash") %>%
  hc_add_series(data = data.frame(x = plot_dates[(T-5):T], y = fittedYields[(T-5):T,1]*100), hcaes(x = x, y = y),
                type = "line", name = "Historical_ACM_Fitted", color = "black", lineWidth = 2, dashStyle = "Dash") %>%
  hc_add_series(data = data.frame(x = plot_dates_proj, y = ESTR[T,1:36]*100), hcaes(x = x, y = y),
                type = "line", name = "ACM_Projection", color = "brown", lineWidth = 2, dashStyle = "Dash") %>%
  hc_legend(
    layout = "horizontal",
    align = "center",
    verticalAlign = "bottom",
    itemWidth = 200,
    itemStyle = list(textOverflow = "ellipsis")
  ) %>%
  hc_boost(enabled = TRUE) %>%
  hc_exporting(enabled = TRUE)
