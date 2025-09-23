
getSURETrend <- function(cross_trend, cross_data, R){

    # RESTRICTED:
    restricted_trend_system     <-  systemfit(cross_trend, method="SUR", data = cross_data,
                                              restrict.matrix = R)
    print(summary(restricted_trend_system))
    
    cross_restricted_trend <-summary(restricted_trend_system)$coefficients
    US_restricted_trend    <- t(data.frame(t(cross_restricted_trend)) %>%
                                  dplyr::select(starts_with("eq1_US")))
    rownames(US_restricted_trend) <- gsub("^eq\\d+_US_", "", rownames(US_restricted_trend))
    saveResults(US_restricted_trend, "US_restricted_trend", cleaned=1)
    
    cross_restricted_trend <-summary(restricted_trend_system)$coefficients
    EA_restricted_trend    <- t(data.frame(t(cross_restricted_trend)) %>%
                                  dplyr::select(starts_with("eq2_EA")))
    rownames(EA_restricted_trend) <- gsub("^eq\\d+_EA_", "", rownames(EA_restricted_trend))
    saveResults(EA_restricted_trend, "EA_restricted_trend", cleaned=1)
    
    cross_trend_coeff           <- data.frame(coef(restricted_trend_system))
    colnames(cross_trend_coeff) <- "coeff"
    cross_trend_coeff           <- data.frame(t(cross_trend_coeff))
    
    # Create US_trend_coeff by dplyr::selecting columns with "eq1_US" prefix
    US_trend_coeff <- cross_trend_coeff %>%
      dplyr::select(starts_with("eq1_US"))
    
    # Create EA_trend_coeff by dplyr::selecting columns with "eq2_EA" prefix
    EA_trend_coeff <- cross_trend_coeff %>%
      dplyr::select(starts_with("eq2_EA"))
    # Remove prefix from column names in US_trend_coeff
    colnames(US_trend_coeff) <- gsub("^eq\\d+_US_", "", colnames(US_trend_coeff))
    
    # Remove prefix from column names in EA_trend_coeff
    colnames(EA_trend_coeff) <- gsub("^eq\\d+_EA_", "", colnames(EA_trend_coeff))
    
    u <- resid(restricted_trend_system)
    colnames(u) <- c("US", "EA")
    u$date <- cross_data$date
    
    filepath <- "atsm_eu/data/output/results_cross_trend_resid.csv"
    if (file.exists(filepath)) {
      file.remove(filepath)
    }
    write.csv(u, filepath, 2, row.names = FALSE)
    
    cross_data$US_u <- u$US
    cross_data$EA_u <- u$EA
    
    cross_data$US_lag_u <- lag.xts(cross_data$US_u)
    cross_data$EA_lag_u <- lag.xts(cross_data$EA_u)
    
    output <- list(
      cross_data = cross_data,
      US_trend_coeff = US_trend_coeff,
      EA_trend_coeff = EA_trend_coeff
    ) 
  return(output)
}