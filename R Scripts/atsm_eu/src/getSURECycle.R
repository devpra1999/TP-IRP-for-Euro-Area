getSURECycle <- function(cross_cycle, cross_data){

  cycle_system <- systemfit(cross_cycle, "SUR", data = cross_data)
  
  cross_cycle <-summary(cycle_system)$coefficients
  US_cycle    <- t(data.frame(t(cross_cycle)) %>%
                     dplyr::select(starts_with("eq1_US")))
  rownames(US_cycle) <- gsub("^eq\\d+_US_", "", rownames(US_cycle))
  saveResults(US_cycle, "US_cycle", cleaned=1)
  
  cross_cycle <-summary(cycle_system)$coefficients
  EA_cycle    <- t(data.frame(t(cross_cycle)) %>%
                     dplyr::select(starts_with("eq2")))
  rownames(EA_cycle) <- gsub("^eq\\d+_EA_", "", rownames(EA_cycle))
  rownames(EA_cycle) <- gsub("^eq\\d+_", "", rownames(EA_cycle))
  saveResults(EA_cycle, "EA_cycle", cleaned=1)
  
  cross_cycle_coeff           <- data.frame(coef(cycle_system))
  colnames(cross_cycle_coeff) <- "coeff"
  cross_cycle_coeff           <- data.frame(t(cross_cycle_coeff))
  
  # Create US_cycle_coeff by dplyr::selecting columns with "eq1_US" prefix
  US_cycle_coeff <- cross_cycle_coeff %>%
    dplyr::select(starts_with("eq1"))
  
  # Create EA_cycle_coeff by dplyr::selecting columns with "eq2_EA" prefix
  EA_cycle_coeff <- cross_cycle_coeff %>%
    dplyr::select(starts_with("eq2"))
  # Remove prefix from column names in US_cycle_coeff
  colnames(US_cycle_coeff) <- gsub("^eq\\d+_US_", "", colnames(US_cycle_coeff))
  
  # Remove prefix from column names in EA_cycle_coeff
  colnames(EA_cycle_coeff) <- gsub("^eq\\d+_EA_", "", colnames(EA_cycle_coeff))
  colnames(EA_cycle_coeff) <- gsub("^eq\\d+_", "", colnames(EA_cycle_coeff))
  
  v <- resid(cycle_system)
  colnames(v) <- c("US", "EA")
  
  cross_data$US_v <- v$US
  cross_data$EA_v <- v$EA
  
  output <- list(
    cross_data = cross_data,
    US_cycle_coeff      = US_cycle_coeff,
    EA_cycle_coeff      = EA_cycle_coeff,
    v                   = v,
    cycle_system  = cycle_system
  ) 
  return(output)

}