nss_yields <- function(filename, n_maturities) {
  data <- read_csv(filename)
  data <- data[complete.cases(data), ] # Remove rows with missing values
  data <- data[order(data$Date), ]
  rownames(data) <- NULL
  
  # Convert to EOM observations
  data <- data %>% mutate(Date= ymd(Date), month = month(Date), year= year(Date)) %>%
                  group_by(month,year) %>%
                  arrange(Date) %>%
                  filter(row_number()==1) 
  
  # Nelson, Svensson, Siegel yield curve parameterization
  nss_yield <- function(n) {
    Beta0 <- data$BETA0
    Beta1 <- data$BETA1
    Beta2 <- data$BETA2
    Beta3 <- data$BETA3
    Tau1 <- data$TAU1
    Tau2 <- data$TAU2
    
    return(Beta0 + Beta1 * (1 - exp(-n / Tau1)) / (n / Tau1) +
             Beta2 * ((1 - exp(-n / Tau1)) / (n / Tau1) - exp(-n / Tau1)) +
             Beta3 * ((1 - exp(-n / Tau2)) / (n / Tau2) - exp(-n / Tau2)))
  }
  
  # Compute yields
  rawYields <- matrix(nrow = nrow(data), ncol = n_maturities)
  for (mat in 1:n_maturities) {
    rawYields[, mat] <- nss_yield((mat + 1) / 12)
  }
  
  plot_dates <- as.Date(data$Date)
  
  return(list(rawYields = rawYields, plot_dates = plot_dates))
}
