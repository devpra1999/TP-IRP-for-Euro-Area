# Load required libraries
library(readxl)

# Load Gurkaynak, Sack, Wright dataset.
# This data is extracted from here: https://www.federalreserve.gov/pubs/feds/2006/200628/200628abs.html
nss_yields <- function(filename, n_maturities) {
  data <- read_excel(filename, col_types = c("date", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric"))
  data <- data[complete.cases(data), ] # Remove rows with missing values
  data <- data[order(data$Date), ]
  rownames(data) <- NULL
  
  # Convert to EOM observations
  data <- data %>% mutate(Date= ymd(Date), # convert statistic_date to date format
                month = month(Date),  #create month and year columns
                year= year(Date)) %>%
    group_by(month,year) %>% # group by month and year
    arrange(Date) %>% # make sure the df is sorted by date
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
  
  plot_dates <- as.POSIXct(data$Date)
  
  return(list(rawYields = rawYields, plot_dates = plot_dates))
}
