################################################################################
###
### AUTHOR:       Carlo Favero and Rubén Fernández-Fuertes
### DATE:         Fall 2023
### DESCRIPTION:  This script creates the main dataset that is used in our paper.
###               We call it "main.rda" because of obvious reasons.
### OUTPUT:       main.rda
###
################################################################################


## -----------------------------------------------------------------------------
#rm(list=ls())                                                                   # Clear the environment 

## -----------------------------------------------------------------------------
#setwd(dirname(rstudioapi::getActiveDocumentContext()$path))                     # Set the appropriate environment to work in every machine

## -----------------------------------------------------------------------------
#listofpackages <- c("tidyverse","ellipse","reshape2","xts","xlsx","readxl",     # Install and load the required packages
#                    "quantmod")   
#source("aux/preparePackages.R")
#preparePackages(listofpackages)


## -----------------------------------------------------------------------------
load('US/data/raw/rawdata.rda')

# Inflation: π (pi)
rawdata.ts$LP     <- log(rawdata.ts$PCECTPI)
rawdata.ts$pi     <- (rawdata.ts$LP - lag.xts(rawdata.ts$LP, 4))*100
rawdata.ts$pi_gap <- rawdata.ts$pi - rawdata.ts$PTR

# Output gap: y_gap 
rawdata.ts$y      <- log(rawdata.ts$GDPC1)
rawdata.ts$y_pot  <- log(rawdata.ts$GDPPOT)
rawdata.ts$y_gap  <- (rawdata.ts$y - rawdata.ts$y_pot)*100
rawdata.ts$dy_pot <- (rawdata.ts$y_pot - lag.xts(rawdata.ts$y_pot, 4))*100
#plot(rawdata.ts$dy_pot)
#summary(dynlm(y_gap ~ L(y_gap)))

# Interest rates
rawdata.ts$y3m_cc <-log(1+0.01*rawdata.ts$DGS3MO)
rawdata.ts$US_RFcc<-log(1+0.01*rawdata.ts$US_RF)
#rawdata.ts$y3m_ccq<-0.25*log(1+0.01*rawdata.ts$DGS3MO)*100

# Maturities in quarters
maturities        <- read.csv("US/data/maturities.csv")
rx_maturities     <-  c(4,8,12,16,20,28,40,60) 
# Looping through years for yield and HPR calculations
colnames_y        <- c("yNSS1")
colnames_hper     <- c()

N <-last(maturities[last(index(maturities)),"mat"])

for (q in 2:N){
  
  colname_y       <- paste0("y",q)
  colname_prev_y  <- paste0("y",q-1)  
  # Take the fit ones if the original do not exist
  if (!colname_y %in% colnames(rawdata.ts)){colname_y <- paste0("yNSS",q)}
  if (!colname_prev_y %in% colnames(rawdata.ts)){colname_prev_y <- paste0("yNSS",q-1)} 
  colname_hpr     <- paste0("hpr",  sprintf("%1d", q))
  colname_hper    <- paste0("hper", sprintf("%1d", q))
  
  # Calculating hpr values based on provided formula
  new_hpr    <- lag.xts(rawdata.ts[, colname_y]) - (q-1) * (rawdata.ts[, colname_prev_y] - lag.xts(rawdata.ts[, colname_y]))
  new_hpr    <- new_hpr/4
  rawdata.ts <- merge(rawdata.ts, setNames(new_hpr, colname_hpr))
  
  # Calculating hper values based on provided formula
  new_hper   <- rawdata.ts[, colname_hpr] - 1/4 * lag.xts(rawdata.ts$yNSS1)     
  
  rawdata.ts <- merge(rawdata.ts, setNames(new_hper, colname_hper))
  
  # Keep a record of the columns used
  colnames_y      <- c(colnames_y, colname_y)
  colnames_hper   <- c(colnames_hper, colname_hper)
}

# Other parameters that don't follow a numeric pattern
params1 <- c("pi", "pi_gap", "y_gap", "y_pot", "dy_pot", "MY", "PTR", "y3m_cc", "US_RFcc")
params1 <- c(params1, colnames_y)
params1 <- c(params1, colnames_hper)
params1 <- unique(params1)

data.ts <- rawdata.ts[, params1]

# Save the database for the econometric analysis
save(data.ts,file="US/data/main.rda")


if (1==0){
## -----------------------------------------------------------------------------
## PLOT:

# Create a dataframe from xts object 
maturities.cols <- grep("^y[0-9]+(_fit)?$", colnames(data.ts), value = TRUE)
maturities.all  <- colnames(data.ts)[maturities.cols]
maturities.list <- as.numeric(gsub("y", "", maturities.all))
N <- max(maturities.list)

# Select the sub-dataset containing ONLY the yields
y.ts  <- data.ts[, maturities.all]
y.ts <- merge(data.ts$y3m_cc, y.ts)

data_df <- data.frame(dates = index(y.ts), Data = coredata(y.ts))

col_names <- c("date", colnames(y.ts))
colnames(data_df) <- col_names

typeof(data_df)
# Subset based on a date range
subset_df <- data_df[data_df$date >= as.Date("1980-03-01") & data_df$date <= as.Date("2023-09-01"), ]


plot <- ggplot(subset_df, aes(x = date)) +
 geom_line(aes(y = y3m_cc, color = "y3m_cc"), size = 1) +
 geom_line(aes(y = y40, color = "y40"), size = 1) +
 labs(title = "Trends in Yields", x = "Time", y = "%") +
 scale_color_manual(values = c("y3m_cc" = "blue", "y40" = "red"), name = NULL) +
 theme_minimal() +
 theme(axis.line = element_line(color = "black")) +
 scale_x_date(date_breaks = "5 year", date_labels = "%Y") +
 theme(
   legend.position = c(0.9, 0.9),  # Specify custom coordinates (x, y)
   legend.justification = c(1, 1),  # Specify justification for the coordinates
   axis.text.x = element_text(size = 8)  # Adjust the font size here
 )
print(plot)

#ggsave(filename = "Figure_1.pdf", plot = plot, device = "pdf",width = 5.72,height=3.12)

# Gather the y variables into a single column, adding an "Intensity" column
long_data <- subset_df %>%
 gather(key = "Variable", value = "Value", starts_with("y"))

# Set the levels for the "Variable" factor to ensure the desired color mapping
long_data$Variable <- factor(long_data$Variable, levels = unique(long_data$Variable))

# Define a custom color palette from pale blue to dark blue for the y variables
color_palette <- colorRampPalette(c("lightblue", "darkblue"))(length(unique(long_data$Variable)))

plot <- ggplot(long_data, aes(x = date, y = Value, color = Variable)) +
 geom_line(size = 1) +
 labs(title = "Common Trends in Yields",
      x = "Time", y = "Value") +
 scale_color_manual(values = setNames(color_palette, levels(long_data$Variable))) +  # Manually set colors
 #geom_line(data = subset_df, aes(x = Dates, y = y3m_cc), color = "red", size = 1) +  # Plot x in red
 theme_minimal() +
 theme(axis.line = element_line(color = "black")) +
 scale_x_date(date_breaks = "5 year", date_labels = "%Y") +
 theme(legend.text = element_text(size = 6),  # Adjust the font size in the legend
       legend.key.size = unit(0.1, "cm"))  # Specify a smaller legend key size (1 cm)

print(plot)

ggsave(filename = "Figure_1.pdf", plot = plot, device = "pdf",width = 5.72,height=3.12)
rm(long_data)
#tsdata <- na.omit(tsdata) # omitting the rows with NA presence

# Gather the y variables into a single column, adding an "Intensity" column
long_data <- subset_df %>%
 gather(key = "Variable", value = "Value", starts_with("hper"))

# Set the levels for the "Variable" factor to ensure the desired color mapping
long_data$Variable <- factor(long_data$Variable, levels = unique(long_data$Variable))

# Define a custom color palette from pale blue to dark blue for the y variables
color_palette <- colorRampPalette(c("lightblue", "darkblue"))(length(unique(long_data$Variable)))

plot <- ggplot(long_data, aes(x = date, y = Value, color = Variable)) +
 geom_line(size = 1) +
 labs(title = "Common Cycles in Holding Period Excess Returns",
      x = "Time", y = "Value") +
 scale_color_manual(values = setNames(color_palette, levels(long_data$Variable))) +  # Manually set colors
 #geom_line(data = subset_df, aes(x = Dates, y = y3m_cc), color = "red", size = 1) +  # Plot x in red
 theme_minimal() +
 theme(axis.line = element_line(color = "black")) +
 scale_x_date(date_breaks = "5 year", date_labels = "%Y") +
 theme(legend.text = element_text(size = 6),  # Adjust the font size in the legend
       legend.key.size = unit(0.1, "cm"))  # Specify a smaller legend key size (1 cm)

print(plot)

ggsave(filename = "Figure_2.pdf", plot = plot, device = "pdf",width = 5.72,height=3.12)
rm(long_data)




## THIS CODE ONLY INCLUDES ONLY 

#for (i in index(maturities)[-1]) {
#  # i in quarter to obtain the column
#  q       <- maturities[i, "mat"]
#  prev_q  <- maturities[i,"prev_mat"]
#  # Generating column names
#  colname_y       <- paste0("y", sprintf("%1d", q))
#  y       <- rawdata.ts[, colname_y, drop = FALSE]
#  # Assigning yields using merge
#  if (!colname_y %in% colnames(rawdata.ts) & (q!=0)){
#    rawdata.ts <- merge(rawdata.ts, setNames(y, colname_y))
#  }
#  # Add also the  previous yield
#  colname_prev_y  <-  paste0("y", sprintf("%1d", prev_q))
#  # cat("q:", q, "prev_q:", prev_q,"colname_y", colname_y, "colname_prev_y", colname_prev_y, "\n")
#  prev_y     <- rawdata.ts[, colname_prev_y, drop = FALSE]
#  if (! colname_prev_y %in% colnames(rawdata.ts) & (prev_q!=0)){
#    rawdata.ts <- merge(rawdata.ts, setNames(prev_y, colname_prev_y))
#  }
#
#  if (q %in% rx_maturities) {
#    colname_hpr     <- paste0("hpr", sprintf("%1d", q))
#    colname_hper    <- paste0("hper", sprintf("%1d", q))
#
#    # Calculating hpr values based on provided formula
#    new_hpr <-  1/4 * lag.xts(rawdata.ts[, colname_y]) - (q/4 - 1/4) * (rawdata.ts[, colname_prev_y] - lag.xts(rawdata.ts[, colname_y]))
#    rawdata.ts <- merge(rawdata.ts, setNames(new_hpr, colname_hpr))
#    
#    # Calculating hper values based on provided formula
#    new_hper <- rawdata.ts[, colname_hpr] - 1/4 * rawdata.ts$US_RFcc
#    rawdata.ts <- merge(rawdata.ts, setNames(new_hper, colname_hper))
#    
#    #
#    colnames_hper <- c(colnames_hper, colname_hper)
#  }
#  colnames_y <- c(colnames_y, colname_prev_y)
#  colnames_y <- c(colnames_y, colname_y)
#}

load("US/data/cf_nss_yields.rda")




plot(nss_yields$n_4)
lines(data.ts$y4)
lines(data.ts$y12)
}