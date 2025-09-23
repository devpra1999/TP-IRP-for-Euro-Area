
readData            <- function(filename){
  filename <- as.character(filename)
  
  # Check and append .csv if necessary
  if (!grepl("\\.csv$", filename)) {
    filename <- paste0(filename, ".csv")
  }
  
  # Construct the file path
  filepath <- paste0("atsm_eu/data/", filename)
  return(read.csv(filepath))
}
      
convertToTS         <- function(data.df, country=NULL){
  
  # Convert the 'date' column to Date class with the correct format
  data.df$date <- as.Date(data.df$date, format="%Y-%m-%d")

  
  data.ts <- xts(data.df[, !colnames(data.df) %in% c('date')], 
                      order.by = (data.df$date))
  colnames(data.ts) <- colnames(data.df[!colnames(data.df) %in% c('date')])
  if (!is.null(country)) { 
    colnames(data.ts) <- paste0(country,"_", colnames(data.ts))
  }
  return(data.ts)

}
      
getPureNames        <- function(data.ts, country){
  colnames(data.ts) <- gsub(paste0(country,"_"), "", names(data.ts))
  return(data.ts)
}

addCountryToHeader  <- function(data.ts, country){
  colnames(data.ts) <- paste0(country, "_", names(data.ts))
  return(data.ts)
}