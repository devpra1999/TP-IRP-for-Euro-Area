

prepareMainRDA <- function(){
  
  scaleData <- function(data) {
    # List of column names to be checked and potentially scaled
    columns_to_scale <- c("dy_pot", "y_gap", "pi_gap", "pi", "PTR")
    
    # Iterate through each column
    for (col in columns_to_scale) {
      # Check if the column exists in the data
      if (col %in% names(data)) {
        # Check if max of column > 1 and scale if necessary
        if (max(data[,col], na.rm = TRUE) > 1) {
          data[,col] <- data[,col] / 100
        }
      }
    }
    
    return(data)
  }
  
  if (!file.exists('US/data/main.rda')) {
    message("Data not found. Running data download and transformation scripts...")
    
    # Ensure that you're in the correct working directory or adjust the paths accordingly.
    source('US/0_download_rf.R')
    source('US/1_transform_rf.R')
    
    # Now, your 'main.rda' should be created and you might want to load it after creating.
    if(file.exists('US/data/main.rda')){
      load('US/data/main.rda')
      data.ts <- scaleData(data.ts)
      save(data.ts, file='US/data/main.rda')
  } else {
      stop("The file 'US/data/main.rda' is not created. Check the scripts '0_download.R' and '1_transform.R'.")
    }
    
  } else {
    load('US/data/main.rda',envir = globalenv())
    data.ts <- scaleData(data.ts)
    save(data.ts, file='US/data/main.rda')
  }
  
}