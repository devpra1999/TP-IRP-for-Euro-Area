

prepareMainRDA <- function(update){
  
  scaleData <- function(data) {
    # List of column names to be checked and potentially scaled
    columns_to_scale <- c("dy", "dy_pot", "y_gap", "pi_gap", "pi", "PTR")
    
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
  
  if (!file.exists('data/main.rda') || (update == 1)) {
    
    print("hey")
    
    if (!file.exists('data/main.rda')) {message("Data not found. Running data download and transformation scripts...")}
    
    # Ensure that you're in the correct working directory or adjust the paths accordingly.
    source('src/getRawData.R')
    source('src/getMainData.R')
    
    # UPDATE 1: Wednesday 22nd November, 2023.
    # Adding 2023Q3 PTR and pi_gap
    newdata.ts <- data.ts
    newdata.ts["2023-09-01", "PTR"] = 2.22
    newdata.ts["2023-09-01", "pi_gap"] = newdata.ts["2023-09-01", "pi"] - newdata.ts["2023-09-01", "PTR"]
    data.ts <- newdata.ts
    save(data.ts, file="data/updated_main.rda")
    print( data.ts["2023-09-01", "pi_gap"])

    
   # Now, your 'main.rda' should be created and you might want to load it after creating.
   if(file.exists('data/updated_main.rda')){
     load('data/updated_main.rda')
     data.ts <- scaleData(data.ts)
     save(data.ts, file='data/updated_main.rda')
 } else {
     stop("The file 'data/main.rda' is not created. Check the scripts '0_download.R' and '1_transform.R'.")
   }
    
  } else {
    load('data/main.rda',envir = globalenv())
    data.ts <- scaleData(data.ts)
    save(data.ts, file='data/main.rda')
  }
  
}