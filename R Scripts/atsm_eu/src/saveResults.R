saveResults <- function(summary_results, name, cleaned=0){

  FED_ECB_results <- "atsm_eu/data/output/"
  
  if (cleaned==0){
    results <- summary_results$coefficients
  }
  else{
    results <- summary_results
  }

  colnames(results) <- c("coeff", "se", "tstat", "pval")
  filename <- paste0(FED_ECB_results, paste0(paste0("results_",name), ".csv"))
  if (file.exists(filename)) {
    file.remove(filename)
  }
  write.table(
    results, 
    file = filename, 
    row.names = TRUE, 
    quote = FALSE,
    sep=",")
  

}