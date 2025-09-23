## -----------------------------------------------------------------------------
## TABLES:

add_stars <- function(pval) {
  if (pval <= 0.01) return("***")
  else if (pval <= 0.05) return("**")
  else if (pval <= 0.1) return("*")
  else return("")
}

tableXvar <- function(X.var, name){
  
  # Extract coefficients
  coefficients <- coef(X.var)
  
  # Extract standard errors
  se_matrix <- sqrt(diag(vcov(X.var)))
  
  # Assuming the coefficients are structured with equations as rows and variables as columns
  num_eqs <- length(coefficients)  # Number of equations
  num_vars <- nrow(coefficients[[1]])  # Number of variables (assuming all equations have the same number of variables)
  
  formatted_matrix <- matrix("", num_eqs * 2, num_vars)
  
  # Fill the matrix with estimates and standard errors in separate rows:
  for (i in 1:num_eqs) {
    for (j in 1:num_vars) {
      estimate <- coefficients[[i]][j, "Estimate"]
      std_error <- coefficients[[i]][j, "Std. Error"]
      p_value <- coefficients[[i]][j, "Pr(>|t|)"]
      stars <- add_stars(p_value)
      
      formatted_matrix[2*i - 1, j] <- sprintf("%f%s", estimate, stars)
      formatted_matrix[2*i, j] <- sprintf("(%f)", std_error)
    }
  }
  
  
  # Create the LaTeX table
  latex_table <- xtable(formatted_matrix, align=rep("c", num_vars + 1), digits=4)
  if (! "_"%in% name){name <- paste0("_", name)}
  print(latex_table, type="latex", file=paste0("tab/X_var", name, ".txt"))
  
}

tableR2 <- function(system_r, system_unr){
  
  takeR2 <- function(system){return(sapply(system$eq, `[[`, "r.squared"))}
  
  system_r    <- summary(system_r)
  system_r_R2 <- takeR2(system_r)
  
  system_unr    <- summary(system_unr)
  system_unr_R2 <- takeR2(system_unr)
  
  table <- data.frame(n             = c("4", "8", "12", "16", "20", "38", "40", "60"),
                      system_r_R2   = system_r_R2,
                      system_unr_R2 = system_unr_R2)
  
  return(table)
}

tableR2toLATEX <- function(df){
  
  # Function to generate a single panel of LaTeX table
  generatePanel <- function(panelName, model, df, systems){
    # Create the LaTeX table header for a panel
    panel <- sprintf("
\\textbf{Panel %s: %s} \\\\\\hline
 & 1 & 2 & 3 & 4 & 5 & 6 & 7 & 8 \\\\
\\toprule
", panelName, model)
    
    for(system in systems){
      vals <- round(df[df$model == model, system], 2)
      
      # Construct the row for this system
      row_data <- paste(system, paste(vals, collapse = " & "), sep = " & ")
      
      # Add the row to the panel
      panel <- paste(panel, row_data, "\\\\ \n", sep="")
      
    }
    
    return(panel)
  }
  
  # Extract system_r_R2 and system_unr_R2 rows
  systems <- c('system_r_R2', 'system_unr_R2')
  
  # Generate panels for ACM and FF
  acm_panel <- generatePanel("A", "ACM", df, systems)
  ff_panel <-  generatePanel("B", "FF", df, systems)
  ff_panel <-  paste(ff_panel,"\\bottomrule", "\n", sep="")
  
  
  # Combine panels to create the LaTeX table
  latex_table <- sprintf("
\\begin{tabular}{l*{8}{c}}
%s
\\midrule
%s
\\end{tabular}
", acm_panel, ff_panel)
  
  # Replace system names with meaningful descriptions
  latex_table <- gsub('system_r_R2', 'restricted', latex_table)
  latex_table <- gsub('system_unr_R2', 'unrestricted', latex_table)
  
  writeLines(latex_table, "tab/R2.txt")
}

## -----------------------------------------------------------------------------
## FIGURES:

addRecessions <-function(p){
  # p <- p + geom_rect(aes(xmin=as.Date("1974-07-01"), xmax=as.Date("1975-03-01"),  ymin=-Inf, ymax=Inf), 
  #                   fill="grey88", alpha=0.1, inherit.aes = FALSE)
  p <- p + geom_rect(aes(xmin=as.Date("1980-02-01"), xmax=as.Date("1982-11-01"),  ymin=-Inf, ymax=Inf), 
                     fill="gray85", alpha=0.5, inherit.aes = FALSE)
  p <- p + geom_rect(aes(xmin=as.Date("1990-08-01"), xmax=as.Date("1991-03-01"),  ymin=-Inf, ymax=Inf), 
                     fill="gray85", alpha=0.5, inherit.aes = FALSE)
  p <- p + geom_rect(aes(xmin=as.Date("2001-04-01"), xmax=as.Date("2001-11-01"),  ymin=-Inf, ymax=Inf), 
                     fill="gray85", alpha=0.5, inherit.aes = FALSE)
  p <- p + geom_rect(aes(xmin=as.Date("2008-01-01"), xmax=as.Date("2009-06-01"),  ymin=-Inf, ymax=Inf), 
                     fill="gray85", alpha=0.5, inherit.aes = FALSE)
  p <- p + geom_rect(aes(xmin=as.Date("2020-03-01"), xmax=as.Date("2020-04-01"),  ymin=-Inf, ymax=Inf), 
                     fill="gray85", alpha=0.5, inherit.aes = FALSE)
  return(p)
}

plotPC <- function(name){
  plot_df <- data.frame(
    date = index(X.ts),
    PC1  = X.ts[,1],
    PC2  = X.ts[,2],
    PC3  = X.ts[,3],
    PC4  = X.ts[,4],
    PC5  = X.ts[,5]
  )
  
  library(tidyr)
  plot_df_long <- plot_df %>% 
    gather(key = 'PC', value = 'Value', -date)
  
  #color_palette <- rev(colorRampPalette(c("lightblue", "navyblue"))(length(unique(plot_df_long$PC))))
  #color_palette <- rev(c("#add8e6", "#87ceeb", "#6495ed", "blue", "darkblue")) # Light to dark blues
  color_palette <- rev(c("#add8e6", "#87ceeb", "#6495ed", "#4169e1", "darkblue")) # Light to dark blues
  
  p <- ggplot(plot_df_long, aes(x = date, y = Value, color = PC, linetype = PC)) 
  p <- addRecessions(p)  + geom_line(size = 0.66, linetype = "solid") +
    scale_color_manual(values = setNames(color_palette, levels(plot_df_long$PC))) + 
    labs(x = "", y = "") +
    theme(
      axis.text.x = element_text(size = 14),  # Adjust the size as desired for the x-axis tick labels
      axis.text.y = element_text(size = 14),  # Adjust the size as desired for the y-axis tick labels
      axis.title.x = element_text(size = 16),  # Adjust the size as desired for the x-axis label
      axis.title.y = element_text(size = 16),  # Adjust the size as desired for the y-axis label
      panel.background  = element_rect(fill = "white"),
      plot.background   = element_rect(fill = "white"),
      panel.grid.major  = element_line(color = "grey92", linetype = "solid"),
      panel.grid.minor  = element_line(color = "grey95", linetype = "dotted"),
      legend.position   = c(0.9,0.3), # Optional: Change position if needed
      legend.background = element_rect(fill="white", colour="grey"),
      legend.key.size = unit(1, "cm"),
      legend.text = element_text(size = 14),
      #legend.background = element_blank(),   # Remove legend background
      legend.key = element_blank()           # Remove legend key background
    ) + 
    guides(color=guide_legend(title=NULL))
  ggsave(filename = paste0("fig/fig_PC_", name, ".png"), plot = p, width = 7, height = 5, bg = "transparent")
  print(p)
}

plotTermStructure<- function(){
  y.plot <- y.ts
  N <- length(colnames(y.ts))
  y.plot.df <-data.frame(index=index(y.ts), Data=coredata(y.ts))
  colnames(y.plot.df) <- c("date", colnames(y.ts))

  long_data <- y.plot.df %>%
    gather(key = "Variable", value = "Value", starts_with("n"))
  # Set the levels for the "Variable" factor to ensure the desired color mapping
  long_data$Variable <- factor(long_data$Variable, levels = unique(long_data$Variable))
  # Define a custom color palette from pale blue to dark blue for the y variables
  color_palette <- colorRampPalette(c("lightblue", "darkblue"))(length(unique(long_data$Variable)))
  
  
  keep_levels <- c("n1", "n4", "n20", "n40", "n60") # adjust this to your preference
  
  p <- ggplot(long_data, aes(x = date, y = Value, color = Variable)) 
  p <- addRecessions(p)  + geom_line(size = 0.77, linetype = "solid") +
    labs(title = "",
         x = "", y = "") +
    scale_color_manual(values = setNames(color_palette, levels(long_data$Variable))) +  # Manually set colors
    #geom_line(data = subset_df, aes(x = Dates, y = y3m_cc), color = "red", size = 1) +  # Plot x in red
    theme_minimal() +
    theme(axis.line = element_line(color = "black")) +
    scale_x_date(date_breaks = "5 year", date_labels = "%Y") +
    theme(axis.text.x = element_text(size = 14),  # Adjust the size as desired for the x-axis tick labels
          axis.text.y = element_text(size = 14),  # Adjust the size as desired for the y-axis tick labels
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          legend.position = "none" # This removes the legend completely
          ) +
    guides(color=guide_legend(title=NULL))
  
  ggsave(filename = paste0("fig/commonTrend.png"), plot = p, width = 7, height = 5, bg = "transparent")

}

plotHPER<- function(){
  per_columns <- grep("hper", colnames(data.ts), value = TRUE)
  # Now, use these to select the columns from the xts object
  hper.ts <- data.ts[, per_columns]
  hper.plot <- hper.ts
  N <- length(colnames(hper.ts))
  hper.plot.df <-data.frame(index=index(hper.ts), Data=coredata(hper.ts))
  colnames(hper.plot.df) <- c("date", colnames(hper.ts))
  
  long_data <- hper.plot.df %>%
    gather(key = "Variable", value = "Value", starts_with("hper"))
  # Set the levels for the "Variable" factor to ensure the desired color mapping
  long_data$Variable <- factor(long_data$Variable, levels = unique(long_data$Variable))
  # Define a custom color palette from pale blue to dark blue for the y variables
  color_palette <- colorRampPalette(c("lightblue", "darkblue"))(length(unique(long_data$Variable)))
  
  
  keep_levels <- c("n1", "n4", "n20", "n40", "n60") # adjust this to your preference
  
  p <- ggplot(long_data, aes(x = date, y = Value, color = Variable)) 
    p <- addRecessions(p)  + geom_line(size = 0.77, linetype = "solid") +
    labs(title = "",
         x = "", y = "") +
    scale_color_manual(values = setNames(color_palette, levels(long_data$Variable))) +  # Manually set colors
    #geom_line(data = subset_df, aes(x = Dates, y = y3m_cc), color = "red", size = 1) +  # Plot x in red
    theme_minimal() +
    theme(axis.line = element_line(color = "black")) +
    scale_x_date(date_breaks = "5 year", date_labels = "%Y") +
    theme(axis.text.x = element_text(size = 14),  # Adjust the size as desired for the x-axis tick labels
          axis.text.y = element_text(size = 14),  # Adjust the size as desired for the y-axis tick labels
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          legend.position = "none" # This removes the legend completely
    ) +
    guides(color=guide_legend(title=NULL))
  
  ggsave(filename = paste0("fig/hper.png"), plot = p, width = 7, height = 5, bg = "transparent")
  
}

convertQuartersToYears <- function(q) {
  if (q >= 4) {
    return(paste0(q/4, "Y"))
  } else if (q < 1) {
    return(paste0(q, "Q"))
  } else {
    return(paste0(q, "Q"))
  }
}

plotYieldvsTrend <- function(number, name){
  
  if (name == "FF"){
  dates <- index(y.ts[current_time_window])
  
  Actual <- y.ts[dates, paste0("n", number)]
  colnames(Actual) <- "Actual"
  
  Trend  = y_trend.ts[dates, paste0("n", number)]
  colnames(Trend) <- "Trend"
  
  plot_df <- data.frame(
    date   = dates,
    Actual = Actual,
    Trend = Trend
  )
  
  library(tidyr)
  plot_df_long <- plot_df %>% 
    gather(key = 'Type', value = 'Value', -date)
  
  p <- ggplot(plot_df_long, aes(x = date, y = Value, color = Type, linetype = Type)) 
  p <- addRecessions(p)  + geom_line(size = 0.66, linetype = "solid") + 
    scale_color_manual(values = c("Actual" = "blue", "Trend" = "red"),name=NULL) +
    scale_linetype_manual(values = c("Actual" = "solid", "Trend" = "dashed"),name=NULL) +
    labs(x = "", y = "") +
    theme(
      axis.text.x = element_text(size = 14),  # Adjust the size as desired for the x-axis tick labels
      axis.text.y = element_text(size = 14),  # Adjust the size as desired for the y-axis tick labels
      axis.title.x = element_text(size = 16),  # Adjust the size as desired for the x-axis label
      axis.title.y = element_text(size = 16),  # Adjust the size as desired for the y-axis label
      panel.background  = element_rect(fill = "white"),
      plot.background   = element_rect(fill = "white"),
      panel.grid.major  = element_line(color = "grey92", linetype = "solid"),
      panel.grid.minor  = element_line(color = "grey95", linetype = "dotted"),
      legend.position   = c(0.825,0.825), # Optional: Change position if needed
      legend.background = element_rect(fill="white", colour="grey"),
      legend.key.size = unit(1.5, "cm"),
      legend.text = element_text(size = 14),
      #legend.background = element_blank(),   # Remove legend background
      legend.key = element_blank()           # Remove legend key background
    ) + 
    guides(color=guide_legend(title=NULL))
  
  ggsave(filename = paste0("fig/fig_acttrend_", convertQuartersToYears(number), "_", name, ".png"), plot = p, width = 7, height = 5, bg = "transparent")
  }
}

plotYieldvsFittedYield <- function(number, name) {
  
  dates <- index(y.ts[current_time_window])
  
  Actual <- y.ts[dates, paste0("n", number)]
  colnames(Actual) <- "Actual"
  
  Fitted  = y.ts.fit[dates, paste0("n", number)]
  colnames(Fitted) <- "Fitted"

  plot_df <- data.frame(
    date   = dates,
    Actual = Actual,
    Fitted = Fitted
  )
  
  library(tidyr)
  plot_df_long <- plot_df %>% 
    gather(key = 'Type', value = 'Value', -date)
  
  p <- ggplot(plot_df_long, aes(x = date, y = Value, color = Type, linetype = Type)) 
  p <- addRecessions(p)  + geom_line(size = 0.66, linetype = "solid") + 
    scale_color_manual(values = c("Actual" = "blue", "Fitted" = "red"),name=NULL) +
    scale_linetype_manual(values = c("Actual" = "solid", "Fitted" = "dashed"),name=NULL) +
    labs(x = "", y = "") +
    theme(
      axis.text.x = element_text(size = 14),  # Adjust the size as desired for the x-axis tick labels
      axis.text.y = element_text(size = 14),  # Adjust the size as desired for the y-axis tick labels
      axis.title.x = element_text(size = 16),  # Adjust the size as desired for the x-axis label
      axis.title.y = element_text(size = 16),  # Adjust the size as desired for the y-axis label
      panel.background  = element_rect(fill = "white"),
      plot.background   = element_rect(fill = "white"),
      panel.grid.major  = element_line(color = "grey92", linetype = "solid"),
      panel.grid.minor  = element_line(color = "grey95", linetype = "dotted"),
      legend.position   = c(0.825,0.825), # Optional: Change position if needed
      legend.background = element_rect(fill="white", colour="grey"),
      legend.key.size = unit(1.5, "cm"),
      legend.text = element_text(size = 14),
      #legend.background = element_blank(),   # Remove legend background
      legend.key = element_blank()           # Remove legend key background
      ) + 
    guides(color=guide_legend(title=NULL))
  
  ggsave(filename = paste0("fig/fig_acfit_ff_", convertQuartersToYears(number), "_", name, ".png"), plot = p, width = 7, height = 5, bg = "transparent")
  
}

preparePredList <- function(name){
  train_period <- "1980-09-01/2023-09-01"
  forecast_window.length <- floor((2033-2023)*4)
  forecast_window.dates <-seq(as.Date("2023-12-01"),length=forecast_window.length, by="quarters")
  
  X_train.ts  <- X.ts["1980-09-01/2023-09-01"]
  X_train.var <- VAR(X_train.ts, p = 1, type = "const") 
  X_train.for <- predict(X_train.var, n.ahead = forecast_window.length)
  X_in.mat    <- fitted(X_train.var) 
  X_for.mat   <- cbind(X_train.for$fcst$PC1[,1],X_train.for$fcst$PC2[,1],X_train.for$fcst$PC3[,1],
                       X_train.for$fcst$PC4[,1],X_train.for$fcst$PC5[,1])
  dates <- c(index(X_train.ts),forecast_window.dates)
  pred_list <- list()
  for (n in c(1, 40)) {
    
    if (name=="FF"){
      # Create prediction
      y_cycle_OoS <- t(-(A[n]+t(B[, n])%*%t(X_for.mat))/ttm[n])
      y_cycle_IN  <- t(-(A[n]+t(B[, n])%*%t(X_in.mat))/ttm[n])
      
      # Convert prediction to xts time series
      y_cycle_OoS.ts <- xts((y_cycle_OoS), order.by = forecast_window.dates) # assuming dates_for remains constant
      y_cycle_IN.ts <- xts(y_cycle_IN, order.by = index(X_train.ts)[-1])
      
      y_OoS.ts <- merge(y_cycle_OoS.ts, y_trend.ts[,n],join="outer")
      y_IN.ts  <- merge(y_cycle_IN.ts, y_trend.ts[,n],join="outer")
      
      colnames(y_OoS.ts) <- c("cycle", "trend")
      colnames(y_IN.ts) <- c("cycle", "trend")
      
      y_OoS.ts <- y_OoS.ts$cycle + y_OoS.ts$trend
      y_IN.ts  <- y_IN.ts$cycle + y_IN.ts$trend
    } else {
      # Create prediction
      y_OoS <- t(-(A[n]+t(B[, n])%*%t(X_for.mat))/ttm[n])
      y_IN  <- t(-(A[n]+t(B[, n])%*%t(X_in.mat))/ttm[n])
      
      # Convert prediction to xts time series
      y_OoS.ts <- xts((y_OoS), order.by = forecast_window.dates) # assuming dates_for remains constant
      y_IN.ts <- xts(y_IN, order.by = index(X_train.ts)[-1])
      
    }
    
    # Merge time series
    pred.ts <- xts(merge(y.ts[dates, paste0("n", n)], na.omit(y_OoS.ts), join = 'outer'), dates)
    pred.ts <- xts(merge(pred.ts, na.omit(y_IN.ts), join = 'outer'), dates)
    pred.ts <- pred.ts["1980-09-01/2033-09-01"]
    
    # Set column names
    colnames(pred.ts) <- c(paste0("n", n), paste0("n", n, "_OoS"), paste0("n", n, "_IN"))
    
    # Append to the results list
    pred_list[[paste0("n", n)]] <- pred.ts
    
  }
  return(pred_list)
}

plotOoSYields <- function(pred_list, name){
  for (n in c(1,40)){
    pred.ts   <- pred_list[[paste0("n",n)]]
    colnames(pred.ts) <- c("Actual", "OoS", "IN")
    pred.plot <- data.frame(
      date   = index(pred.ts),
      Actual = pred.ts$Actual,
      OoS    = pred.ts$OoS,
      IN     = pred.ts$IN
    )
    library(tidyr)
    plot_df_long <- pred.plot %>% gather(key = 'Type', value = 'Value', -date)
    
    p <- ggplot(plot_df_long, aes(x = date, y = Value, color = Type, linetype = Type)) +
      geom_line(size = 0.5) +
      scale_color_manual(values = c("Actual" = "blue", "OoS" = "red", "IN" = "red"),name=NULL) +
      scale_linetype_manual(values = c("Actual" = "solid", "OoS" = "dashed", "IN"="solid"),name=NULL) +
      labs(x = "", y = "") +
      theme(
        axis.text.x = element_text(size = 14),  # Adjust the size as desired for the x-axis tick labels
        axis.text.y = element_text(size = 14),  # Adjust the size as desired for the y-axis tick labels
        axis.title.x = element_text(size = 16),  # Adjust the size as desired for the x-axis label
        axis.title.y = element_text(size = 16),  # Adjust the size as desired for the y-axis label
        panel.background  = element_rect(fill = "white"),
        plot.background   = element_rect(fill = "white"),
        panel.grid.major  = element_line(color = "grey92", linetype = "solid"),
        panel.grid.minor  = element_line(color = "grey95", linetype = "dotted"),
        legend.position   = c(0.825,0.825), # Optional: Change position if needed
        legend.background = element_rect(fill="white", colour="grey"),
        legend.key.size = unit(1.5, "cm"),
        legend.text = element_text(size = 14),
        #legend.background = element_blank(),   # Remove legend background
        legend.key = element_blank()           # Remove legend key background
      ) + 
      guides(color=guide_legend(title=NULL))
      ggsave(filename = paste0("fig/fig_OoS_", convertQuartersToYears(n), "_", name, ".png"), plot = p, width = 7, height = 5, bg = "transparent")
      write.csv(pred.plot, file=paste0("data/output/forecast_", convertQuartersToYears(n), "_", name, ".csv"), row.names = TRUE)
      
  }
}

plotTermPremia = function(name){
  pred.plot <- data.frame(
    date = index(termpremia.ts),
    TP10Y = termpremia.ts[,"n40"],
    TP1Y  = termpremia.ts[, "n4"]
  )
  colnames(pred.plot) <- c("date", "TP10Y", "TP1Y")
  plot_df_long <- pred.plot %>% gather(key = 'Type', value = 'Value', -date)
  p <- ggplot(plot_df_long, aes(x = date, y = Value, color = Type, linetype = Type)) 
  p <- addRecessions(p) + geom_line(size = 0.75) 
  p <- p + scale_color_manual(values = c("TP10Y" = "lightsalmon", "TP1Y" = "darkred"),name=NULL) +
    scale_linetype_manual(values = c("TP10Y" = "solid", "TP1Y" = "solid"),name=NULL) +
    labs(x = "", y = "") +
    theme(
      axis.text.x = element_text(size = 14),  # Adjust the size as desired for the x-axis tick labels
      axis.text.y = element_text(size = 14),  # Adjust the size as desired for the y-axis tick labels
      axis.title.x = element_text(size = 16),  # Adjust the size as desired for the x-axis label
      axis.title.y = element_text(size = 16),  # Adjust the size as desired for the y-axis label
      panel.background  = element_rect(fill = "white"),
      plot.background   = element_rect(fill = "white"),
      panel.grid.major  = element_line(color = "grey92", linetype = "solid"),
      panel.grid.minor  = element_line(color = "grey95", linetype = "dotted"),
      legend.position   = c(0.825,0.825), # Optional: Change position if needed
      legend.background = element_rect(fill="white", colour="grey"),
      legend.key.size = unit(1.5, "cm"),
      legend.text = element_text(size = 14),
      #legend.background = element_blank(),   # Remove legend background
      legend.key = element_blank()           # Remove legend key background
    ) + 
    guides(color=guide_legend(title=NULL))
  ggsave(filename = paste0("fig/fig_TP_10y_vs_1y_", name, ".png"), plot = p, width = 7, height = 5, bg = "transparent")
}


plotForecasts = function(){
  for (n in c(1,40)){
    ACM <- read.csv(paste0("data/output/forecast_", convertQuartersToYears(n), "_ACM.csv"))
    ACM$ACM <- coalesce(ACM$IN, ACM$OoS)
    FF  <- read.csv(paste0("data/output/forecast_", convertQuartersToYears(n), "_FF.csv"))
    FF$FF <- coalesce(FF$IN, FF$OoS)
    pred.plot <- data.frame(
      date   = as.Date(ACM$date),
      Actual = ACM$Actual,
      ACM    = ACM$ACM,
      FF     = FF$FF
    )
    library(tidyr)
    plot_df_long <- na.omit(pred.plot) %>% 
      gather(key = 'Model', value = 'Value', -date)
    p <- ggplot(plot_df_long, aes(x = date, y = Value, color = Model, linetype = Model)) +
      geom_rect(aes(xmin=as.Date("2006-03-01"), xmax=rev(pred.plot$date)[1],  ymin=-Inf, ymax=Inf), 
                fill="grey85", alpha=0.5, inherit.aes = FALSE)+
      geom_line(size = 0.75, linetype="solid") +
    scale_color_manual(values = c("Actual" = "blue", "FF" = "red", "ACM" = "darkgreen"),name=NULL) +
      labs(x = "", y = "") +
      theme(
        axis.text.x = element_text(size = 14),  # Adjust the size as desired for the x-axis tick labels
        axis.text.y = element_text(size = 14),  # Adjust the size as desired for the y-axis tick labels
        axis.title.x = element_text(size = 16),  # Adjust the size as desired for the x-axis label
        axis.title.y = element_text(size = 16),  # Adjust the size as desired for the y-axis label
        panel.background  = element_rect(fill = "white"),
        plot.background   = element_rect(fill = "white"),
        panel.grid.major  = element_line(color = "grey92", linetype = "solid"),
        panel.grid.minor  = element_line(color = "grey95", linetype = "dotted"),
        legend.position   = c(0.78,0.75), # Optional: Change position if needed
        legend.background = element_rect(fill="white", colour="grey"),
        legend.key.size = unit(1.5, "cm"),
        legend.text = element_text(size = 14),
        #legend.background = element_blank(),   # Remove legend background
        legend.key = element_blank()           # Remove legend key background
      ) + 
      guides(color=guide_legend(title=NULL)) 
    p
    ggsave(filename = paste0("fig/fig_forecasts_", convertQuartersToYears(n), ".png"), plot = p, width = 7, height = 5, bg = "transparent")
    
  }
}