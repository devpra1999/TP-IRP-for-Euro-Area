#clear the environment 
#rm(list=ls()) 
## ------------------------------------------------------------------------
#setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
## -------Load/install packages--------------------------------------------
#listofpackages = c("lubridate","zoo","ggplot2","dygraphs","plyr","dplyr",
#                   "tidyverse","highcharter")
#for (j in listofpackages){
#  if(sum(installed.packages()[, 1] == j) == 0) {
#    install.packages(j)
#  }
#  library(j, character.only = T, quietly = TRUE, warn.conflicts = FALSE)
#}
library(shiny)
library(lubridate)
library(zoo)
library(ggplot2)
library(dygraphs)
library(plyr)
library(dplyr)
library(tidyverse)
library(highcharter)
library(markdown)

source("Historical.R")
source("Future.R")
source("Plots.R")

#FUNCTION TO ADD USER FORECAST BASED TERM PREMIA TO THE TABLE
add_user_for <- function(df,rate_forecast,T=40){
  df %>%
    mutate(User_EM = (1 - 1/T)*(rate_forecast[1] - lr$Rate) +
             (1 - 2/T)*(rate_forecast[2] - rate_forecast[1]) +
             (1 - 3/T)*(rate_forecast[3] - rate_forecast[2]) +
             (1 - 4/T)*(rate_forecast[4] - rate_forecast[3]) +
             (1 - 5/T)*(rate_forecast[5] - rate_forecast[4]) +
             (1 - 6/T)*(rate_forecast[6] - rate_forecast[5]) +
             (1 - 7/T)*(rate_forecast[7] - rate_forecast[6]) +
             (1 - 8/T)*(rate_forecast[8] - rate_forecast[7]) +
             (1 - 9/T)*(rate_forecast[9] - rate_forecast[8]) +
             (1 - 10/T)*(rate_forecast[10] - rate_forecast[9]) +
             (1 - 11/T)*(rate_forecast[11] - rate_forecast[10]) +
             (1 - 12/T)*(rate_forecast[12] - rate_forecast[11]) +
             lr$Rate,
           User_TP = Yield_10Y - User_EM)
}

ui <- navbarPage("Term Premia in the Euro Area",
        tabPanel("Methodology",
                 tags$iframe(style="height:800px; width:100%", src="Euro_Area_Term_Premia.pdf")),
        tabPanel("Historical Data",
           htmlOutput("desc1"),
           highchartOutput("yieldplot"),
           textOutput("desc2"),
           p("Yield Decomposition using Consensus Forecasts",align = "center", style = "font-family: Lucida Grande, Lucida Sans Unicode; color: black; font-size: 18px"),
           fluidRow(
             column(6, highchartOutput("germany_tp_cf")),
             column(6, highchartOutput("italy_tp_cf"))
            ),
           fluidRow(
             column(6, highchartOutput("france_tp_cf")),
             column(6, highchartOutput("spain_tp_cf"))
           )
        ),
        tabPanel("Current Projections",
           sidebarLayout(
             sidebarPanel(
               h1("Your Forecasts"),
               numericInput("F1", label = "Forecast for 1 quarter ahead", value = round(fut_rate_consensus[2], 1)),
               numericInput("F2", label = "Forecast for 2 quarters ahead", value = round(fut_rate_consensus[3], 1)),
               numericInput("F3", label = "Forecast for 3 quarters ahead", value = round(fut_rate_consensus[4], 1)),
               numericInput("F4", label = "Forecast for 4 quarters ahead", value = round(fut_rate_consensus[5], 1)),
               numericInput("F5", label = "Forecast for 5 quarters ahead", value = round(fut_rate_consensus[6], 1)),
               numericInput("F6", label = "Forecast for 6 quarters ahead", value = round(fut_rate_consensus[7], 1)),
               numericInput("F7", label = "Forecast for 7 quarters ahead", value = round(fut_rate_consensus[8], 1)),
               numericInput("F8", label = "Forecast for 8 quarters ahead", value = round(fut_rate_consensus[9], 1)),
               numericInput("F9", label = "Forecast for 9 quarters ahead", value = round(fut_rate_consensus[10], 1)),
               numericInput("F10", label = "Forecast for 10 quarters ahead", value = round(fut_rate_consensus[11], 1)),
               numericInput("F11", label = "Forecast for 11 quarters ahead", value = round(fut_rate_consensus[12], 1)),
               numericInput("F12", label = "Forecast for 12 quarters ahead", value = round(fut_rate_consensus[13], 1)),
               actionButton("go", "Submit"),
               width = 3
             ),
             mainPanel(
               textOutput("desc3"),
               hr(),
               textOutput("desc4"),
               hr(),
               highchartOutput("rateplot"),
               hr(),
               tableOutput("table"),
               width = 7
             )
           )
        )
)

server <- function(input, output) {
    #EXPLANATION + HISTORICAL ---------------------------------------------------------------
    output$desc1 <- renderUI({
      HTML("The following figures report the 10-year yields and yield decompositions
      for Germany, France, Italy and Spain. The first plot compares the yields of 10 year bonds
      for the four countries from 2010 to the present. The second plot shows the yield decompositions
      separately for all of them. <br>
      <br>
      The yield is decomposed into two parts - <br>
      1. Monetary Policy component - It is the average expected monetary policy rate (short rate)
      over the residual life of the long-term bond. <br>
      2. Term Premia - It can be understood as premia on buying a long term (10 year) bond over
      buying roll-over short-term bonds over the period of the long-term bond. More detials are
      available in the Methodology section. <br>
      <br>
      The next section shows the current projections for the interest rate computed term premia.
      You can add your interest rate forecasts and get the resultant term premia estimates for the countries<br>
      <br>
      Note - The monetary policy rates are obtained using consensus forecasts for the short term rates from the
      ECB Survey of Professional Forecasters (SPF). Since the consensus forecasts are available only quarterly (as opposed to the monthly data used for yields),
      term premia estimates are computed only quarterly (the complete monthly series is interpolated)."
      )
    })
    
    output$yieldplot <- renderHighchart({
      Yield_Plot
    })
    
    output$germany_tp_cf <- renderHighchart({
      Germany_Term_Premia
    })
    
    output$france_tp_cf <- renderHighchart({
      France_Term_Premia
    })
    
    output$italy_tp_cf <- renderHighchart({
      Italy_Term_Premia
    })
    
    output$spain_tp_cf <- renderHighchart({
      Spain_Term_Premia
    })
    
    
    output$desc3 <- renderText({
      string <- "In this section the current value of the term premia for the four countries to
      interest can be constructed. The two default series are built by using the Expected Monetary Policy
      component estimated respectively by the Consensus Forecast and the naive autoregressive model
      for the change in policy rates. Users have the option to add their forecasts for the three
      months rates and see the term premia implied by them."})
      
    output$desc4 <- renderText({
      string <- "Please use the boxes to the left of the page to input your forecasts. Click submit to
      get the term premia estimate based on your forecast. The default values in the boxes are given by
      the consensus forecasts for the respective quarter. The graphs reports the path for monetary policy
      rates while the Table provides the decomposition of current 10-year yields into Term Premia and Expected Monetary Policy."
    })
  
    
    #DEFINE REACTIVE FUNCTION FOR INPUTS-------------------------------------------------------
    user_forecasts <- eventReactive(input$go, {
      user_forecasts <- c(input$F1,input$F2,input$F3,input$F4,input$F5,input$F6,
                          input$F7,input$F8,input$F9,input$F10,input$F11,input$F12)
    })
    
    
    #PLOTS AND TABLES WITH USER PROVIDED FORECASTS----------------------------------------------
    output$rateplot <- renderHighchart({
      highchart() %>%
        hc_xAxis(type = "datetime", dateTimeLabelFormats = list(month = "%b %Y")) %>%
        hc_yAxis(title = list(text = "Interest Rate"), min = min(-1,user_forecasts()), max = max(5,user_forecasts())) %>%
        hc_add_series(data = recent_data, hcaes(x = Date, y = Rate),
                      type = "line", name = "Historical", color = "black", lineWidth = 2) %>%
        hc_add_series(data = data.frame(x = fut_date, y = fut_rate_mod), hcaes(x = x, y = y),
                      type = "line", name = "Model estimate", color = "blue", lineWidth = 2, dashStyle = "Dash") %>%
        hc_add_series(data = data.frame(x = fut_date, y = fut_rate_consensus), hcaes(x = x, y = y),
                      type = "line", name = "Consensus forecast", color = "red", lineWidth = 2, dashStyle = "Dash") %>%
        hc_add_series(data = data.frame(x = fut_date, y = append(lr$Rate, user_forecasts())), hcaes(x = x, y = y),
                      type = "line", name = "User forecast", color = "brown", lineWidth = 2, dashStyle = "Dash") %>%
        hc_legend(
          layout = "horizontal",
          align = "center",
          verticalAlign = "bottom",
          itemWidth = 200,
          itemStyle = list(textOverflow = "ellipsis")
        ) %>%
        hc_exporting(enabled = TRUE)
    })
    
    output$table <- renderTable({
      add_user_for(main_table,user_forecasts())
    })
}


# Run the application 
shinyApp(ui = ui, server = server)
