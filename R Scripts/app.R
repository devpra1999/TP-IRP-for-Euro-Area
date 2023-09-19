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
library(plm)

source("Historical.R")
source("ACM.R")
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
        tabPanel("Historical Yield Decompositions using Consensus Forecasts",
           highchartOutput("yieldplot"),
           htmlOutput("yield_decomposition"),
           p("Yield Decomposition using Consensus Forecasts",align = "center", style = "font-family: Lucida Grande, Lucida Sans Unicode; color: black; font-size: 18px"),
           fluidRow(
             column(6, highchartOutput("germany_tp_cf")),
             column(6, highchartOutput("italy_tp_cf"))
            ),
           fluidRow(
             column(6, highchartOutput("france_tp_cf")),
             column(6, highchartOutput("spain_tp_cf"))
           )
#           h3("Case Study - Term Premia using Affine Term Structure Model"),
#           htmlOutput("ACM"),
#           highchartOutput("germany_acm"),
#           htmlOutput("ACM_compare"),
#           highchartOutput("germany_acm_cf_mp"),
#           highchartOutput("germany_acm_cf_tp")
        ),
        tabPanel("Historical Yield Decompositions using Affine Model",
                 h3("Case Study - Term Premia using Affine Term Structure Model"),
                 htmlOutput("ACM"),
                 highchartOutput("germany_acm"),
                 htmlOutput("ACM_compare"),
                 highchartOutput("germany_acm_cf_mp"),
                 highchartOutput("germany_acm_cf_tp")
        ),
        tabPanel("Current Projections",
                 h3("Expected Path of short-term rates"),
                 highchartOutput("rateplot"),
                 fluidRow(
                   column(12, align="center", tableOutput('table'))
                 ),
                 htmlOutput("prompt"),
                 h3("Your Forecasts for Short-Term Rates"),
                 fixedRow(
                   column(4, numericInput("F1", label = "1 quarter ahead", value = round(fut_rate_consensus[2], 1))),
                   column(4, numericInput("F2", label = "2 quarters ahead", value = round(fut_rate_consensus[3], 1))),
                   column(4, numericInput("F3", label = "3 quarters ahead", value = round(fut_rate_consensus[4], 1)))
                 ),
                 fixedRow(
                   column(4, numericInput("F4", label = "4 quarters ahead", value = round(fut_rate_consensus[5], 1))),
                   column(4, numericInput("F5", label = "5 quarters ahead", value = round(fut_rate_consensus[6], 1))),
                   column(4, numericInput("F6", label = "6 quarters ahead", value = round(fut_rate_consensus[7], 1)))
                 ),
                 fixedRow(
                   column(4, numericInput("F7", label = "7 quarters ahead", value = round(fut_rate_consensus[8], 1))),
                   column(4, numericInput("F8", label = "8 quarters ahead", value = round(fut_rate_consensus[9], 1))),
                   column(4, numericInput("F9", label = "9 quarters ahead", value = round(fut_rate_consensus[10], 1)))
                 ),
                 fixedRow(
                   column(4, numericInput("F10", label = "10 quarters ahead", value = round(fut_rate_consensus[11], 1))),
                   column(4, numericInput("F11", label = "11 quarters ahead", value = round(fut_rate_consensus[12], 1))),
                   column(4, numericInput("F12", label = "12 quarters ahead", value = round(fut_rate_consensus[13], 1)))
                 ),
                 actionButton("go", "Submit"),
                 highchartOutput("rateplot_user"),
                 h3("Yield Decompostions"),
                 fluidRow(
                   column(12, align="center", tableOutput('table_user'))
                 )
        )
)

server <- function(input, output) {
    #EXPLANATION + HISTORICAL ---------------------------------------------------------------
    output$yield_decomposition <- renderUI({
      HTML(
      "The figure above shows the historical yields since 2010 for 10-year government bonds for
      the major economies in the Euro Area - Germany, France, Italy and Spain. <br>
      <br>
      The 10-year yields can be decomposed into two parts - <br>
      <b>1. Monetary Policy component</b> - It is the average expected monetary policy rate (short rate)
      over the residual life of the long-term bond. <br>
      <b>2. Term Premia</b> - It can be understood as premia on buying a long term (10 year) bond over
      buying roll-over short-term bonds over the period of the long-term bond. More detials are
      available in the Methodology section. <br>
      <br>
      We construct estimates for the monetary policy component (and the term premia) using the consensus
      forecasts for the expected path of short rates from the ECB survey of Professional Forecasters (SPF).<br><br>"
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
    
    output$germany_acm <- renderHighchart({
      Germany_Term_Premia_ACM
    })
    
    output$germany_acm_cf_tp <- 
      renderHighchart({
        Germany_ACM_CF_TP
      })
    
    output$germany_acm_cf_mp <- 
      renderHighchart({
        Germany_ACM_CF_MP
      })
    
    output$ACM <- renderUI({
      HTML(
        "The computation of the term premia requires the expected path of short
        rates. These can be estimated using the term structure models. As a result
        affine term structure models are a popular way of determing the term premia.<br>
        <br>
        <b>Adrian, Crump and Moench (2012)</b> propose a 3-step linear regression based 
        approach to price the term structure, henceforth called the <b>ACM model</b>. We
        use this approach to compute the term premia for Germany. The yields at 
        different maturities, required by the model, are obtained using the  Nelson-Siegel-Svensson
        (NSS) parameters provided by Bundesbank. The results are shown
        below -"
      )
    })
    
    output$ACM_compare <- renderUI({
      HTML(
        "The <b>monetary policy (MP) component</b> is computed in 2 steps - (1) The expected 
        short rates are extracted from the model, and (2) the MP component is calculated using 
        the methodology used for the other forecasts. The <b>risk neutral yield</b>, on the 
        other hand, is generated by setting the market price for risk to zero in the model.<br>
        <br>
        Both the curves follow each other closely. There are two instances of sharp deviations 
        due to spikes in the MP component (generated by spikes in the 1M rates obtained from 
        NSS interpolation). The term premia is calculated using the risk neutral yields 
        (which are equivalent to the MP component).<br>
        <br>
        This affine-model-based MP component and term premia are compare below - "
      )
    })
    
    output$prompt <- renderUI({
      HTML(
        "The figure above plots the expected path of future short (monetary policy) 
        rates using the naive model, consensus forecasts and ACM (affine) model. The 
        first two are forecasts for 3-month rates, while the ACM model forecasts are 
        for the 1-month rates.<br>
        <br>
        You can provide your own forecasts for the short-term (3 month) rates, and click
        submit to get the current term premia estimates based on your forecasts for the 
        Euro Area countries. The default rates in the input boxes are the consensus forecasts
         for the respective period."
      )
    })
    
    output$rateplot <- renderHighchart({
      Projections
    })
    
    output$table <- renderTable({
      main_table
    }, bordered = TRUE, align = "c")
    
    #DEFINE REACTIVE FUNCTION FOR INPUTS-------------------------------------------------------
    user_forecasts <- eventReactive(input$go, {
      user_forecasts <- c(input$F1,input$F2,input$F3,input$F4,input$F5,input$F6,
                          input$F7,input$F8,input$F9,input$F10,input$F11,input$F12)
    })
    
    
    #PLOTS AND TABLES WITH USER PROVIDED FORECASTS----------------------------------------------
    output$rateplot_user <- renderHighchart({
      highchart() %>%
        hc_xAxis(type = "datetime", dateTimeLabelFormats = list(month = "%b %Y")) %>%
        hc_yAxis(title = list(text = "Interest Rate"), min = min(-1,user_forecasts()), max = max(5,user_forecasts())) %>%
        hc_add_series(data = recent_data, hcaes(x = Date, y = Rate),
                      type = "line", name = "Historical", color = "black", lineWidth = 2) %>%
        hc_add_series(data = data.frame(x = fut_date, y = fut_rate_mod), hcaes(x = x, y = y),
                      type = "line", name = "Model estimate", color = "blue", lineWidth = 2, dashStyle = "Dash") %>%
        hc_add_series(data = data.frame(x = fut_date, y = fut_rate_consensus), hcaes(x = x, y = y),
                      type = "line", name = "Consensus forecast", color = "red", lineWidth = 2, dashStyle = "Dash") %>%
        hc_add_series(data = data.frame(x = plot_dates[(T-3):T], y = fittedYields[(T-3):T,1]*100), hcaes(x = x, y = y),
                      type = "line", name = "Historical_ACM (1M)", color = "black", lineWidth = 2, dashStyle = "Dash") %>%
        hc_add_series(data = data.frame(x = plot_dates_proj, y = ESTR[T,1:36]*100), hcaes(x = x, y = y),
                      type = "line", name = "ACM_Projection (1M)", color = "brown", lineWidth = 2, dashStyle = "Dash") %>%
        hc_add_series(data = data.frame(x = fut_date, y = append(lr$Rate, user_forecasts())), hcaes(x = x, y = y),
                      type = "line", name = "User forecast", color = "brown", lineWidth = 2, dashStyle = "Dash") %>%
        hc_legend(
          layout = "horizontal",
          align = "center",
          verticalAlign = "bottom",
          itemWidth = 200,
          itemStyle = list(textOverflow = "ellipsis")
        ) %>%
        hc_boost(enabled = TRUE) %>%
        hc_exporting(enabled = TRUE)
    })
    
    output$table_user <- renderTable({
      add_user_for(main_table,user_forecasts())
    }, bordered = TRUE, align = "c")
}


# Run the application 
shinyApp(ui = ui, server = server)
