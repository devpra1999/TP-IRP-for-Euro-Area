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
                 withMathJax(includeMarkdown("Report.md"))),
        tabPanel("Historical Data",
           textOutput("desc1"),
           highchartOutput("yieldplot"),
           textOutput("desc2"),
           p("Yield Decompositions",align = "center", style = "font-family: Lucida Grande, Lucida Sans Unicode; color: black; font-size: 18px"),
           fluidRow(
             column(6, highchartOutput("germany_tp")),
             column(6, highchartOutput("italy_tp"))
            ),
           fluidRow(
             column(6, highchartOutput("france_tp")),
             column(6, highchartOutput("spain_tp"))
           )
        ),
        tabPanel("User Forecasts",
           sidebarLayout(
             sidebarPanel(
               h1("Your Forecasts"),
               numericInput("F1", label = "Forecast for Q1", value = round(fut_rate_consensus[2], 1)),
               numericInput("F2", label = "Forecast for Q2", value = round(fut_rate_consensus[3], 1)),
               numericInput("F3", label = "Forecast for Q3", value = round(fut_rate_consensus[4], 1)),
               numericInput("F4", label = "Forecast for Q4", value = round(fut_rate_consensus[5], 1)),
               numericInput("F5", label = "Forecast for Q5", value = round(fut_rate_consensus[6], 1)),
               numericInput("F6", label = "Forecast for Q6", value = round(fut_rate_consensus[7], 1)),
               numericInput("F7", label = "Forecast for Q7", value = round(fut_rate_consensus[8], 1)),
               numericInput("F8", label = "Forecast for Q8", value = round(fut_rate_consensus[9], 1)),
               numericInput("F9", label = "Forecast for Q9", value = round(fut_rate_consensus[10], 1)),
               numericInput("F10", label = "Forecast for Q10", value = round(fut_rate_consensus[11], 1)),
               numericInput("F11", label = "Forecast for Q11", value = round(fut_rate_consensus[12], 1)),
               numericInput("F12", label = "Forecast for Q12", value = round(fut_rate_consensus[13], 1)),
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
    output$desc1 <- renderText({
      string <- "Lorem ipsum dolor sit amet, consectetuer adipiscing elit.Aenean commodo
      ligula eget dolor. Aenean massa. Cum sociis natoque penatibus et magnis dis parturient
      montes, nascetur ridiculus mus. Donec quam felis, ultricies nec, pellentesque eu, pretium
      quis, sem. Nulla consequat massa quis enim. Donec pede justo, fringilla vel, aliquet nec,
      vulputate eget, arcu. In enim justo, rhoncus ut, imperdiet a, venenatis vitae, justo.
      Nullam dictum felis eu pede mollis pretium. Integer tincidunt. Cras dapibus. Vivamus
      elementum semper nisi. Aenean vulputate eleifend tellus. Aenean leo ligula, porttitor eu,
      consequat vitae, eleifend ac, enim. Aliquam lorem ante, dapibus in, viverra quis."
    })
    
    output$yieldplot <- renderHighchart({
      Yield_Plot
    })
    
    output$desc2 <- renderText({
      string <- "Lorem ipsum dolor sit amet, consectetuer adipiscing elit.Aenean commodo
      ligula eget dolor. Aenean massa. Cum sociis natoque penatibus et magnis dis parturient
      montes, nascetur ridiculus mus. Donec quam felis, ultricies nec, pellentesque eu, pretium
      quis, sem. Nulla consequat massa quis enim. Donec pede justo, fringilla vel, aliquet nec,
      vulputate eget, arcu. In enim justo, rhoncus ut, imperdiet a, venenatis vitae, justo.
      Nullam dictum felis eu pede mollis pretium. Integer tincidunt. Cras dapibus. Vivamus
      elementum semper nisi. Aenean vulputate eleifend tellus. Aenean leo ligula, porttitor eu,
      consequat vitae, eleifend ac, enim. Aliquam lorem ante, dapibus in, viverra quis."
    })
    
    output$germany_tp <- renderHighchart({
      Germany_Term_Premia
    })
    
    output$france_tp <- renderHighchart({
      France_Term_Premia
    })
    
    output$italy_tp <- renderHighchart({
      Italy_Term_Premia
    })
    
    output$spain_tp <- renderHighchart({
      Spain_Term_Premia
    })
    
    
    output$desc3 <- renderText({
      string <- "Please add the forecasts for EU short term rates for the next quarters.
      The default value is the consensus forecast for the respective quarter"})
      
    output$desc4 <- renderText({
      string <- "Lorem ipsum dolor sit amet, consectetuer adipiscing elit.Aenean commodo
      ligula eget dolor. Aenean massa. Cum sociis natoque penatibus et magnis dis parturient
      montes, nascetur ridiculus mus. Donec quam felis, ultricies nec, pellentesque eu, pretium
      quis, sem. Nulla consequat massa quis enim. Donec pede justo, fringilla vel, aliquet nec."
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
        hc_yAxis(title = list(text = "Interest Rate"), min = -1, max = 5) %>%
        hc_add_series(data = temp, hcaes(x = Date, y = Rate),
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
