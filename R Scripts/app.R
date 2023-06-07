#clear the environment 
rm(list=ls()) 
## ------------------------------------------------------------------------
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
## -------Load/install packages--------------------------------------------
listofpackages = c("lubridate","zoo","ggplot2","dygraphs","plyr","dplyr",
                   "tidyverse","highcharter","shiny")
for (j in listofpackages){
  if(sum(installed.packages()[, 1] == j) == 0) {
    install.packages(j)
  }
  library(j, character.only = T)
}

source("Historical.R")
source("Future.R")

library(knitr)

rmdfiles <- c("Report/Report.Rmd")
sapply(rmdfiles, knit, quiet = T)

#FUNCTION TO ADD USER FORECAST BASED TERM PREMIA TO THE TABLE
add_user_for <- function(df,rate_forecast,T=40){
  df %>%
    mutate(User_EM = (1 - 1/T)*(rate_forecast[1] - lr$Rate) + (1 - 2/T)*(rate_forecast[2] - rate_forecast[1]) +
             (1 - 3/T)*(rate_forecast[3]- rate_forecast[2]) + (1 - 4/T)*(rate_forecast[4] - rate_forecast[3]),
           User_TP = Yield_10Y - User_EM)
}

ui <- navbarPage("Term Premia in the Euro Area",
        tabPanel("Methodology",
                 withMathJax(includeMarkdown("Report.md"))),
        tabPanel("Historical Data",
           textOutput("desc1"),
           highchartOutput("yieldplot"),
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
               numericInput("F1",label = "Forecast for Q1", value = NA),
               numericInput("F2",label = "Forecast for Q2", value = NA),
               numericInput("F3",label = "Forecast for Q3", value = NA),
               numericInput("F4",label = "Forecast for Q4", value = NA),
               actionButton("go", "Submit"),
               width = 3
             ),
             mainPanel(
               textOutput("desc2"),
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
      highchart() %>%
        hc_title(text = "10-Y Government Bond Yields") %>%
        hc_xAxis(type = "datetime", dateTimeLabelFormats = list(month = "%b %Y")) %>%
        hc_yAxis(title = list(text = "Yield")) %>%
        hc_add_series(data = master_df, hcaes(x = Date, y = Yield_Germany), type = "line", color = "blue", name = "Germany") %>%
        hc_add_series(data = master_df, hcaes(x = Date, y = Yield_France), type = "line", color = "brown", name = "France") %>%
        hc_add_series(data = master_df, hcaes(x = Date, y = Yield_Spain), type = "line", color = "green", name = "Spain") %>%
        hc_add_series(data = master_df, hcaes(x = Date, y = Yield_Italy), type = "line", color = "red", name = "Italy") %>%
        hc_plotOptions(series = list(marker = list(enabled = FALSE))) %>%
        hc_legend(layout = "horizontal", align = "center", verticalAlign = "top") %>%
        hc_colors(c("blue", "brown", "green", "red")) %>%
        hc_exporting(enabled = TRUE)
    })
    
    output$germany_tp <- renderHighchart({
      highchart() %>%
        hc_add_series(master_df, "line", hcaes(x = Date, y = Spread_Germany), name = "Term Spread") %>%
        hc_add_series(master_df, "line", hcaes(x = Date, y = TP_Germany),
                      name = "Term Premia", dashStyle = "dash", color = "red", lineWidth = 2) %>%
        hc_add_series(master_df, "scatter", hcaes(x = Date, y = TP_cf_Germany),
                      name = "Consensus_TP", marker = list(symbol = "circle", lineWidth = 0, radius = 2)) %>%
        hc_title(text = " Germany") %>%
        hc_xAxis(type = "datetime", title = list(text = "Date")) %>%
        hc_yAxis(title = list(text = "Yield & Composition"), min = -2, max = 7) %>%
        hc_legend(enabled = TRUE) %>%
        hc_exporting(enabled = TRUE)
    })
    
    output$france_tp <- renderHighchart({
      highchart() %>%
        hc_add_series(master_df, "line", hcaes(x = Date, y = Spread_France), name = "Term Spread") %>%
        hc_add_series(master_df, "line", hcaes(x = Date, y = TP_France),
                      name = "Term Premia", dashStyle = "dash", color = "red", lineWidth = 2) %>%
        hc_add_series(master_df, "scatter", hcaes(x = Date, y = TP_cf_France),
                      name = "Consensus_TP", marker = list(symbol = "circle", lineWidth = 0, radius = 2)) %>%
        hc_title(text = "France") %>%
        hc_xAxis(type = "datetime", title = list(text = "Date")) %>%
        hc_yAxis(title = list(text = "Yield & Composition"), min = -2, max = 7) %>%
        hc_legend(enabled = TRUE) %>%
        hc_exporting(enabled = TRUE)
    })
    
    output$italy_tp <- renderHighchart({
      highchart() %>%
        hc_add_series(master_df, "line", hcaes(x = Date, y = Spread_Italy), name = "Term Spread") %>%
        hc_add_series(master_df, "line", hcaes(x = Date, y = TP_Italy),
                      name = "Term Premia", dashStyle = "dash", color = "red", lineWidth = 2) %>%
        hc_add_series(master_df, "scatter", hcaes(x = Date, y = TP_cf_Italy),
                      name = "Consensus_TP", marker = list(symbol = "circle", lineWidth = 0, radius = 2)) %>%
        hc_title(text = "Italy") %>%
        hc_xAxis(type = "datetime", title = list(text = "Date")) %>%
        hc_yAxis(title = list(text = "Yield & Composition"), min = -2, max = 7) %>%
        hc_legend(enabled = TRUE) %>%
        hc_exporting(enabled = TRUE)
    })
    
    output$spain_tp <- renderHighchart({
      highchart() %>%
        hc_add_series(master_df, "line", hcaes(x = Date, y = Spread_Spain), name = "Term Spread") %>%
        hc_add_series(master_df, "line", hcaes(x = Date, y = TP_Spain),
                      name = "Term Premia", dashStyle = "dash", color = "red", lineWidth = 2) %>%
        hc_add_series(master_df, "scatter", hcaes(x = Date, y = TP_cf_Spain),
                      name = "Consensus_TP", marker = list(symbol = "circle", lineWidth = 0, radius = 2)) %>%
        hc_title(text = "Spain") %>%
        hc_xAxis(type = "datetime", title = list(text = "Date")) %>%
        hc_yAxis(title = list(text = "Yield & Composition"), min = -2, max = 7) %>%
        hc_legend(enabled = TRUE) %>%
        hc_exporting(enabled = TRUE)
    })
    
    
    output$desc2 <- renderText({
      string <- "Lorem ipsum dolor sit amet, consectetuer adipiscing elit.Aenean commodo
      ligula eget dolor. Aenean massa. Cum sociis natoque penatibus et magnis dis parturient
      montes, nascetur ridiculus mus. Donec quam felis, ultricies nec, pellentesque eu, pretium
      quis, sem. Nulla consequat massa quis enim. Donec pede justo, fringilla vel, aliquet nec."
    })
  
    
    #DEFINE REACTIVE FUNCTION FOR INPUTS-------------------------------------------------------
    user_forecasts <- eventReactive(input$go, {
      user_forecasts <- c(input$F1,input$F2,input$F3,input$F4)
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
                      type = "scatter", name = "Consensus forecast", color = "red", lineWidth = 2, dashStyle = "Dash") %>%
        hc_add_series(data = data.frame(x = fut_date[1:5], y = append(lr$Rate, user_forecasts())), hcaes(x = x, y = y),
                      type = "scatter", name = "User forecast", color = "brown", lineWidth = 2, dashStyle = "Dash") %>%
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
