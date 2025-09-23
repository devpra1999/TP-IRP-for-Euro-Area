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
library(psych)
library(pracma)
library(zoo)
library(lubridate)
library(highcharter)
library(matrixcalc)
library(readxl)
library(xts)
library(xtable)
library(stargazer)
library(ellipse)
library(reshape2)
library(readxl)
#library(xlsx)
library(quantmod)
library(matlib)
library(systemfit)
library(vars)
library(plm)
library(aTSA)
detach("package:dplyr", unload = TRUE)
library(dplyr)
source("Historical.R")
#source("atsm_eu/main.R")
source("ACM.R")
source("Future.R")
source("Plots.R")
source("main.R")
source("US_CF.R")
source("US_Future.R")
source("Plots_US.R")

#FUNCTION TO ADD USER FORECAST BASED TERM PREMIA TO THE TABLE
add_user_for <- function(df,rate_forecast_Y,T=40){
  rate_forecast <- rep(NA,13)
  rate_forecast[1] <- lr$Rate
  rate_forecast[5] <- rate_forecast_Y[1]
  rate_forecast[9] <- rate_forecast_Y[2]
  rate_forecast[13] <- rate_forecast_Y[3]
  rate_forecast <- na.approx(rate_forecast)[-1]
  
  df %>%
    mutate(User_MP = (1 - 1/T)*(rate_forecast[1] - lr$Rate) +
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
           User_TP = Yield_10Y - User_MP) %>%
    select(Country, Yield_10Y, User_MP,User_TP,everything())
}

add_user_for_US <- function(df,rate_forecast){
  rate_forecast <- c(lr_US$V1, rate_forecast[1:3],rep(NA,5),rate_forecast[4])
  rate_forecast <- na.approx(rate_forecast)
  MP <- (1 - 1/T)*(rate_forecast[2] - rate_forecast[1]) +
    (1 - 2/T)*(rate_forecast[3] - rate_forecast[2]) +
    (1 - 3/T)*(rate_forecast[4] - rate_forecast[3]) +
    (1 - 4/T)*(rate_forecast[5] - rate_forecast[4]) +
    (1 - 5/T)*(rate_forecast[6] - rate_forecast[5]) +
    (1 - 6/T)*(rate_forecast[7] - rate_forecast[6]) +
    (1 - 7/T)*(rate_forecast[8] - rate_forecast[7]) +
    (1 - 8/T)*(rate_forecast[9] - rate_forecast[8]) +
    (1 - 9/T)*(rate_forecast[10] - rate_forecast[9])+ rate_forecast[1]
  Yield = df[3,2]
  TP = Yield - MP
  df[4,] = c("User",Yield,TP,MP)
  df[,2:4] <- sapply(df[,2:4],as.numeric)
  df[,2:4] <- round(df[,2:4],2)
  df
}

ui <- navbarPage("Term Premia",
                 tabPanel("Euro Area",
                          tabsetPanel(tabPanel("Methodology",
                                               tags$iframe(style="height:800px; width:100%", src="Euro_Area_Term_Premia_extended.pdf")),
                                      tabPanel("Term Premia (Consensus)",
                                               highchartOutput("yieldplot"),
                                               htmlOutput("yield_decomposition"),
                                               p("10Y Yield Decomposition using Consensus Forecasts",align = "center", style = "font-family: Lucida Grande, Lucida Sans Unicode; color: black; font-size: 18px"),
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
                                      tabPanel("Term Premia (ATSM)",
                                               h3("Term Premia using Affine Term Structure Model (ATSM)"),
                                               htmlOutput("ACM"),
                                               fluidRow(
                                                 column(6, highchartOutput("germany_tp_acm")),
                                                 column(6, highchartOutput("italy_tp_acm"))
                                               ),
                                               fluidRow(
                                                 column(6, highchartOutput("france_tp_acm")),
                                                 column(6, highchartOutput("spain_tp_acm"))
                                               )
                                               # htmlOutput("FF"),
                                               # fluidRow(
                                               #   column(6, highchartOutput("germany_tp_ff")),
                                               #   column(6, highchartOutput("italy_tp_ff"))
                                               # ),
                                               # fluidRow(
                                               #   column(6, highchartOutput("france_tp_ff")),
                                               #   column(6, highchartOutput("spain_tp_ff"))
                                               # )
                                               #htmlOutput("ACM_compare")
                                      ),
                                      tabPanel("Comparison",
                                               highchartOutput("acm_cf_mp"),
                                               fluidRow(
                                                 column(6, highchartOutput("germany_acm_cf_tp")),
                                                 column(6, highchartOutput("italy_acm_cf_tp"))
                                               ),
                                               fluidRow(
                                                 column(6, highchartOutput("france_acm_cf_tp")),
                                                 column(6, highchartOutput("spain_acm_cf_tp"))
                                               )
                                      ),
                                      tabPanel("Current Projections",
                                               h3("Expected Path of short-term rates"),
                                               highchartOutput("rateplot"),
                                               htmlOutput("proj_exp"),
                                               fluidRow(
                                                 column(12, align="center", tableOutput('table'))
                                               ),
                                               htmlOutput("prompt"),
                                               h3("Your Forecasts for Short-Term Rates"),
                                               fixedRow(
                                                 column(4, numericInput("F1", label = "1 year ahead", value = round(fut_rate_consensus[4], 1))),
                                                 column(4, numericInput("F2", label = "2 years ahead", value = round(fut_rate_consensus[8], 1))),
                                                 column(4, numericInput("F3", label = "3 years ahead", value = round(fut_rate_consensus[12], 1)))
                                               ),
                                               actionButton("go", "Submit"),
                                               highchartOutput("rateplot_user"),
                                               h3("Yield Decompostions"),
                                               fluidRow(
                                                 column(12, align="center", tableOutput('table_user'))
                                               )
                                        )
                                )
                          ),
                 tabPanel("United States",
                          tabsetPanel(tabPanel("Methodology",
                                               htmlOutput("method_US")),
                                      tabPanel("Term Premia (FOMC)",
                                               h4("FOMC-based "),
                                               htmlOutput("US_FOMC_dscrp"),
                                               highchartOutput("US_FOMC")),
                                      tabPanel("Term Premia (ATSM)",
                                               h4("ACM Model"),
                                               highchartOutput("US_ACM"),
                                               h4("Modified ATSM Model"),
                                               highchartOutput("US_FF")
                                               ),
                                      tabPanel("Comparison",
                                               h4("Term Premia Comparison"),
                                               highchartOutput("US_TP_Compare"),
                                               h4("Monetary Policy Component Comparison"),
                                               highchartOutput("US_MP_Compare")
                                               ),
                                      tabPanel("Current Projections",
                                               h3("Expected Path of short-term rates"),
                                               highchartOutput("rateplot_US"),
                                               htmlOutput("proj_exp_US"),
                                               fluidRow(
                                                 column(12, align="center", tableOutput('table_US'))
                                               ),
                                               htmlOutput("prompt_US"),
                                               h3("Your Forecasts for Short-Term Rates"),
                                               fixedRow(
                                                 column(4, numericInput("U1", label = "1 year ahead", value = round(lr_US$V2, 1))),
                                                 column(4, numericInput("U2", label = "2 years ahead", value = round(lr_US$V3, 1))),
                                                 column(4, numericInput("U3", label = "3 years ahead", value = round(lr_US$V4, 1)))
                                               ),
                                               fixedRow(
                                                 column(6, numericInput("UL", label = "Long-term (in 10 years)", value = round(lr_US$V10, 1)))
                                               ),
                                               actionButton("go_US", "Submit"),
                                               highchartOutput("rateplot_user_US"),
                                               h3("Yield Decompostions"),
                                               fluidRow(
                                                 column(12, align="center", tableOutput('table_user_US'))
                                               )
                                        )
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
    
    output$germany_tp_acm <- renderHighchart({
      Germany_Term_Premia_ACM
    })
    
    output$france_tp_acm <- renderHighchart({
      France_Term_Premia_ACM
    })
    
    output$italy_tp_acm <- renderHighchart({
      Italy_Term_Premia_ACM
    })
    
    output$spain_tp_acm <- renderHighchart({
      Spain_Term_Premia_ACM
    })
    
    output$germany_tp_ff <- renderHighchart({
      Germany_Term_Premia_FF
    })
    
    output$france_tp_ff <- renderHighchart({
      France_Term_Premia_FF
    })
    
    output$italy_tp_ff <- renderHighchart({
      Italy_Term_Premia_FF
    })
    
    output$spain_tp_ff <- renderHighchart({
      Spain_Term_Premia_FF
    })
    
    output$germany_acm_cf_tp <- 
      renderHighchart({
        Germany_ACM_CF_TP
      })
    
    output$france_acm_cf_tp <- 
      renderHighchart({
        France_ACM_CF_TP
      })
    
    output$spain_acm_cf_tp <- 
      renderHighchart({
        Spain_ACM_CF_TP
      })
    
    output$italy_acm_cf_tp <- 
      renderHighchart({
        Italy_ACM_CF_TP
      })
    
    output$acm_cf_mp <- 
      renderHighchart({
        ACM_CF_MP
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
    
    output$FF <- renderUI({
      HTML(
        "<b>Favero & Fernandez-Fuertes (2023)</b> propose a new ATS model in which yields drift, 
        sharing a common stochastic trend driven by the drift in short-term (monetary policy) rates
        and excess returns are stationary as the compensation for risk depends on the cycle in yields
        (FF) The results are shown below -"
      )
    })
    
    output$ACM_compare <- renderUI({
      HTML(
        "<i>Note: In the ACM plot, the <b>monetary policy (MP) component</b> is computed in 2 steps - (1) The expected 
        short rates are extracted from the model, and (2) the MP component is calculated using 
        the methodology used for the other forecasts. The <b>risk neutral yield</b>, on the 
        other hand, is generated by setting the market price for risk to zero in the model. 
        Both the curves follow each other closely. There are two instances of sharp deviations 
        due to spikes in the MP component (generated by spikes in the 1M rates obtained from 
        NSS interpolation). Thus, we can use the risk neutral yield as an approximation for the 
        monetary policy component in the term premia calculations and MP comparison plot.</i>"
      )
    })
    
    output$proj_exp <- renderUI({
      HTML(
        "The figure above plots the expected path of future short (monetary policy) 
        rates using the consensus forecasts, ACM model and FF model. The table below 
        lists the 10Y yield decompositions into the monetary policy component 
        (MP) and the term premia (TP) computed using the 3 methods."
      )
    })
    
    output$prompt <- renderUI({
      HTML(
        "You can provide your own forecasts for the short-term (3 month) rates, and click
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
    
    output$method_US <- renderUI({
      HTML(
        "<br>We use three methods to measure the term premia in the United States. <br>
        <br>
        1. A popular affine term structure model outlined in Adrian Crump Moench (2013),
        also called the <b>ACM model</b>, is used to decompose the yields into two components 
        - risk neutral yield and the term premia. The model-implied forecasts for short 
        term rates can also be generated (see <a href = 'https://papers.ssrn.com/sol3/papers.cfm?abstract_id=4622555'>
        Favero and Fernandez-Fuertes (2023)</a>). <br>
        <br>
        2. A modified version of the ACM model - we call it the <b>FF model</b> - is used similarly 
        (see <a href = 'https://papers.ssrn.com/sol3/papers.cfm?abstract_id=4622555'> 
        Favero and Fernandez-Fuertes (2023)</a>). <br>
        <br>
        3. The third measure is based on the <b>FOMC projections</b> for the federal funds rates. These are 
        used to decompose the yield into two components - the expected monetary policy component and 
        the term premia. For the derivation, see Euro Area -> Methodology -> Section 2.1 of this website. <br>"
      )
    })
    
    output$US_ACM <- renderHighchart({
      US_ACM_Decomp
    })
    
    output$US_FF <- renderHighchart({
      US_FF_Decomp
    })
    
    output$US_FOMC_dscrp <- renderUI({
      HTML(
        "The U.S. Federal Open Market Committee (FOMC) releases the Summary of Economic 
        Projections, based on the assessment of meeting participants, in March, June, 
        September and December of every year (see the 'Projection Materials' in <a href = 'https://www.federalreserve.gov/monetarypolicy/fomccalendars.htm'> 
        FOMC Meeting Reports </a>. Among other forecasts, it includes the 
        forecasts for the federal funds rate for the next three years and also the projected
        long run forecast of the federal funds rate. Assuming the long run forecast to hold 
        after 10 years, we can construct the expected monetary policy component of a 10-year 
        bond and measure the term premia as the residual. The plot below uses the FOMC 
        forecasts for the decomposition -"
      )
    })
    
    output$US_FOMC <- renderHighchart({
      US_FOMC_Decomp
    })
    
    output$US_TP_Compare <- renderHighchart({
      US_TP_ACM_FF_FOMC
    })
    
    output$US_MP_Compare <- renderHighchart({
      US_MP_ACM_FF_FOMC
    })
    
    output$proj_exp_US <- renderUI({
      HTML(
        "The figure above plots the expected path of future federal funds rates 
        rates using the ACM model, FF model, FOMC forecasts. The 
        table below lists the 10Y yield decompositions into the monetary policy component 
        (MP) and the term premia (TP) computed using the 3 methods."
      )
    })
    
    output$prompt_US <- renderUI({
      HTML(
        "You can provide your own forecasts for the short-term (3 month) rates, and click
        submit to get the current term premia estimates based on your forecasts for US. 
        The default rates in the input boxes are the FOMC forecasts for the respective period. 
        The current rate is 5.6%"
      )
    })
    
    output$rateplot_US <- renderHighchart({
      Projections_US
    })
    
    output$table_US <- renderTable({
      main_table_US
    }, bordered = TRUE, align = "c")
    
    
    
    #DEFINE REACTIVE FUNCTION FOR INPUTS-------------------------------------------------------
    user_forecasts <- eventReactive(input$go, {
      user_forecasts <- c(input$F1,input$F2,input$F3)
    })
    
    user_forecasts_US <- eventReactive(input$go_US, {
      user_forecasts_US <- c(input$U1,input$U2,input$U3,input$UL)
    })
    
    
    #PLOTS AND TABLES WITH USER PROVIDED FORECASTS----------------------------------------------
    output$rateplot_user <- renderHighchart({
      T = length(plot_dates.ACM)
      #T.FF = length(plot_dates.FF)
      highchart() %>%
        hc_xAxis(type = "datetime", dateTimeLabelFormats = list(month = "%b %Y"), gridLineWidth = 1) %>%
        hc_yAxis(title = list(text = "Interest Rate"), min = -1, max = 5) %>%
        hc_add_series(data = recent_data, hcaes(x = Date, y = Rate),
                      type = "line", name = "Observed 3M rates", color = "black", lineWidth = 2) %>%
        #hc_add_series(data = data.frame(x = fut_date, y = fut_rate_mod), hcaes(x = x, y = y),
        #              type = "line", name = "Naive Model estimate", color = "orange", lineWidth = 2, dashStyle = "Dash") %>%
        hc_add_series(data = data.frame(x = fut_date, y = fut_rate_consensus), hcaes(x = x, y = y),
                      type = "line", name = "Consensus forecast", color = "red", lineWidth = 2, dashStyle = "Dash") %>%
        hc_add_series(data = data.frame(x = plot_dates.ACM[(T-4):T], y = fittedYields.ACM[(T-4):T,1]*100), hcaes(x = x, y = y),
                      type = "line", name = "ACM Model Fitted", color = "brown", lineWidth = 2) %>%
        #hc_add_series(data = data.frame(x = plot_dates.FF[(T.FF-4):T.FF], y = fittedYields.FF[(T.FF-4):T.FF,1]*100), hcaes(x = x, y = y),
        #              type = "line", name = "FF Model Fitted", color = "blue", lineWidth = 2) %>%
        hc_add_series(data = data.frame(x = plot_dates_proj_3, y = append(ESTR.ACM[T,1:12],fittedYields.ACM[T,1], after = 0)*100), hcaes(x = x, y = y),
                      type = "line", name = "ACM Forecast", color = "brown", lineWidth = 2, dashStyle = "Dash") %>%
        #hc_add_series(data = data.frame(x = plot_dates_proj.FF, y = append(ESTR.FF[T.FF,1:12]+y_trend.matrix[T.FF,1],fittedYields.FF[T.FF,1], after = 0)*100), hcaes(x = x, y = y),
        #              type = "line", name = "FF Forecast", color = "blue", lineWidth = 2, dashStyle = "Dash") %>%
        hc_add_series(data = data.frame(x = fut_date[c(1,5,9,13)], y = append(lr$Rate, user_forecasts())), hcaes(x = x, y = y),
                      type = "line", name = "User forecast", color = "green", lineWidth = 2, dashStyle = "Dash") %>%
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
    
    output$rateplot_user_US <- renderHighchart({
      forecasts <- c(lr_US$V1,user_forecasts_US()[1:3],rep(NA,5),user_forecasts_US()[4])
      forecasts <- na.approx(forecasts)
      s1 <- US_3M
      names(s1) <- c("x","y")
      s1$x <- as.Date(s1$x)
      s1$y <- round(s1$y,2)
      #ACM-fitted recent
      s2 <- as.data.frame(cbind(tail(index(na.omit(l_ACM$n1$n1_IN)),12),as.numeric(tail(na.omit(l_ACM$n1$n1_IN),12))*100))
      names(s2) <- c("x","y")
      s2$x <- as.Date(s2$x)
      s2$y <- round(s2$y,2)
      #FF-fitted recent
      s3 <- as.data.frame(cbind(tail(index(na.omit(l_FF$n1$n1_IN)),12),as.numeric(tail(na.omit(l_FF$n1$n1_IN),12))*100))
      names(s3) <- c("x","y")
      s3$x <- as.Date(s3$x)
      s3$y <- round(s3$y,2)
      #ACM-predicted
      s4 <- as.data.frame(cbind(index(na.omit(l_ACM$n1$n1_OoS)),as.numeric(na.omit(l_ACM$n1$n1_OoS))*100))
      names(s4) <- c("x","y")
      s4$x <- as.Date(s4$x)
      s4$y <- round(s4$y,2)
      #FF-predicted
      s5 <- as.data.frame(cbind(index(na.omit(l_FF$n1$n1_OoS)),as.numeric(na.omit(l_FF$n1$n1_OoS))*100))
      names(s5) <- c("x","y")
      s5$x <- as.Date(s5$x)
      s5$y <- round(s5$y,2)
      #FOMC forecasts
      s6 <- as.data.frame(cbind(FOMC_dates,FOMC_proj))
      names(s6) <- c("x","y")
      s6$x <- as.Date(s6$x)
      s6$y <- round(s6$y,2)
      s7 <- as.data.frame(cbind(FOMC_dates,forecasts))
      names(s7) <- c("x","y")
      s7$x <- as.Date(s7$x)
      s7$y <- round(s7$y,2)
      highchart() %>%
        hc_add_series(s1, "line", hcaes(x, y), name = "Actual Recent", color = "black") %>%
        hc_add_series(s2, "line", hcaes(x, y), name = "ACM fitted", color = "orange") %>%
        hc_add_series(s3, "line", hcaes(x, y), name = "FF fitted", color = "blue") %>%
        hc_add_series(s4, "line", hcaes(x, y), name = "ACM forecast", dashStyle = "dash", color = "orange") %>%
        hc_add_series(s5, "line", hcaes(x, y), name = "FF forecast", dashStyle = "dash", color = "blue") %>%
        hc_add_series(s6, "line", hcaes(x, y), name = "FOMC forecast", dashStyle = "dash", color = "green") %>%
        hc_add_series(s7, "line", hcaes(x, y), name = "User forecast", dashStyle = "dash", color = "brown") %>%
        hc_title(text = "Projected Rates") %>%
        hc_xAxis(type = "datetime", title = list(text = "Date")) %>%
        hc_yAxis(title = list(text = "Term Premia"), min = 0, max = 6) %>%
        hc_legend(enabled = TRUE) %>%
        hc_boost(enabled = TRUE) %>%
        hc_exporting(enabled = TRUE)
    })
    
    output$table_user_US <- renderTable({
      add_user_for_US(main_table_US,user_forecasts_US())
    }, bordered = TRUE, align = "c")
}


# Run the application 
shinyApp(ui = ui, server = server)