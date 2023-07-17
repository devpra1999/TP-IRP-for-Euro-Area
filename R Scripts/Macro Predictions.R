library(shiny)
library(lubridate)
library(zoo)
library(ggplot2)
library(dygraphs)
library(plm)
library(plyr)
library(dplyr)
library(tidyverse)
library(highcharter)
library(markdown)
library(stargazer)


source("Historical.R")
#Growth rate

#Germany_GDP <- gdp_dat %>% filter(Country == "DE") %>% select(Date,GDP)
#Germany_GDP$Growth_q <- (Germany_GDP$GDP/dplyr::lag(Germany_GDP$GDP,1) - 1)*100
#Germany_GDP$Growth_a <- (Germany_GDP$GDP/dplyr::lag(Germany_GDP$GDP,4) - 1)*100
Germany_comp <- master_df %>%
  select(Date,Spread_Germany,TP_cf_Germany,sum_Er_cf,Rate) %>%
  mutate(Monetary_Policy = sum_Er_cf + Rate) %>%
  select(Date,Spread_Germany,TP_cf_Germany,Monetary_Policy)
colnames(Germany_comp) <- c("Date","Spread","Term_Premia","Monetary_Policy")
Germany_table <- merge(Germany_ga,Germany_gq, by = "Date")
Germany_table <- merge(Germany_table,Germany_comp, by = "Date")
Germany_table <- merge(Germany_table,Germany_inf, by = "Date")
Germany_table <- merge(Germany_table,Germany_inf_q, by = "Date")
Germany_table <- merge(Germany_table,Global_CP, by = "Date")

#France_GDP <- gdp_dat %>% filter(Country == "FR") %>% select(Date, GDP)
#France_GDP$Growth_q <- (France_GDP$GDP/dplyr::lag(France_GDP$GDP, 1) - 1) * 100
#France_GDP$Growth_a <- (France_GDP$GDP/dplyr::lag(France_GDP$GDP, 4) - 1) * 100
France_comp <- master_df %>%
  select(Date, Spread_France, TP_cf_France, sum_Er_cf, Rate) %>%
  mutate(Monetary_Policy = sum_Er_cf + Rate) %>%
  select(Date, Spread_France, TP_cf_France, Monetary_Policy)
colnames(France_comp) <- c("Date", "Spread", "Term_Premia", "Monetary_Policy")
France_table <- merge(France_ga, France_gq, by = "Date")
France_table <- merge(France_table, France_comp, by = "Date")
France_table <- merge(France_table, France_inf, by = "Date")
France_table <- merge(France_table, France_inf_q, by = "Date")
France_table <- merge(France_table,Global_CP, by = "Date")



#Italy_GDP <- gdp_dat %>% filter(Country == "IT") %>% select(Date, GDP)
#Italy_GDP$Growth_q <- (Italy_GDP$GDP / dplyr::lag(Italy_GDP$GDP, 1) - 1) * 100
#Italy_GDP$Growth_a <- (Italy_GDP$GDP / dplyr::lag(Italy_GDP$GDP, 4) - 1) * 100
Italy_comp <- master_df %>% select(Date, Spread_Italy, TP_cf_Italy, sum_Er_cf,Rate) %>%
  mutate(Monetary_Policy = sum_Er_cf + Rate) %>%
  select(Date,Spread_Italy,TP_cf_Italy,Monetary_Policy)
colnames(Italy_comp) <- c("Date", "Spread", "Term_Premia", "Monetary_Policy")
Italy_table <- merge(Italy_ga, Italy_gq, by = "Date")
Italy_table <- merge(Italy_table, Italy_comp, by = "Date")
Italy_table <- merge(Italy_table,Italy_inf, by = "Date")
Italy_table <- merge(Italy_table,Italy_inf_q, by = "Date")
Italy_table <- merge(Italy_table,Global_CP, by = "Date")

#Spain_GDP <- gdp_dat %>% filter(Country == "ES") %>% select(Date, GDP)
#Spain_GDP$Growth_q <- (Spain_GDP$GDP/dplyr::lag(Spain_GDP$GDP, 1) - 1) * 100
#Spain_GDP$Growth_a <- (Spain_GDP$GDP/dplyr::lag(Spain_GDP$GDP, 4) - 1) * 100
Spain_comp <- master_df %>%
  select(Date, Spread_Spain, TP_cf_Spain, sum_Er_cf, Rate) %>%
  mutate(Monetary_Policy = sum_Er_cf + Rate) %>%
  select(Date, Spread_Spain, TP_cf_Spain, Monetary_Policy)
colnames(Spain_comp) <- c("Date", "Spread", "Term_Premia", "Monetary_Policy")
Spain_table <- merge(Spain_ga, Spain_gq, by = "Date")
Spain_table <- merge(Spain_table, Spain_comp, by = "Date")
Spain_table <- merge(Spain_table, Spain_inf, by = "Date")
Spain_table <- merge(Spain_table, Spain_inf_q, by = "Date")
Spain_table <- merge(Spain_table,Global_CP, by = "Date")

#Adding country names
Germany_table$Country <- "Germany"
Italy_table$Country <- "Italy"
France_table$Country <- "France"
Spain_table$Country <- "Spain"

# Combine the tables into a single dataframe
macro_table <- bind_rows(Germany_table, Italy_table, France_table, Spain_table)
macro_table <- macro_table %>% arrange(Country,Date)
macro_table <- macro_table[, c(ncol(macro_table), 1:(ncol(macro_table)-1))]
#Using term premia and monetary policy to predict macroeconomic variables


#Growth - Quarter-on-Quarter
fe_gr_q <- plm(Growth_q ~ lag(Growth_q,1) + lag(Spread, 1), data = macro_table, model = "within", effect = "individual")
fe_gr_q_sep <- plm(Growth_q ~ lag(Growth_q,1) + lag(Monetary_Policy, 1) + lag(Term_Premia, 1), data = macro_table, model = "within", effect = "individual")

#Growth - Year-on-Year
fe_gr_a <- plm(Growth_a ~ lag(Growth_a,4) + lag(Spread, 4),
               data = macro_table, model = "within", effect = "individual")
fe_gr_a_sep <- plm(Growth_a ~ lag(Growth_a,4) + lag(Monetary_Policy, 8) + lag(Term_Premia, 4),
                   data = macro_table, model = "within", effect = "individual")

#Growth - Results
stargazer(fe_gr_q, fe_gr_q_sep, fe_gr_a, fe_gr_a_sep, type = "html", out = "growth.doc")


#Inflation - Year-on-Year
fe_inf <- plm(Inflation ~ lag(Inflation,4) + lag(Spread, 4) + lag(GCP_YY, 4),
                data = macro_table, model = "within", effect = "individual")
fe_inf_sep <- plm(Inflation ~ lag(Inflation,4) + lag(Monetary_Policy,4) + lag(Term_Premia,4) + lag(GCP_YY,4),
                    data = macro_table, model = "within", effect = "individual")

fe_inf_q <- plm(Inflation_QQ ~ lag(Inflation_QQ,1) + lag(Spread, 1) + lag(GCP_QQ, 1),
              data = macro_table, model = "within", effect = "individual")
fe_inf_q_sep <- plm(Inflation_QQ ~ lag(Inflation_QQ,1) + lag(Monetary_Policy,1) + lag(Term_Premia,1) + lag(GCP_QQ,1),
                  data = macro_table, model = "within", effect = "individual")

stargazer(fe_inf, fe_inf_sep, fe_inf_q, fe_inf_q_sep, type = "html", out = "inflation.doc")

inf_TP_pred <- macro_table
inf_TP_pred$Predicted_Inflation_TP <- predict(fe_inf_sep,newdata=inf_TP_pred)
inf_TP_pred$Predicted_Inflation_TP <- dplyr::lag(inf_TP_pred$Predicted_Inflation_TP,4)
inf_TP_pred <- select(inf_TP_pred,Country,Date,Inflation,Predicted_Inflation_TP) %>% 
  pivot_longer(cols=c("Inflation","Predicted_Inflation_TP"))

inf_Spread_pred <- macro_table
inf_Spread_pred$Predicted_Inflation_Spread <- predict(fe_inf,newdata=inf_Spread_pred)
inf_Spread_pred$Predicted_Inflation_Spread <- dplyr::lag(inf_Spread_pred$Predicted_Inflation_Spread,4)
inf_Spread_pred <- select(inf_Spread_pred,Country,Date,Inflation,Predicted_Inflation_Spread) %>% 
  pivot_longer(cols=c("Inflation","Predicted_Inflation_Spread"))

inf_pred <- merge(inf_Spread_pred, inf_TP_pred, by = c("Country", "Date", "name", "value"), all = TRUE)

ggplot(data=inf_pred) + 
  aes(x=Date,y=value,color=name) + 
  geom_point() + 
  facet_wrap(~Country,nrow=4)

inf_q_TP_pred <- macro_table
inf_q_TP_pred$Predicted_Inflation_QQ_TP <- predict(fe_inf_q_sep,newdata=inf_q_TP_pred)
inf_q_TP_pred$Predicted_Inflation_QQ_TP <- dplyr::lag(inf_q_TP_pred$Predicted_Inflation_QQ_TP,4)
inf_q_TP_pred <- select(inf_q_TP_pred,Country,Date,Inflation_QQ,Predicted_Inflation_QQ_TP) %>% 
  pivot_longer(cols=c("Inflation_QQ","Predicted_Inflation_QQ_TP"))

inf_q_Spread_pred <- macro_table
inf_q_Spread_pred$Predicted_Inflation_QQ_Spread <- predict(fe_inf_q,newdata=inf_q_Spread_pred)
inf_q_Spread_pred$Predicted_Inflation_QQ_Spread <- dplyr::lag(inf_q_Spread_pred$Predicted_Inflation_QQ_Spread,1)
inf_q_Spread_pred <- select(inf_q_Spread_pred,Country,Date,Inflation_QQ,Predicted_Inflation_QQ_Spread) %>% 
  pivot_longer(cols=c("Inflation_QQ","Predicted_Inflation_QQ_Spread"))

inf_q_pred <- merge(inf_q_Spread_pred, inf_q_TP_pred, by = c("Country", "Date", "name", "value"), all = TRUE)

ggplot(data=inf_q_pred) + 
  aes(x=Date,y=value,color=name) + 
  geom_point() + 
  facet_wrap(~Country,nrow=4)



#Term Premia Pedictor
tp_reg <- plm(Term_Premia ~ lag(Monetary_Policy,4),
              data = macro_table, model = "within", effect = "individual")

