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


source("Historical.R")
#Growth rate
Germany_GDP <- gdp_dat %>% filter(Country == "DE") %>% select(Date,GDP)
Germany_GDP$Growth_q <- (Germany_GDP$GDP/dplyr::lag(Germany_GDP$GDP,1) - 1)*100
Germany_GDP$Growth_a <- (Germany_GDP$GDP/dplyr::lag(Germany_GDP$GDP,4) - 1)*100
Germany_comp <- master_df %>%
  select(Date,Spread_Germany,TP_cf_Germany,sum_Er_cf,Rate) %>%
  mutate(Monetary_Policy = sum_Er_cf + Rate) %>%
  select(Date,Spread_Germany,TP_cf_Germany,Monetary_Policy)
colnames(Germany_comp) <- c("Date","Spread","Term_Premia","Monetary_Policy")
Germany_table <- merge(Germany_GDP,Germany_comp, by = "Date")
Germany_table <- merge(Germany_table,Germany_inf, by = "Date")

France_GDP <- gdp_dat %>% filter(Country == "FR") %>% select(Date, GDP)
France_GDP$Growth_q <- (France_GDP$GDP/dplyr::lag(France_GDP$GDP, 1) - 1) * 100
France_GDP$Growth_a <- (France_GDP$GDP/dplyr::lag(France_GDP$GDP, 4) - 1) * 100
France_comp <- master_df %>%
  select(Date, Spread_France, TP_cf_France, sum_Er_cf, Rate) %>%
  mutate(Monetary_Policy = sum_Er_cf + Rate) %>%
  select(Date, Spread_France, TP_cf_France, Monetary_Policy)
colnames(France_comp) <- c("Date", "Spread", "Term_Premia", "Monetary_Policy")
France_table <- merge(France_GDP, France_comp, by = "Date")
France_table <- merge(France_table, France_inf, by = "Date")


Italy_GDP <- gdp_dat %>% filter(Country == "IT") %>% select(Date, GDP)
Italy_GDP$Growth_q <- (Italy_GDP$GDP / dplyr::lag(Italy_GDP$GDP, 1) - 1) * 100
Italy_GDP$Growth_a <- (Italy_GDP$GDP / dplyr::lag(Italy_GDP$GDP, 4) - 1) * 100
Italy_comp <- master_df %>% select(Date, Spread_Italy, TP_cf_Italy, sum_Er_cf,Rate) %>%
  mutate(Monetary_Policy = sum_Er_cf + Rate) %>%
  select(Date,Spread_Italy,TP_cf_Italy,Monetary_Policy)
colnames(Italy_comp) <- c("Date", "Spread", "Term_Premia", "Monetary_Policy")
Italy_table <- merge(Italy_GDP, Italy_comp, by = "Date")
Italy_table <- merge(Italy_table,Italy_inf, by = "Date")


Spain_GDP <- gdp_dat %>% filter(Country == "ES") %>% select(Date, GDP)
Spain_GDP$Growth_q <- (Spain_GDP$GDP/dplyr::lag(Spain_GDP$GDP, 1) - 1) * 100
Spain_GDP$Growth_a <- (Spain_GDP$GDP/dplyr::lag(Spain_GDP$GDP, 4) - 1) * 100
Spain_comp <- master_df %>%
  select(Date, Spread_Spain, TP_cf_Spain, sum_Er_cf, Rate) %>%
  mutate(Monetary_Policy = sum_Er_cf + Rate) %>%
  select(Date, Spread_Spain, TP_cf_Spain, Monetary_Policy)
colnames(Spain_comp) <- c("Date", "Spread", "Term_Premia", "Monetary_Policy")
Spain_table <- merge(Spain_GDP, Spain_comp, by = "Date")
Spain_table <- merge(Spain_table, Spain_inf, by = "Date")

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

#Growth regressions
fe_gr_q <- plm(Growth_q ~ Spread, data = macro_table, model = "within", effect = "individual")
fe_gr_q_sep <- plm(Growth_q ~ Monetary_Policy + Term_Premia, data = macro_table, model = "within", effect = "individual")

stargazer(fe_gr_q, fe_gr_q_sep, type = "html", out = "growth_qq.doc")

fe_gr_q_1 <- plm(Growth_q ~ lag(Spread, 1), data = macro_table, model = "within", effect = "individual")
fe_gr_q_sep_1 <- plm(Growth_q ~ lag(Monetary_Policy, 1) + lag(Term_Premia, 1), data = macro_table, model = "within", effect = "individual")
fe_gr_q_2 <- plm(Growth_q ~ lag(Spread, 2), data = macro_table, model = "within", effect = "individual")
fe_gr_q_sep_2 <- plm(Growth_q ~ lag(Monetary_Policy, 2) + lag(Term_Premia, 2), data = macro_table, model = "within", effect = "individual")
fe_gr_q_3 <- plm(Growth_q ~ lag(Spread, 3), data = macro_table, model = "within", effect = "individual")
fe_gr_q_sep_3 <- plm(Growth_q ~ lag(Monetary_Policy, 3) + lag(Term_Premia, 3), data = macro_table, model = "within", effect = "individual")
fe_gr_q_4 <- plm(Growth_q ~ lag(Spread, 4), data = macro_table, model = "within", effect = "individual")
fe_gr_q_sep_4 <- plm(Growth_q ~ lag(Monetary_Policy, 4) + lag(Term_Premia, 4), data = macro_table, model = "within", effect = "individual")
fe_gr_q_all <- plm(Growth_q ~ lag(Spread, 1) + lag(Spread, 2) + lag(Spread, 3) + lag(Spread, 4), data = macro_table, model = "within", effect = "individual")
fe_gr_q_sep_all <- plm(Growth_q ~ lag(Monetary_Policy, 1) + lag(Term_Premia, 1) + lag(Monetary_Policy, 2) + lag(Term_Premia, 2) + lag(Monetary_Policy, 3) + lag(Term_Premia, 3) + lag(Monetary_Policy, 4) + lag(Term_Premia, 4), data = macro_table, model = "within", effect = "individual")

stargazer(fe_gr_q_1, fe_gr_q_sep_1, fe_gr_q_2, fe_gr_q_sep_2, type = "html", out = "growth_qq_lag_1_2.doc")
stargazer(fe_gr_q_3, fe_gr_q_sep_3, fe_gr_q_4, fe_gr_q_sep_4, type = "html", out = "growth_qq_lag_3_4.doc")
stargazer(fe_gr_q_all, fe_gr_q_sep_all, type = "html", out = "growth_qq_lag_all.doc")

fe_gr_a <- plm(Growth_a ~ Spread, data = macro_table, model = "within", effect = "individual")
fe_gr_a_sep <- plm(Growth_a ~ Monetary_Policy + Term_Premia, data = macro_table, model = "within", effect = "individual")

stargazer(fe_gr_a, fe_gr_a_sep, type = "html", out = "growth_a.doc")

fe_gr_a_1 <- plm(Growth_a ~ lag(Spread, 1), data = macro_table, model = "within", effect = "individual")
fe_gr_a_sep_1 <- plm(Growth_a ~ lag(Monetary_Policy, 1) + lag(Term_Premia, 1), data = macro_table, model = "within", effect = "individual")
fe_gr_a_2 <- plm(Growth_a ~ lag(Spread, 2), data = macro_table, model = "within", effect = "individual")
fe_gr_a_sep_2 <- plm(Growth_a ~ lag(Monetary_Policy, 2) + lag(Term_Premia, 2), data = macro_table, model = "within", effect = "individual")
fe_gr_a_3 <- plm(Growth_a ~ lag(Spread, 3), data = macro_table, model = "within", effect = "individual")
fe_gr_a_sep_3 <- plm(Growth_a ~ lag(Monetary_Policy, 3) + lag(Term_Premia, 3), data = macro_table, model = "within", effect = "individual")
fe_gr_a_4 <- plm(Growth_a ~ lag(Spread, 4), data = macro_table, model = "within", effect = "individual")
fe_gr_a_sep_4 <- plm(Growth_a ~ lag(Monetary_Policy, 4) + lag(Term_Premia, 4), data = macro_table, model = "within", effect = "individual")
fe_gr_a_all <- plm(Growth_a ~ lag(Spread, 1) + lag(Spread, 2) + lag(Spread, 3) + lag(Spread, 4), data = macro_table, model = "within", effect = "individual")
fe_gr_a_sep_all <- plm(Growth_a ~ lag(Monetary_Policy, 1) + lag(Term_Premia, 1) + lag(Monetary_Policy, 2) + lag(Term_Premia, 2) + lag(Monetary_Policy, 3) + lag(Term_Premia, 3) + lag(Monetary_Policy, 4) + lag(Term_Premia, 4), data = macro_table, model = "within", effect = "individual")

stargazer(fe_gr_a_1, fe_gr_a_sep_1, fe_gr_a_2, fe_gr_a_sep_2, type = "html", out = "growth_a_lag_1_2.doc")
stargazer(fe_gr_a_3, fe_gr_a_sep_3, fe_gr_a_4, fe_gr_a_sep_4, type = "html", out = "growth_a_lag_3_4.doc")
stargazer(fe_gr_a_all, fe_gr_a_sep_all, type = "html", out = "growth_a_lag_all.doc")


#Inflation
fe_inf <- plm(Inflation ~ Spread, data = macro_table, model = "within", effect = "individual")
fe_inf_sep <- plm(Inflation ~ Monetary_Policy + Term_Premia, data = macro_table, model = "within", effect = "individual")

stargazer(fe_inf, fe_inf_sep, type = "html", out = "inflation.doc")

fe_inf_1 <- plm(Inflation ~ lag(Spread, 1), data = macro_table,
              model = "within", effect = "individual")
fe_inf_sep_1 <- plm(Inflation ~ lag(Monetary_Policy,1) + lag(Term_Premia,1), data = macro_table,
                  model = "within", effect = "individual")
fe_inf_2 <- plm(Inflation ~ lag(Spread, 2), data = macro_table,
                model = "within", effect = "individual")
fe_inf_sep_2 <- plm(Inflation ~ lag(Monetary_Policy,2) + lag(Term_Premia,2), data = macro_table,
                    model = "within", effect = "individual")
fe_inf_3 <- plm(Inflation ~ lag(Spread, 3), data = macro_table,
                model = "within", effect = "individual")
fe_inf_sep_3 <- plm(Inflation ~ lag(Monetary_Policy,3) + lag(Term_Premia,3), data = macro_table,
                    model = "within", effect = "individual")
fe_inf_4 <- plm(Inflation ~ lag(Spread, 4), data = macro_table,
                model = "within", effect = "individual")
fe_inf_sep_4 <- plm(Inflation ~ lag(Monetary_Policy,4) + lag(Term_Premia,4), data = macro_table,
                    model = "within", effect = "individual")
fe_inf_all <- plm(Inflation ~ lag(Spread, 1) + lag(Spread, 2) + lag(Spread, 3) + lag(Spread, 4),
              data = macro_table, model = "within", effect = "individual")
fe_inf_sep_all <- plm(Inflation ~ lag(Monetary_Policy,1)+lag(Term_Premia,1) + lag(Monetary_Policy,2)+lag(Term_Premia,2) + lag(Monetary_Policy,3)+lag(Term_Premia,3) + lag(Monetary_Policy,4)+lag(Term_Premia,4),
                  data = macro_table, model = "within", effect = "individual")

stargazer(fe_inf_1, fe_inf_sep_1, fe_inf_2, fe_inf_sep_2, type = "html", out = "inflation_lag_1_2.doc")
stargazer(fe_inf_3, fe_inf_sep_3, fe_inf_4, fe_inf_sep_4, type = "html", out = "inflation_lag_3_4.doc")
stargazer(fe_inf_all, fe_inf_sep_all, type = "html", out = "inflation_lag_all.doc")






#---------  EXTRA.  ----------------------------------

#for (n in 1:6){
#  GDP_mod <- lm(Growth_a ~ lag(Spread,n), data = Italy_table)
#  GDP_mod_sep <- lm(Growth_a ~ lag(Monetary_Policy,n) + lag(Term_Premia,n), data = Italy_table)
#  print(summary(GDP_mod))
#  print(summary(GDP_mod_sep))
#}

#inf_mod <- lm(Inflation ~ lag(Spread, n), data = France_table)
#inf_mod_sep <- lm(Inflation ~ lag(Monetary_Policy, n) + lag(Term_Premia, n), data = France_table)
#print(summary(inf_mod))
#print(summary(inf_mod_sep))
#print(summary(inf_mod)$r.squared)
#print(summary(inf_mod_sep)$r.squared)
