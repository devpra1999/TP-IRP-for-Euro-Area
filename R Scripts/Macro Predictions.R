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
