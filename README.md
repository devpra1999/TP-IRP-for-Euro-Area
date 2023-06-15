# Term Premia &amp; Inflation Risk Premia for the Euro Area

## R Scripts
The R scripts have the following function -
1. app.R  
  This is an R Shiny script - with the ui and server required for generating the website. It loads the required libraries as well.
  Dependencies - Historical.R, Future.R, Plots.R

2. Historical.R
   This script creates a master database for the yields, spreads and term premia estimates for all the 4 countries - Germany, France, Italy and Spain.  
   The master database also consists of the consensus forecasts for 3 years, and all the requisites for the term premia estimation.
   Finally, a separate database for the rates and their first differences is also generated which is used for an AR(1) model for estimating term premia.  
   Dependencies - Getting_Data.R (commented by default), Loading_Data.R, Build_Database.R, Model for expected Rates.R, Using Updated Consensus Forecasts.R  

4. Current Projections.R  
   This scripts creates a table for the yields and its two components - expected monetary policy component and term premia for all the countries at the current time. The components are calculated using - model rates, consensus forecasts, and user forecasts. In addition it also gives vectors for model and consensus forecasts for expected rates.  

5. Getting_Data.R  
  To download the data files for 10 year bond yields, short term rates and consensus forecasts. Commented by default. It should be uncommented for the first use, and whenever updated data files are required  

6. Loading_Data.R  
   To read the downloaded files' data into tables.
   
7. Build_Database.R  
  It generates a function for building a clean database for each country which can be used for analysis and modelling.  
  Returns a list with two data frames - dataframe for country and a common rate dataframe for our model.  
  
8. Model for expected rates.R  
  Currently builds a simple predictive model for expected short term rates.  
  Step 1 - Estimate rho for: dy_t = rho*dy_t-1 + e_t.  
  Step 2 - Forecast dy_t+i+1 using the estimated rho.  
  Use these values to calculate the term premia.  
  
9. Using Updated Consensus Forecasts.R  
  Adds the consensus forecasts to the main database. 
  Calculate the term premia using the consensus forecasts.  



## Data Files
Long Term Bond Yield Data has been sourced from -
1. Germany - https://sdw.ecb.europa.eu/quickview.do?SERIES_KEY=229.IRS.M.DE.L.L40.CI.0000.EUR.N.Z
2. Italy - https://sdw.ecb.europa.eu/quickview.do?SERIES_KEY=229.IRS.M.IT.L.L40.CI.0000.EUR.N.Z
3. France - https://sdw.ecb.europa.eu/quickview.do?SERIES_KEY=229.IRS.M.FR.L.L40.CI.0000.EUR.N.Z
4. Spain - https://sdw.ecb.europa.eu/quickview.do?SERIES_KEY=229.IRS.M.ES.L.L40.CI.0000.EUR.N.Z
  
Short term Data for the Euro Area has been sourced from -  
https://ec.europa.eu/eurostat/databrowser/view/IRT_ST_M/default/table?lang=en&category=irt.irt_st (Market rates - 3 month)  
https://sdw.ecb.europa.eu/quickview.do?SERIES_KEY=143.FM.D.U2.EUR.4F.KR.MRR_RT.LEV (Official Rates)

Consensus forecasts are sourced from -  
https://sdw.ecb.europa.eu/intelligentsearch/?searchTerm=professional%20forecaster%20survey%20interest&pageNo=1&itemPerPage=10&sortBy=relevance (n months before target period)  



  
