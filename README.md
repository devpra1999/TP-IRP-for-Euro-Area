# Term Premia &amp; Inflation Risk Premia for the Euro Area

## R Scripts
The R scripts have the following function -
1. main.R  
  The main script to be used for running the entire analysis and saving the suitable plots.  

2. Getting Data.R  
  To read the data files for 10 year bond yields, short term rates and consensus forecasts into tables.   
  The codes can be run in any system, given the repo is cloned and set as the working directory.
  
2. Build_Database.R  
  It builds a clean database for analysis and modelling.  
  Adds the required variables which will be used in the models.
  
3. Model for expected rates.R  
  Currently builds a simple predictive model for expected short term rates.  
  Step 1 - Estimate rho for: dy_t = rho*dy_t-1 + e_t.  
  Step 2 - Forecast dy_t+i+1 using the estimated rho.  
  Use these values to calculate the term premia.  
  
4. Using Updated Consensus Forecasts.R  
  Adds the consensus forecasts to the main database. 
  Calculate the term premia using the consensus forecasts - implemented vectorization. 

## Plots
Contains plots for
  1. Term Premia comparison between countries.  
  2. Yield decomposition into short term component and term premia (both from model and consensus forecasts). 

The consensus forecast plots integrate all the information and can be looked to get the complete overview.

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



  
