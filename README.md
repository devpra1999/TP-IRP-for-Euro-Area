# Term Premia &amp; Inflation Risk Premia for the Euro Area

## R Scripts
The R scripts have the following function (and need to be used in this order) -
1. Getting Data.R  
  To read the data files for 10 year bond yields and short term rates into tables.  
  It does NOT read consesnus forecasts datafiles.  
  Will update it to directly read the datafiles using a URL so that the codes can be run in any system. 
  
2. Build_Database.R  
  It builds a clean database for analysis and modelling.  
  It currently has basic plots - they will be moved to a new script for exploratory analysis later.  
  
3. Model for expected rates.R  
  Currently builds a simple predictive model for expected short term rates.  
  Step 1 - Estimate rho for: dy_t = rho*dy_t-1 + e_t.  
  Step 2 - Forecast dy_t+i+1 using the estimated rho.  
  Use these values to calculate the term premia.  
  
4. Using Consensus Forecasts for R  
  Reads the consensus forecast files. 
  Adds the consensus forecasts to the main database. 
  Calculate the term premia using the consensus forecasts - implemented vectorization. 

## Plots
Contains plots for
  1. Term Premia
  2. Yield decomposition into short term component and term premia
  3. Consensus Forecasts

The consensus forecast plots integrate all the information and can be looked to get the complete overview.

## Data Files
Long Term Bond Yield Data has been sourced from -
1. Germany - https://sdw.ecb.europa.eu/quickview.do?SERIES_KEY=229.IRS.M.DE.L.L40.CI.0000.EUR.N.Z
2. Italy - https://sdw.ecb.europa.eu/quickview.do?SERIES_KEY=229.IRS.M.IT.L.L40.CI.0000.EUR.N.Z
3. France - https://sdw.ecb.europa.eu/quickview.do?SERIES_KEY=229.IRS.M.FR.L.L40.CI.0000.EUR.N.Z
4. Spain - https://sdw.ecb.europa.eu/quickview.do?SERIES_KEY=229.IRS.M.ES.L.L40.CI.0000.EUR.N.Z
  
Short term Data for the Euro Area has been sourced from -  
[https://sdw.ecb.europa.eu/intelligentsearch/?searchTerm=professional%20forecaster%20survey%20interest&pageNo=1&itemPerPage=10&sortBy=relevance](https://sdw.ecb.europa.eu/quickview.do?SERIES_KEY=143.FM.D.U2.EUR.4F.KR.MRR_RT.LEV)  

Consensus forecasts are sourced from -  
https://sdw.ecb.europa.eu/intelligentsearch/?searchTerm=professional%20forecaster%20survey%20interest&pageNo=1&itemPerPage=10&sortBy=relevance  



  
