# TimeSeriesAnalysis

Time Series Analysis of Online Retail Sales in R

- Data Source: https://www.kaggle.com/roshansharma/online-retail#__sid=js0

Steps:
1. Import relevant libraries
2. Read the data
3. Explore and preprocess data
    - Remove duplicate rows
    - Filter out cancelled invoices
    - Filter out negative values of quantity and unit price
    - Remove outliers using tsclean() 
    - Smooth out the graph using ma()
 4. Predictive modeling: 
    - Perform Dicky-Fuller Test to check if the time series is stationary
    - Plot ACF and PACF to choose model order
    - Perform differencing
    - Determine p, d, q
    - Fit the model: Auto-ARIMA, ARIMA
    - Forecast
