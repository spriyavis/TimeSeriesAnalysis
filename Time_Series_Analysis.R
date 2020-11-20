# Importing relevant libraries
library(corrplot)
library(readxl)
library(ggplot2)
library(forecast)
library(tseries)

# Read data from file
data <- read_excel("Online_Retail.xlsx")
data <- as.data.frame(data)

# Top few and bottom few rows of dataset
head(data)
tail(data)

# Getting dimensions of dataset
dim(data)

# Number of rows in original dataset = 541909
# Number of columns in original dataset = 8

# Datatype/structure of each column in dataset
str(data)

# Checking for number of missing values in each column
colSums(is.na(data))

# There are missing values in CustomerID and Description but I am not deleting the corresponding rows
# as they are not going to impact the time series analysis.

# Getting the total number of duplicate rows in the dataset
sum(duplicated(data))

# Total number of duplicate rows is 5268.

# Removing duplicate rows
data <- data[!duplicated(data),]

# Dimensions of dataframe
dim(data)

# Number of rows after removing duplicate rows is 536641.

# SOME STATISTICS BEFORE REMOVING CANCELLED INVOICES 

# Number of unique items (Descriptions)
paste("Number of unique items: ",length(unique(data$Description)))
# There are 4212 unique items.

# Number of unique stock codes
paste("Number of stock codes: ",length(unique(data$StockCode)))
# There are 4070 unique stock codes.

# Number of unique invoices 
paste("Number of unique invoices: ",length(unique(data$InvoiceNo)))
# There are 25900 unique invoices.

# Number of unique customers based on available data (some are not available as inferred above)
paste("Number of unique customers: ",length(unique(data$CustomerID)))
# There are 4373 unique customers.

# Removing cancelled invoices as they do not contribute to the sales
data$InvoiceNo <- gsub("C[0-9][0-9][0-9][0-9][0-9][0-9]","C",data$InvoiceNo,ignore.case = FALSE)
data <- data[!(data$InvoiceNo =='C'),]

# Dimensions of dataframe
dim(data)

# Number of rows after removing cancelled invoices is 527390.

# Removing negative values of quantity and unit price
data <- data[!(data$Quantity <= 0),]
data <- data[!(data$UnitPrice <= 0),]

# Dimensions of dataframe
dim(data)

# Number of rows after removing negative values of quantity and unit price is 524878.

# Summary of each column of dataset - predictor and target variables
# Showing summary at this stage as duplicates will affect the values
summary(data)

# Create new column called TotalPrice
data$TotalPrice <- data$Quantity * data$UnitPrice

# Total Sales per Country - Top 10
sales_per_country <- aggregate(data$TotalPrice,by = list(data$Country),FUN = sum)
names(sales_per_country)[names(sales_per_country) == "Group.1"] <- "Country"
names(sales_per_country)[names(sales_per_country) == "x"] <- "Total_Sales"

sales_per_country_sorted <- sales_per_country[order(-sales_per_country$Total_Sales),]

ggplot(data=head(sales_per_country_sorted,10), aes(x= reorder(Country,-Total_Sales), y=Total_Sales)) +
  geom_bar(stat="identity") + xlab("Country") + ylab("Total Sales($)") + # Set axis labels
  ggtitle("Total Sales per Country - Top 10")      

# Total sales is highest in United Kingdom followed by Netherlands.

# Number of unique items (Descriptions)
paste("Number of unique items: ",length(unique(data$Description)))
# There are 4015 unique items.

# Number of unique stock codes
paste("Number of stock codes: ",length(unique(data$StockCode)))
# There are 3922 unique stock codes.

# Number of unique invoices 
paste("Number of unique invoices: ",length(unique(data$InvoiceNo)))
# There are 19960 unique invoices.

# Number of unique customers based on available data (some are not available as inferred above)
paste("Number of unique customers: ",length(unique(data$CustomerID)))
# There are 4339 unique customers.

# Converting timestamp to date
data$InvoiceDate <- as.Date(data$InvoiceDate)

# Aggregating sales over date (Total sales per date)
daily_data <- aggregate(data[,c(9)],by = list(data$InvoiceDate),FUN = sum)
names(daily_data)[names(daily_data) == "Group.1"] <- "Date"
names(daily_data)[names(daily_data) == "x"] <- "TotalPrice"

# Plotting the time series data
ggplot(daily_data, aes(Date, TotalPrice)) + geom_line() + scale_x_date('month')  + ylab("Sales($)") +
  xlab("Date")

# Cleaning the time series data: Removing outliers using tsclean()
TotalPrice_ts = ts(daily_data[, c('TotalPrice')])

daily_data$clean_TotalPrice = tsclean(TotalPrice_ts)

ggplot() +
  geom_line(data = daily_data, aes(x = Date, y = clean_TotalPrice)) + ylab('Cleaned Sales($)')

# Smoothing the graph
daily_data$TotalPrice_ma7 = ma(daily_data$clean_TotalPrice, order=7) # using the clean total price with no outliers
daily_data$TotalPrice_ma30 = ma(daily_data$clean_TotalPrice, order=30)

ggplot() +
  geom_line(data = daily_data, aes(x = Date, y = clean_TotalPrice, colour = "Sales")) +
  geom_line(data = daily_data, aes(x = Date, y = TotalPrice_ma7,   colour = "Weekly Moving Average"))  +
  geom_line(data = daily_data, aes(x = Date, y = TotalPrice_ma30, colour = "Monthly Moving Average"))  +
  ylab('Sales')

# Decompose Data - Going with weekly

sales_ma = ts(na.omit(daily_data$TotalPrice_ma7), frequency=30)
decomp = stl(sales_ma, s.window="periodic")
deseasonal_sales <- seasadj(decomp)
plot(decomp)

# Stationary Test

adf.test(sales_ma, alternative = "stationary")

# Since p-value (0.01) is less than 0.05, we can reject the null hypothesis in favor of the alternative hypothesis
# and conclude that the time series is stationary

# AUTOCORRELATIONS AND CHOOSING MODEL ORDER

# Autocorrelation function plot
Acf(sales_ma, main='')

# Even though we concluded that the alternative 

# Partial autocorrelation function plot
Pacf(sales_ma, main='')

# Differencing the time series by 1 and plotting the graph 

count_d1 = diff(deseasonal_cnt, differences = 1)
plot(count_d1)

adf.test(count_d1, alternative = "stationary")

sales_d1 = diff(deseasonal_sales, differences = 1)
plot(sales_d1)

# Dicky-Fuller Test again
adf.test(sales_d1, alternative = "stationary")

# Autocorrelation Function Plot and Partial Autocorrelation Function Plot again
Acf(sales_d1, main='ACF for Differenced Series')
Pacf(sales_d1, main='PACF for Differenced Series')

# FITTING THE MODEL
  
# AUTO ARIMA - WIHOUT AND WITH SEASONALITY

# Case 1: Not taking seasonality into consideration

fit1_1 <- auto.arima(deseasonal_sales, seasonal=FALSE)
fit1_1

# According to auto-arima, best model is ARIMA(5,1,3) with drift when seasonality is not considered.

# Examining ACF and PACF plots for model residuals
# If model order parameters and structure are correctly specified, we would expect no significant autocorrelations present.

tsdisplay(residuals(fit1_1), lag.max=45, main='(5,1,3) Model Residuals - Without Seasonality')

# Since there are significant autocorrelations present in this model as per the above graphs, we will disregard this model

# Case 2: Taking seasonality into consideration

fit1_2 <- auto.arima(deseasonal_sales, seasonal=TRUE)
fit1_2

# Examining ACF and PACF plots for model residuals
# If model order parameters and structure are correctly specified, we would expect no significant autocorrelations present.

tsdisplay(residuals(fit1_2), lag.max=45, main='ARIMA(2,1,2)(1,0,0)[30] Model Residuals - With Seasonality')

# Since there are significant autocorrelations present in this model as per the above graphs, we will disregard this model.

# ARIMA

# Running a loop to get the best values for p and q such that the AIC is minimum. Also printing the AIC values for all p,d,q.


spfinal.aic <- Inf
spfinal.order <- c(0,0,0)
for (i in 6:7) for (j in 6:7) {
  spcurrent.aic <- AIC(arima(deseasonal_sales, order=c(i, 1, j)))
  print(paste("p: ",i,", d: 1, q: ",j," - AIC: ", spcurrent.aic))
  if (spcurrent.aic < spfinal.aic) {
    spfinal.aic <- spcurrent.aic
    spfinal.order <- c(i, 1, j)
    spfinal.arma <- arima(deseasonal_sales, order=spfinal.order)
  }
}

# Checking for the best model
spfinal.order

# Hence, by looking at the AIC values above, as well the automated output, 
# we can conclude that p = 7, d = 1 and q = 7 is the best model.

# Fitting ARIMA model with p = 7, d = 1 and q = 7

fit2 <- arima(deseasonal_sales, order=c(7,1,7))
fit2

# As can be seen, AIC value is 5336.03. The coefficients of the ARIMA model are printed above.

# Plot graph
tsdisplay(residuals(fit2), lag.max=15, main='Seasonal Model Residuals')

# There are NO significant autocorrelations present in this model as per the above graphs. 
#Hence, ARIMA(7,1,7) is our final model.

# FORECASTING
# ARIMA(7,1,7)

fcast <- forecast(fit2, h=30)
plot(fcast)
pred <- predict(fit2, n.ahead = 30)
pred


