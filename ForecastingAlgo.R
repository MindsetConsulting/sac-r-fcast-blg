require(tidyverse)
require(lubridate)
require(stringr)
require(forecast)
require(readr)
require(dplyr)
require(ggplot2)

# Reading the data
#Sales_Design_Page_1 <- data.frame(lapply(Sales_Price, as.character), stringsAsFactors=FALSE)
Sales_Design_Page_1 <- Sales_Price
str(Sales_Design_Page_1)
# Subset by CategoryCategory and Sales_ZoneDescription
library(dplyr)
Sales_Design_Page_1_Subset <- subset(Sales_Design_Page_1, Sales_Design_Page_1$CategoryCategory == "Actuals" & Sales_Design_Page_1$Sales_ZoneDescription == "Zone A")

Sales_Design_Page_1_Subset2 <- subset(Sales_Design_Page_1, Sales_Design_Page_1$CategoryCategory == "Actuals")

Sales_Design_Page_1_SubsetAct <- subset(Sales_Design_Page_1, Sales_Design_Page_1$CategoryCategory == "Actuals")

Sales_Design_Page_1_SubsetBud <- subset(Sales_Design_Page_1, Sales_Design_Page_1$CategoryCategory == "Budget")

# Removing duplicate rows, if any
Sales_Design_Page_1_Unique <- distinct(Sales_Design_Page_1_Subset) # Every row is unique here
Sales_Design_Page_1_Unique2 <- distinct(Sales_Design_Page_1_Subset2)

# Grouping and summarizing by TimeMonth
Sales_Design_Page_1_TSData <- Sales_Design_Page_1_Unique %>% group_by(TimeMonth) %>% summarise(Summarized_Value = mean(Value))
Sales_Design_Page_1_TSDataAct <- Sales_Design_Page_1_SubsetAct %>% group_by(TimeMonth) %>% summarise(Summarized = mean(Value))
Sales_Design_Page_1_TSDataActS <- Sales_Design_Page_1_SubsetAct %>% group_by(TimeMonth) %>% summarise(Summarized = sum(Value))
Sales_Design_Page_1_TSDataBud <- Sales_Design_Page_1_SubsetBud %>% group_by(TimeMonth) %>% summarise(Summarized = sum(Value))
print(Sales_Design_Page_1_TSDataAct)
print(Sales_Design_Page_1_TSDataBud)

# Convert TimeMonth to date-time format
library(zoo)

Sales_Design_Page_1_TSDataAct$TimeMonth <- as.Date(as.yearmon(c(as.character(Sales_Design_Page_1_TSDataAct$TimeMonth)), "%Y%m"))
Sales_Design_Page_1_TSDataActS$TimeMonth <- as.Date(as.yearmon(c(as.character(Sales_Design_Page_1_TSDataActS$TimeMonth)), "%Y%m"))
Sales_Design_Page_1_TSDataBud$TimeMonth <- as.Date(as.yearmon(c(as.character(Sales_Design_Page_1_TSDataBud$TimeMonth)), "%Y%m"))
print(Sales_Design_Page_1_TSDataAct)
head(Sales_Design_Page_1_TSDataBud)

# convert our sales data to a time series object
salesTS <- ts(Sales_Design_Page_1_TSDataAct$Summarized, frequency = 12, start = c(2017,1))
salesTSS <- ts(Sales_Design_Page_1_TSDataActS$Summarized, frequency = 12, start = c(2017,1))
class(salesTS)

options(repr.plot.width = 6, repr.plot.height = 5)
salesDecomp <- decompose(salesTS)
salesDecompS <- decompose(salesTSS)
plot(salesDecomp)

#apply holt winters exp smt on converted time series object
salesLog <- salesTS
salesLogS <- salesTSS
salesLogHW <- HoltWinters(salesLog)
salesLogHWS <- HoltWinters(salesLogS)
salesLogHW

options(repr.plot.width = 6, repr.plot.height = 4)
plot(salesLogHW)

# forecast next year's sales
nextYearSales <- forecast(salesLogHW, h=12)
nextYearSalesS <- forecast(salesLogHWS, h=24)
nextYearSales
nextYearSalesS
# plot
plot(nextYearSales, main="Sales Forecast using Holt Winters", frame = FALSE ,col.main="purple", font.main=1, cex.main=1.5,
     xlab = "Month - Year", ylab = "Sales",
     font.lab = 2, col.lab="purple", cex.lab = 1.2, , col = c("magenta", "pink", "violet", "blue"))
axis(1, col = "violet", col.axis = 'purple', cex.axis = 1, lwd = 2)
axis(2, col = "violet", col.axis = "dark violet", lwd = 2)


