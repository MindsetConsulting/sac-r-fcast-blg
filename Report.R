require(tidyverse)
require(lubridate)
require(stringr)
require(forecast)
require(readr)
require(dplyr)
require(ggplot2)
require(flextable)

# Reading the data

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

salesLog <- salesTS
salesLogS <- salesTSS
salesLogHW <- HoltWinters(salesLog)
salesLogHWS <- HoltWinters(salesLogS)
salesLogHW

options(repr.plot.width = 6, repr.plot.height = 4)
#plot(salesLogHW)

# forecast next year's sales
nextYearSales <- forecast(salesLogHW, h=12)
nextYearSalesS <- forecast(salesLogHWS, h=24)
nextYearSales
nextYearSalesS
class(nextYearSalesS)
head(nextYearSalesS)
df_holt = as.data.frame(nextYearSalesS)
df_holt
df <- cbind(MonthYear = rownames(df_holt), df_holt)
rownames(df_holt) <- 1:nrow(df_holt)
df

# plot tabular report denoting point forecast for next years
library(officer)


myft <- flextable(
  df)
  #col_keys = c("am", "carb", "gear", "mpg", "drat" ))

myft <- color(myft, color = "#bbaeff")
myft <-  bg(myft, bg = "#bbaeff", part = "header")
# dark gray as background color for body
myft <-  bg(myft, bg = "#333333", part = "body")
myft <- fit_to_width(myft, max_width = 5.5)

myft
