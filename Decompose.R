require(tidyverse)
require(lubridate)
require(stringr)
require(forecast)
require(readr)
require(dplyr)
require(ggplot2)

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
Sales_Design_Page_1_TSDataBud <- Sales_Design_Page_1_SubsetBud %>% group_by(TimeMonth) %>% summarise(Summarized = sum(Value))
print(Sales_Design_Page_1_TSDataAct)
print(Sales_Design_Page_1_TSDataBud)

# Convert TimeMonth to date-time format
library(zoo)
Sales_Design_Page_1_TSDataAct$TimeMonth <- as.Date(as.yearmon(c(as.character(Sales_Design_Page_1_TSDataAct$TimeMonth)), "%Y%m"))
Sales_Design_Page_1_TSDataBud$TimeMonth <- as.Date(as.yearmon(c(as.character(Sales_Design_Page_1_TSDataBud$TimeMonth)), "%Y%m"))
print(Sales_Design_Page_1_TSDataAct)
head(Sales_Design_Page_1_TSDataBud)


# convert our sales data to a time series object
salesTS <- ts(Sales_Design_Page_1_TSDataAct$Summarized, frequency = 12, start = c(2017,1))
class(salesTS)

options(repr.plot.width = 6, repr.plot.height = 5)
salesDecomp <- decompose(salesTS)
salesTS

# Get the time values for the time series
Time = Sales_Design_Page_1_TSDataAct$TimeMonth
print(Time)

# Convert td to data frame
dat = cbind(Time, with(salesDecomp, data.frame(Observed=x, Trend=trend, Seasonal=seasonal, Random=random)))

ggplot(gather(dat, component, value, -Time), aes(Time, value), col = "purple") +
  facet_grid(component ~ ., scales="free_y") +
  geom_line() +

  labs(y="Sales", x="Year") +
  ggtitle(expression(Decomposition~of~Time~series)) +

	theme (
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill = "transparent",colour = "purple"),
    plot.background = element_rect(fill = "transparent",colour = NA),
      axis.text.x = element_text(color = "black", size = 13), 
        axis.text.y = element_text(color = "black", size = 13),
  		axis.title = element_text(color = "white")
      
  	) 