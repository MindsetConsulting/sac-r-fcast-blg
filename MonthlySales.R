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
Sales_Design_Page_1_TSDataAct <- Sales_Design_Page_1_SubsetAct %>% group_by(TimeMonth) %>% summarise(Summarized = sum(Value))
Sales_Design_Page_1_TSDataBud <- Sales_Design_Page_1_SubsetBud %>% group_by(TimeMonth) %>% summarise(Summarized = sum(Value))
print(Sales_Design_Page_1_TSDataAct)
print(Sales_Design_Page_1_TSDataBud)



# Convert TimeMonth to date-time format
library(zoo)

Sales_Design_Page_1_TSDataAct$TimeMonth <- as.Date(as.yearmon(c(as.character(Sales_Design_Page_1_TSDataAct$TimeMonth)), "%Y%m"))
Sales_Design_Page_1_TSDataBud$TimeMonth <- as.Date(as.yearmon(c(as.character(Sales_Design_Page_1_TSDataBud$TimeMonth)), "%Y%m"))
print(Sales_Design_Page_1_TSDataAct)
head(Sales_Design_Page_1_TSDataBud)

#plot monthly sales data against month-year time period
options(repr.plot.width = 6, repr.plot.height = 3)
ggplot(Sales_Design_Page_1_TSDataAct, aes(x = TimeMonth, y = Summarized)) + geom_line(color="purple") + geom_point(shape=21) +geom_smooth(method = 'lm', color="blue", fill="blue") +labs(x = "Time", y = "Monthly Sales") +
theme_classic() +
theme (
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill = "transparent",colour = "purple"),
    plot.background = element_rect(fill = "transparent",colour = NA),
      axis.text.x = element_text(color = "white", size = 12), 
        axis.text.y = element_text(color = "white", size = 12),
  		axis.title = element_text(color = "white")      
  	)