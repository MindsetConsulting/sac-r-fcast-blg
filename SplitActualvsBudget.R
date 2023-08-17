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
Sales_Design_Page_1_Act_Q <- Sales_Design_Page_1_SubsetAct %>% group_by(TimeQuarter) %>% summarise(SummarizedA = sum(Value))
Sales_Design_Page_1_Bud_Q <- Sales_Design_Page_1_SubsetBud %>% group_by(TimeQuarter) %>% summarise(SummarizedB = sum(Value))
print(Sales_Design_Page_1_TSDataAct)
print(Sales_Design_Page_1_TSDataBud)
print(Sales_Design_Page_1_Act_Q)
print(Sales_Design_Page_1_Bud_Q)
Sales_Design_Page_1_T <- merge(Sales_Design_Page_1_Act_Q, Sales_Design_Page_1_Bud_Q, by.y = "TimeQuarter", all.y = TRUE)
#Sales_Design_Page_1_TSDataAct_Format <- merge(Sales_Design_Page_1_TSDataAct, Sales_Design_Page_1_TSDataBud, by.x = "TimeMonth")
print(Sales_Design_Page_1_T)

require(plotly)

plot_ly(data=Sales_Design_Page_1_T, x = ~TimeQuarter, y = ~SummarizedB, type = 'bar', name = 'Budget', marker = list(color = 'violet')) %>%
    add_trace(y = ~SummarizedA, name = 'Actuals', marker = list(color = 'purple')) %>%
layout(yaxis = list(title = 'Gross Sales - Quarterly', zerolinecolor = toRGB("red"), tickfont=list(color='magenta')),barmode = 'stack') %>% 
layout(xaxis=list(tickfont=list(color='magenta'))) %>% 
layout(plot_bgcolor="rgba(255, 255, 255, 0)") %>%
layout(paper_bgcolor="rgba(255, 255, 255, 0)") %>%
layout(title="Actuals vs Budget Performance", font=list(color='white')) %>%
layout(fig_bgcolor="rgb(255, 255, 0)")

# Convert TimeMonth to date-time format
library(zoo)

Sales_Design_Page_1_TSDataAct$TimeMonth <- as.Date(as.yearmon(c(as.character(Sales_Design_Page_1_TSDataAct$TimeMonth)), "%Y%m"))
Sales_Design_Page_1_TSDataBud$TimeMonth <- as.Date(as.yearmon(c(as.character(Sales_Design_Page_1_TSDataBud$TimeMonth)), "%Y%m"))
print(Sales_Design_Page_1_TSDataAct)
head(Sales_Design_Page_1_TSDataBud)
