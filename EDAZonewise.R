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
Sales_Design_Page_1_TSDataAct <- Sales_Design_Page_1_SubsetAct %>% group_by(TimeMonth) %>% summarise(Summarized_Actual = mean(Value))
Sales_Design_Page_1_TSDataBud <- Sales_Design_Page_1_SubsetBud %>% group_by(TimeMonth) %>% summarise(Summarized_Budget = mean(Value))
head(Sales_Design_Page_1_TSDataAct)
head(Sales_Design_Page_1_TSDataBud)

# Convert TimeMonth to date-time format
library(zoo)
Sales_Design_Page_1_TSData$TimeMonth <- as.Date(as.yearmon(c(as.character(Sales_Design_Page_1_TSData$TimeMonth)), "%Y%m"))
print(Sales_Design_Page_1_TSData)


Sales_Design_Page_1_TSData2 <- Sales_Design_Page_1_Unique2 %>% group_by(Sales_ZoneDescription) %>% summarise(Summarized_Value = mean(Value))
str(Sales_Design_Page_1_TSData2)

library(ggplot2)
ggplot(data = Sales_Design_Page_1_SubsetAct, mapping = aes(x = Sales_ZoneDescription, y = Value)) +
   geom_boxplot(alpha = 0, colour = "#3366FF", notch=TRUE) +

theme_classic() +
theme (
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill = "transparent",colour = "purple"),
    plot.background = element_rect(fill = "transparent",colour = NA),
      axis.text.x = element_text(color = "black", size = 11), 
        axis.text.y = element_text(color = "black", size = 11),
  		axis.title = element_text(color = "white")      
  	) + 
  labs(
    x = "Sales Zones",
    y = "Sales Price",
    title = paste(
      "Sales on Actuals, Zone wise"
    )
  ) +

geom_jitter(aes(color = Sales_ZoneDescription))

