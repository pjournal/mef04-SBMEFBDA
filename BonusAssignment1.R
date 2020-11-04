
library(tidyverse)
library(readxl)
library(dplyr)
library(ggplot2)

#Reading Data

#file_chosen <- file.choose()
#read_df <- read_xlsx(file_chosen, sheet = "EVDS", n_max = 130)

read_df <- read_xlsx("/Users/Serhan/Desktop/EVDS_istanbul_property_data.xlsx", sheet = "EVDS", n_max = 130)

read_df %>% glimpse()



#Renaming columns
my_df <- read_df %>% select(Date = 1, Total = 2, Mortgage = 3, FirstHand = 4, 
                            SecondHand = 5, Foreign = 6, NewHousePriceIndex = 7, 
                            HousePriceIndex = 8, UnitPrice = 9)

glimpse(my_df)



#Analyze Points

plot_1_df <- my_df %>% select(Date, UnitPrice) %>% arrange(Date)

ggplot(plot_1_df, aes(x = Date, y = UnitPrice, group = 1, color = UnitPrice)) + 
  geom_line() + 
  ylab("Unit Prices - TL/m2") +
  ggtitle("Unit Prices of Properties in Istanbul","Jan 2010 - September 2020") +
  theme(axis.text.x = element_blank(), axis.ticks = element_blank(), plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5))




plot_2_df <- my_df %>% 
  filter(is.na(SecondHand) == FALSE) %>%
  transmute(Date, SecondHandRatio = SecondHand / Total) %>% arrange(Date)

ggplot(plot_2_df, aes(x = Date, y = SecondHandRatio, group = 1, color = SecondHandRatio)) + 
  geom_line() + 
  ylab("Second Hand Ratio - Percentage") +
  ggtitle("Second Hand Property Sale Ratio in Istanbul","Jan 2013 - September 2020") +
  theme(axis.text.x = element_blank(), axis.ticks = element_blank(), plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5))