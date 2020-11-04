
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


ggplot(plot_1_df, aes(x = Date, y = UnitPrice, group = 1)) + 
  geom_line() + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))


#Plots



#Conclusion