
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


plot_1_df <- my_df %>% 
  filter(is.na(FirstHand) == FALSE & is.na(SecondHand) == FALSE) %>%
  select(Date, FirstHand, SecondHand) %>% 
  pivot_longer(.,-Date) 


ggplot(plot_1_df, aes(x=Date, y=value, color=name)) + geom_line()



plot_2_df <- my_df %>% 
  filter(is.na(FirstHand) == FALSE & is.na(SecondHand) == FALSE & is.na(UnitPrice) == FALSE) %>% 
  transmute(Date, FirstHand, SecondHand, MagnifiedUnitPrice = UnitPrice * 2)
  
ggplot(plot_2_df) + 
  geom_line(aes(x=Date, y=FirstHand, group=1, color="FirstHand")) + 
  geom_line(aes(x=Date, y=SecondHand, group=1, color="SecondHand")) + 
  geom_line(aes(x=Date, y=MagnifiedUnitPrice, group=1, color="UnitPrice")) + 
  theme(axis.text.x = element_blank(), axis.ticks = element_blank())
  #theme(axis.text.x = element_text(angle = 60, vjust = 1, hjust=1))


print(plot_1_df)




plot_2_df <- my_df %>% 
  filter(is.na(SecondHand) == FALSE) %>%
  transmute(Date, SecondHandRatio = SecondHand / Total) %>% arrange(Date)

ggplot(plot_2_df, aes(x = Date, y = SecondHandRatio, group = 1, color = SecondHandRatio)) + 
  geom_line() + 
  ylab("Second Hand Ratio - Percentage") +
  ggtitle("Second Hand Property Sales in Istanbul","Jan 2013 - September 2020") +
  theme(axis.text.x = element_blank(), axis.ticks = element_blank(), plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5))




#Plots

plot_3_df <- my_df %>% 
  filter(is.na(Total) == FALSE) %>%
  select(Date, Total) %>% arrange(Date)

ggplot(plot_3_df, aes(x = Date, y = Total, group = 1)) + 
  geom_line() + 
  theme(axis.text.x = element_text(angle = 60, vjust = 1, hjust=1))



#Conclusion