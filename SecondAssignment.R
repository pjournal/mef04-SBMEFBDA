
library(tidyverse)
library(readr)
library(dplyr)
library(ggplot2)
library(lubridate)

#Data Preparation
filepath <- file.choose()
read_df <- read_csv(filepath)

#read_df <- read_xls("/Users/Serhan/Desktop/ptf-smf.xls", n_max = 721)

read_df %>% glimpse()

# - Renaming columns
# - Substituting commas with dots 
# - Changing data type of numerical values
# - Changing data type of date

# Found that useful function to change the names of the weekdays:
# https://dplyr.tidyverse.org/reference/recode.html

elec_df <-read_df %>% 
  transmute(Date = dmy_hm(Date), MCP, SMP, 
            PosImbP = `Positive Imbalance Price (TL/MWh)`, 
            NegImbP = `Negative Imbalance Price (TL/MWh)`, 
            SMP_Dir = str_replace(`SMP Direction`, ".?", "")) %>%
  mutate(Hours = hour(Date), WeekDay = wday(Date)) %>%
  mutate(WeekDay = recode(WeekDay, 
                   `2`="1. Monday",`3`="2. Tuesday",`4`="3. Wednesday",`5`="4. Thursday",
                   `6`="5. Friday",`7`="6. Saturday",`1`="7. Sunday"))

elec_df %>% glimpse() 

# Since we made the data more readable, we can now start to analyze it.

# Energy Deficit has the highest occurrence proportion with 76%. 
# This means that most of the time the consumers predict less electricity need then the actual need.

smp_dir_all <- elec_df %>% 
  count(SMP_Dir, name = "Count") %>%
  mutate(Percentage = round(Count / sum(Count)* 100, 0)) %>%
  select(-Count)
  
ggplot(smp_dir_all, aes(x = "", y = Percentage, fill = SMP_Dir)) +
  geom_bar(stat="identity",width = 1) +
  coord_polar("y") +
  geom_text(aes(label = paste0(Percentage, "%")), color = "white", position = position_stack(vjust = 0.5)) +
  theme_void()

elec_df %>% select(MCP, SMP) %>% summary()

elec_df %>%
  pivot_longer(cols = c(MCP,SMP), names_to = "Price_Type") %>%
  ggplot(aes(x=Price_Type, y=value, fill=Price_Type)) +
  geom_boxplot(outlier.shape = NA) +
  coord_cartesian(ylim = c(150, 500)) + 
  labs(x = "Price Type", y = "Price Value")

# Above boxplots show that median of SMP is higher, this means SMP values has higher values.
# And this supports occurance of Energy Deficit has the most percentage.
  
# reference : https://www.r-graph-gallery.com/piechart-ggplot2.html    

# Let's take a closer look at the SMP_Dir distribution over days and hours.
# Below bar charts tells us that, 
# Since electricity is mostly used between those hours, energy deficit mostly appears between 10 AM and 11 PM.
# For the same reason above, Energy Surplus mostly occurs during the night time. 
# Also, the Balance case occurs mostly between 1 AM and 6 AM.

hourly_smp_dir <- elec_df %>%
  group_by(SMP_Dir, Hours) %>%
  summarise(Count = n())
  
ggplot(hourly_smp_dir, aes(x = Hours, y = Count)) +
  geom_bar(stat = "identity", aes(fill = as.factor(SMP_Dir))) +
  facet_wrap(~ SMP_Dir) +
  ggtitle("SMP Direction Per Hour") + 
  theme(legend.position = "none", plot.title = element_text(hjust = 0.5)) 

# Ref: http://zevross.com/blog/2019/04/02/easy-multi-panel-plots-in-r-using-facet_wrap-and-facet_grid-from-ggplot2/













# Let's look at daily distribution of SMP direction
# From the plots, we can see that Energy Surplus mostly occurs on Saturdays.

daily_smp_dir <- elec_df %>%
  group_by(SMP_Dir, WeekDay) %>%
  summarise(Count = n())

ggplot(daily_smp_dir, aes(x = WeekDay, y = Count)) +
  geom_bar(stat = "identity", aes(fill = as.factor(SMP_Dir))) +
  facet_wrap(~ SMP_Dir) +
  ggtitle("SMP Direction in Days") + 
  theme(legend.position = "none", plot.title = element_text(hjust = 0.5), axis.text.x = element_text(angle = 60,vjust=1,hjust=1)) 





hourly_prices <- elec_df %>%
  group_by(Hours) %>%
  summarise(AverageMCP = mean(MCP), AverageSMP = mean(SMP))

ggplot(hourly_prices, aes(x = Hours)) +
  geom_line(aes(y = AverageMCP, color = "Average MCP")) +
  geom_line(aes(y = AverageSMP, color = "Average SMP")) + 
  ggtitle("MCP & SMP per Hour") + 
  labs(y = "Price Values", color = "Legend") + 
  theme(plot.title = element_text(hjust = 0.5))
  


daily_prices <- elec_df %>%
  group_by(WeekDay) %>%
  summarise(AverageMCP = mean(MCP), AverageSMP = mean(SMP))

ggplot(daily_prices, aes(x = as.factor(WeekDay), group = 1)) +
  geom_line(aes(y = AverageMCP, color = "Average MCP")) +
  geom_line(aes(y = AverageSMP, color = "Average SMP")) + 
  ggtitle("MCP & SMP Over Days") + 
  labs(x = "Days", y = "Price Values", color = "Legend") + 
  theme(plot.title = element_text(hjust = 0.5))







