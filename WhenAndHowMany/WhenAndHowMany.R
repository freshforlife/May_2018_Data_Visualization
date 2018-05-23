library(lubridate)
library(dplyr)
library(ggplot2)
library(ggthemr)
library(scales) 

all_data <- rbind(read.csv("accidents_2005_to_2007.csv",header=TRUE),read.csv("accidents_2009_to_2011.csv",header=TRUE),read.csv("accidents_2012_to_2014.csv",header=TRUE))

data <- select(all_data, Day_of_Week, Time, Number_of_Casualties)
data$Time <- hour(hm(data$Time))
data$Time_Interval <- cut(data$Time, breaks = c(0,3,6,9,12,15,18,21,24), include.lowest = T)
sum(is.na(data))

data %>%
  na.omit() %>%
  group_by(Time_Interval, Day_of_Week) %>% 
  summarise_at(vars(Number_of_Casualties),sum) -> casualties_day_week


casualties_day_week$Time_Interval <- factor(casualties_day_week$Time_Interval, levels = c('(6,9]', '(9,12]', '(12,15]', '(15,18]', '(18,21]', '(21,24]', '[0,3]', '(3,6]'))

ggthemr('flat dark',type="outer",layout="plain", spacing=2)

ggplot(casualties_day_week, aes(Day_of_Week, Number_of_Casualties)) +  
  geom_bar(aes(fill = Time_Interval), position = "dodge", stat="identity")+
  labs(title="Casualties Distribution by Time of Day & Day of the Week 2005-2014*",caption = expression(paste("Source: UK Acccidents Data \n *2008 data is missing")),x = "Day of Week",y = "# of Casualties")+
  theme(axis.title=element_text(size=16,,face="bold"),axis.text = element_text(size = 14),plot.title = element_text(size=22),plot.caption=element_text(hjust=1))+
  scale_y_continuous(labels=comma)+
  xlim('Sunday','Monday','Tuesday','Wednesday','Thursday','Friday','Saturday')+
  scale_fill_discrete(name = "Hours",labels=c("6:01 AM - 9 AM","9:01 AM - Noon","12:01 PM - 3 PM","3:01 PM - 6 PM","6:01 PM - 9 PM","9:01 PM - Midnight","12 AM - 3 AM","3:01 AM - 6 AM"))

ggthemr_reset()



