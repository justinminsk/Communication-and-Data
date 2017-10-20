#------------

library(dplyr)
library(ggplots2)
library(stringr)
library(lubridate)
#import libraries

#------------------

MSD <- read.csv("Mass Shootings Dataset.csv")
#import data

colnames(MSD)
#look at column names

df<-MSD%>%
  mutate(date = mdy(Date))%>%
  filter(date >= "2010-01-01")
#create a date using lubridate

ggplot()+
  geom_line(data = df, aes(x = date, y = Fatalities))+
  geom_line(data = df, aes(x = date, y = Injured), color = "red")+
  scale_x_date(date_breaks = "3 months", date_labels = "%b %y")+
  coord_flip()
#show deaths and injuries from mass shootings from 2010