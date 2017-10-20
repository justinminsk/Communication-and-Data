#-----------

install.packages("flexdashboard")
install.packages("ggiraph")
library(Lahman)
library(dplyr)
library(ggplot2)
library(ggiraph)
library(flexdashboard)

#-------------

df<-Batting%>%
  group_by(playerID)%>%
  summarise(career_HR=sum(HR), career_SO=sum(SO))%>%
  filter(career_HR>=400)

#get the playerID's of all players who have a HR total of over 399

HRvSO<-inner_join(df, Master, by=c("playerID"))%>%
  select(nameFirst, nameLast, career_HR, career_SO)

#join with the master table to get first and last names of the players using plaerID

HRvSO$name<-paste(HRvSO$nameFirst, HRvSO$nameLast)

#create a name column that combines first and last name

g<-ggplot(data = HRvSO) +
  geom_point_interactive(mapping = aes(x = career_SO, y = career_HR, tooltip = name)) +
  ggtitle("Career Homeruns vs Strikeouts for Great Hitters") +
  xlab("Career Strikeouts") +
  ylab("Career Homeruns") 

#create a graph that looks at SO by HR for each player

ggiraph(code = print(g), hover_css = "fill:white;stroke:black")

#makes graph

#--------------

df<-Master%>%
  filter(!is.na(weight))%>%
  select(weight)

#get the weights form master of lahman

ggplot() +
  geom_histogram(data = df, aes(x = weight), color = "white", fill = "blue", bins = 25) +
  ggtitle("Baseball Player Weights")

#graph the weights in a histogram

#-------------

df<-Teams%>%
  filter(yearID == 1980)%>%
  select(name, HR)%>%
  arrange(HR)

#sort the teams from lahman by the year 1980 and show the name and HR total

df$name<-factor(df$name, levels = df$name)

#change the type of data to a factor and not a chr, then level it by our dplyr querry 

ggplot() +
  geom_bar(data = df, aes(x = name, y = HR), stat = "identity", color = "blue", fill = "white") +
  coord_flip()

#graph the data using a bar graph

#------------

df<-Teams%>%
  filter(yearID == 1980)%>%
  select(name, HR)%>%
  arrange(HR)

#sort the teams from lahman by the year 1980 and show the name and HR total

df$name<-factor(df$name, levels = df$name)

#change the type of data to a factor and not a chr, then level it by our dplyr querry 

g<-ggplot() +
  geom_bar_interactive(data = df, aes(x = name, y = HR, tooltip = HR, data_id = name), stat = "identity", color = "blue", fill = "white") +
  coord_flip()+
  ggtitle("1980 Team Homeruns")+
  ylab("Homeruns")+
  xlab("Team Name")

#graph the data using a bar graph

ggiraph(code = print(g), hover_css = "fill:red;stroke:black")

#use ggiraph to make it interactive showing the hr on hover 
        
#---------

df<-Batting%>%
  filter(playerID == "ruthba01")%>%
  select(yearID, HR)

#get Babe Ruths Homeruns by year

g<-ggplot()+
  geom_line(data = df, aes(x = yearID, y = HR)) +
  geom_point_interactive(data = df, aes(x = yearID, y = HR, tooltip = HR, data_id = yearID))

#graph the data

ggiraph(code = print(g), hover_css = "fill:white;stroke:red")

#make an interactive graph with ggiraph showing Homeruns

#------------

result<-Batting%>%
  filter(playerID == "ruthba01")%>%
  select(SO, HR, yearID)
#Babe Ruths strikeouts and Homeruns

ggplot()+
  geom_point(data = result, aes(x = SO, y = HR))+
  xlab("Strikeouts")+
  ylab("Homeruns")
#graph the data with a scatter plot

ggplot()+
  geom_histogram(data = result, aes(x = HR), bins = 5, color = "blue", fill = "white")
#histogram of Babe Ruths homeruns

ggplot()+
  geom_point(data = result, aes(x = yearID, y = HR))+
  geom_line(data = result, aes(x = yearID, y = HR))+
  xlab("Year")+
  ylab("Homeruns")
#compare year to homeruns



#----------------

