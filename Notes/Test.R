#--------------------------------------------

install.packages("Lahman")
#instal the package

#---------------------------------------------

library(Lahman)
#init the package

#---------------------------------------------

colnames(Batting)
#look at column names of package

#---------------------------------------------

Batting %>%
  select(playerID, yearID, teamID, HR) %>%
  filter(teamID == "NYA" & yearID == 1927)
#take from batting the playerID, yearID, HR and then filter so the table only contains NY Yankees from the year 1927

#---------------------------------------------
#9/6/2017

Batting %>%
  select(playerID, yearID, teamID, HR) %>%
  filter(HR > 39 & teamID == "NYA")

#---------------------------------------------

Batting %>%
  select(playerID, yearID, teamID, HR, SO) %>%
  filter(HR > 40 & SO < 60)

#---------------------------------------------

Batting %>%
  select(playerID, yearID, teamID, HR) %>%
  filter(HR > 30 & teamID == "PHI" & yearID >= 1970 & yearID < 1980)

#---------------------------------------------

#new topic arrange()

#---------------------------------------------

Batting %>%
  select(playerID, yearID, teamID, HR) %>%
  filter(HR > 50) %>%
  arrange(desc(HR))

#---------------------------------------------

Batting %>%
  select(playerID, yearID, teamID, SO, AB) %>%
  filter(SO < 10, AB > 400) %>%
  arrange(SO)

#-----------------------------------------------
#9/8/17

Batting %>%
  select(playerID, HR) %>%
  filter(playerID == "ruthba01") 

#-------------------

Batting %>%
  filter(playerID == "ruthba01") %>%
  group_by(playerID) %>%
  summarise(career_HR = sum(HR)) 

#----------------

Batting %>%
  group_by(playerID) %>%
  summarise(career_HR = sum(HR)) %>%
  filter(career_HR >=  600) %>%
  arrange(desc(career_HR))

#---------------

Batting %>%
  group_by(playerID) %>%
  summarise(career_avg_HR = mean(HR)) %>%
  filter(career_avg_HR >= 30) %>%
  arrange(desc(career_avg_HR))

#------------------

Batting %>%
  select(yearID, playerID, HR) %>%
  filter(yearID >= 1970)%>%
  group_by(playerID) %>%
  summarise(highestHR = max(HR))%>%
  filter(highestHR > 50)%>%
  select(playerID)
  

