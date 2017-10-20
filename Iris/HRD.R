data<-read.csv("HR_comma_sep.csv")

colnames(data)

stringx<-paste(data$sales, data$salary, sep = " ")

df<-data%>%
  mutate(stringx = paste(data$sales, data$salary, sep = " "))%>%
  group_by(stringx)%>%
  summarise(countx = n())

p<-ggplot(data = df)+
  geom_bar(aes(x = stringx, y = countx), stat = "identity", color = "white", fill = "white")+
  geom_text(aes(label = countx, y = countx, x = stringx), hjust = 0.5, size = 4,
            position = position_dodge(width = 0.5), angle = 40, check_overlap = TRUE)+
  coord_flip()

p 
