df=read.csv('mydata.csv')
library(dplyr)
df_sp_team=df %>%
  filter(Team=='KKR')
df2<-df_sp_team %>%#Statistical measures as asked in question 3 of the homework
  group_by(Player) %>%
  summarise(Maximum=max(Run_scored),Minimum=min(Run_scored),med=median(Run_scored),first_quantile=quantile(Run_scored,0.25),Third_quantile=quantile(Run_scored,0.75))
library(ggplot2)
ggplot(df_sp_team, aes(x = Player, y = Run_scored)) + geom_boxplot()+
  theme(axis.text.x = element_text(face = "bold", color = "black", size = 10, angle=90))#Boxplot on the runs scored by player of my team for each match in a specific season
