library(dplyr)
library(reshape2)
df<-read.csv('results.csv')
df<-df[1:100,]
ds <- melt(df,id.vars = c("date","city","country","away_team"),measure.vars = c("home_score","away_score"))
ds<- ds%>% arrange(date)
