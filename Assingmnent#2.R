ds1<-read.csv("results.csv")
library(xlsx)

ds2<-read.xlsx("population.xlsx",1,header = T,startRow = 1,colIndex = 1:32)
ds3<-read.csv("originalDataset.txt",row.names = 1,sep=",")
library(dplyr)
secEnd<-function(x){
  substr(x,nchar(x)-3,nchar(x))
}
India_Match=ds3 %>%
  mutate(Year=secEnd(as.vector(Date)))%>%
  filter(Winner=="India")
Win_years=as.data.frame(table(India_Match$Year)) 
library(ggplot2)
ggplot(Win_years,aes(x=Var1,y=Freq))+
  geom_point() +
  theme(axis.text.x=element_text(angle=90))

