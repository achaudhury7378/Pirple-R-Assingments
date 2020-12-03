ds1<-read.csv("results.csv")
library(xlsx)
set.seed(42)
ds2<-read.xlsx("population.xlsx",1,header = T,startRow = 1,colIndex = 1:32)
ds3<-read.csv("originalDataset.txt",row.names = 1,sep=",")
library(dplyr)
library(caTools)

# Working on the ds3
secEnd<-function(x){
  substr(x,nchar(x)-3,nchar(x))
}
India_Match=ds3 %>%
  mutate(Year=secEnd(as.vector(Date)))%>%
  filter(Winner=="India")
Win_years=as.data.frame(table(India_Match$Year)) 
Win_years<-Win_years %>% 
  rename(
    year = Var1
  )
library(ggplot2)
ggplot(Win_years,aes(x=year,y=Freq))+
  geom_point() +
  theme(axis.text.x=element_text(angle=90))
split<-sample.split(Win_years$year,SplitRatio = 0.8)
train<- Win_years[split,]
test<-Win_years[!split,]
model<-lm(as.numeric(year)~Freq,data=train)
predict_ds3<-predict()
predict_ds3<-predict(model,test)
predict_ds3


# Working on ds1
Grouping_ds1<-ds1 %>%
  group_by(date) %>%
  summarize(Max_home_Score=max(home_score),max_away_score=max(away_score))
ggplot(Grouping_ds1,aes(x=Max_home_Score,y=max_away_score))+
  geom_point()+
  theme(axis.text.x=element_text(angle=90))
split<-sample.split(Grouping_ds1$Max_home_Score,SplitRatio = 0.8)
train<- Grouping_ds1[split,]
test<-Grouping_ds1[!split,]
model<-lm(as.numeric(Max_home_Score)~max_away_score,data=train)
predict_ds1<-predict(model,test)
predict_ds1

# Working on ds2
ds2_country_codes=ds2 %>%
  mutate(Country_codes_new=substr(Country,1,3))
year_2005<-as.data.frame(cbind(ds2$Country,ds2$X2005,ds2$X2010))
year_2005<-year_2005 %>% 
  rename(
    population_2005 = V2,
    population_2010 = V3
  )
ggplot(year_2005,aes(x=population_2005,y=population_2010))+
  geom_point()+
  theme(axis.text.x=element_text(angle=90))
split<-sample.split(year_2005$population_2005,SplitRatio = 0.8)
train<- year_2005[split,]
test<-year_2005[!split,]
model<-lm(as.numeric(population_2005)~population_2010,data=train)
predict_ds2<-predict(model,test)
predict_ds2

