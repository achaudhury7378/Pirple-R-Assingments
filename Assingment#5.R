library(xlsx)
ds2<-read.xlsx("population.xlsx",1,header=T)
index<-which(ds2$Country=='United States')
pop_data_usa<-ds2[index,2:32]
pop_data_usa<-t(as.matrix(pop_data_usa))
moving_avg=c()
for (i in 1:28){
  j<-i+3
  values<-mean(as.matrix(as.numeric(pop_data_usa[i:j])))
  moving_avg<-append(moving_avg,values)
}
moving_avg<-as.matrix(moving_avg)
colnames(moving_avg)<-c('Moving Averages')
class(pop_data_usa)
class(moving_avg)
