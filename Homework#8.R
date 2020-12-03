df=read.csv("match.csv")
library(dplyr)
df_2017=df%>%
  filter(Tournament=='IPL 2017')
df_2017_clear<-na_if(df_2017,'-')
