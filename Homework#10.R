df<-read.csv('mydata.csv')
library(dplyr);
df3<-df%>%
  group_by(match_id,Team,Against) %>%
  summarise(Total_Score_in_the_match=sum(Total_run))
Teams<-unique(df3$Team)
m <- matrix(, nrow = 8, ncol = 8)
colnames(m)<-Teams
row.names(m)<-Teams
for (i in 1:8){
  for(j in 1:8){
    m[i,j]=0
  }
}
for (k in 1:59){
  df_match<-df3[df3$match_id==k,]
  Team<-as.vector(df_match$Team)
  Scores<-as.vector(df_match$Total_Score_in_the_match)
  if ((Scores[1]>=Scores[2])==TRUE){
    m[Team[1],Team[2]]=m[Team[1],Team[2]]+1
  }else if((Scores[1]<=Scores[2])==TRUE){
    m[Team[2],Team[1]]=m[Team[2],Team[1]]+1
  }else if((Scores[1]==Scores[2])==TRUE) {
    m[Team[1],Team[2]]=m[Team[1],Team[2]]+0
    m[Team[2],Team[1]]=m[Team[2],Team[1]]+0
  }
}

p <- matrix(, nrow = 8, ncol = 8)
colnames(p)<-Teams
row.names(p)<-Teams
for (i in 1:8){
  for(j in 1:8){
    p[i,j]=0 
  }
}
for (k in 1:59){
  df_match<-df3[df3$match_id==k,]
  Team<-as.vector(df_match$Team)
  p[Team[1],Team[2]]=p[Team[1],Team[2]]+1
  p[Team[2],Team[1]]=p[Team[2],Team[1]]+1
}
# collect data for the model as team , against team,number of match played against the team ,final rank in the league(label to be predicted)
total_wins<-c()
for (i in 1:8){
  total_wins<-append(total_wins,sum(m[i,]))
}
total_wins_sorted<-sort(total_wins,decreasing = TRUE)
ranks<-as.vector(Teams[match(total_wins_sorted,total_wins)])
library(reshape2)
data_set<-melt(p)
Team_s<-as.vector(data_set$Var1)
rank<-c()
rank_char<-c('A','B','C','D','E','F','G','H')
for (i in 1:nrow(data_set)){
  rank<-append(rank,rank_char[match(Team_s[i],ranks)])
}
data_set<-cbind(data_set, rank)
colnames(data_set)<-c('Team_1','Team_2','n_of_played_in_a_season','final_rank')

library(caret)
library(rpart.plot)
library(RColorBrewer)

set.seed(1033)


tree2 <- rpart(final_rank ~Team_1+Team_2+n_of_played_in_a_season, 
               data = data_set, method = "class")

trctrl <- trainControl(method = "repeatedcv", number = 10, repeats = 3)
set.seed(1033)
dtree_fit <- train(final_rank ~., data = data_set, method = "rpart",
                   parms = list(split = "information"),
                   trControl = trctrl,
                   tuneLength = 10)


vector1<-c(0,2,3,4,2,4,3,2,2,0,2,3,4,2,4,3,3,2,0,2,3,4,2,4,4,3,2,0,2,3,4,2,2,4,3,2,0,2,3,4,4,2,4,3,2,0,2,3,3,4,2,4,3,2,0,2,2,3,4,2,4,3,2,0)
count_games<-matrix(vector1,nrow=8,ncol=8)
colnames(count_games)<-Teams
row.names(count_games)<-Teams

predict_data<-melt(count_games)
colnames(predict_data)<-c('Team_1','Team_2','n_of_played_in_a_season')
new_ranks<-c()
for (i in 1:length(Teams)){
  new_ranks<-append(new_ranks,0)
}

  
predict_data_team<-predict_data[predict_data$Team_1=='RCB',]
test_pred <- predict(dtree_fit, newdata = predict_data_team)
new_ranks[match('RCB',Teams)]=test_pred[1]

predict_data_team<-predict_data[predict_data$Team_1=='KKR',]
test_pred <- predict(dtree_fit, newdata = predict_data_team)
new_ranks[match('KKR',Teams)]=test_pred[1]

predict_data_team<-predict_data[predict_data$Team_1=='SRH',]
test_pred <- predict(dtree_fit, newdata = predict_data_team)
new_ranks[match('SRH',Teams)]=test_pred[1]

predict_data_team<-predict_data[predict_data$Team_1=='MI',]
test_pred <- predict(dtree_fit, newdata = predict_data_team)
new_ranks[match('MI',Teams)]=test_pred[1]

predict_data_team<-predict_data[predict_data$Team_1=='RPS',]
test_pred <- predict(dtree_fit, newdata = predict_data_team)
new_ranks[match('RPS',Teams)]=test_pred[1]

predict_data_team<-predict_data[predict_data$Team_1=='GL',]
test_pred <- predict(dtree_fit, newdata = predict_data_team)
new_ranks[match('GL',Teams)]=test_pred[1]

predict_data_team<-predict_data[predict_data$Team_1=='KXIP',]
test_pred <- predict(dtree_fit, newdata = predict_data_team)
new_ranks[match('KXIP',Teams)]=test_pred[1]

predict_data_team<-predict_data[predict_data$Team_1=='DD',]
test_pred <- predict(dtree_fit, newdata = predict_data_team)
new_ranks[match('DD',Teams)]=test_pred[1]

new_ranks<-as.matrix(new_ranks)
row.names(new_ranks)<-Teams
colnames(new_ranks)<-c('ranks')

#The ranking that I predicted in the project#2 is different from this one 
# RCB  SRH  MI   RPS  GL   KKR  KXIP DD
#  7     2   1    2    6     3    4   5 ---> ranks from project#2
#  6     3   1    2    7     4    5   6 ---> ranks predicted in this homework by decision tree
#the difference in ranking may have occured due to the fact which feature the decision model is focusing more on
#that is the determination of root node.More over I think the data does not have much of a variance to capture for 
#that there might be some missclassifications.The training data  size is small it might affect the model also.Where as in the procedural logic I have considered 
#winning probability of each team against the other and hence calculated the expected number of wins in the
#season and then ranked based on the number of wins .Here decision tree directly takes the input as the 
#team played against and the number of matches played against it in the season and from it the decision tree
#predicted the rank .For this I suppose there is a difference in the ranking .More over I think the main reason
#of difference will be how the tree is made and which factor the decision tree focused mostly on. Whereas 
#the procedural logic every feature is equally given importance.

