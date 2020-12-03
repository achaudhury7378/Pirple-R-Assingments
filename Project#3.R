df<-read.csv('City_AllHomes.csv')
library(dplyr)
#calculating the basic statistics
means<-c(mean(df$X2014),mean(df$X2015))
maxs<-c(max(df$X2014),max(df$X2015))
mins<-c(min(df$X2014),min(df$X2015))
Frst_quartile<-c(quantile(df$X2014,0.25),quantile(df$X2015,0.25))
Last_quartile<-c(quantile(df$X2014,0.75),quantile(df$X2015,0.75))
medians<-c(median(df$X2014),median(df$X2015))
stat_data<-data.frame(means,maxs,mins,medians,Frst_quartile,Last_quartile)
row.names(stat_data)<-c('2014','2015')
#Visualizing the data in a boxplot to check for ouliers
boxplot(df$X2014)
#from the boxplot of the data we can see that values above 1e+06 are outliers
boxplot(df$X2015)
#from the boxplot of the data we can see that values above 1e+06 are outliers
#we need to eleminate those values
df1<-df%>%
  filter(df$X2014<1000000 & df$X2015<1000000)
boxplot(df1$X2014)
boxplot(df1$X2015)
#we can see that nowour data is good 
#Now we need to check the correlation between those to columns
cor(df1$X2014,df1$X2015,method=c("spearman"))
#we can see that the two factors are high linear correlation which is 
#also visible in the following scatterplot
library(ggplot2)
ggplot(df1, aes(x = X2014, y = X2015)) + geom_point()
#Since we can see that there is a high linear correlation and the scatterplot 
#clearly supports the fact that there is a linear relationship
#so it is good to fit a line between these to values to predict 2015 from 2014
#prices

#for testing purposes we split the data set into two parts train and test
tr <- sample(1:nrow(df1), 0.8 * nrow(df1))
train <- df1[tr, ]
test <- df1[-tr,]

# Build the model on training data
lmMod <- lm(X2015 ~ X2014, data = train)

# predict
distPred <- predict(lmMod, test)

#made a dataframe based on the actual and predicted prices
actual_preds <- data.frame(actuals = test$X2015, predicted = distPred)
#score on how well the data fitted
accu<-summary(lmMod)$r.squared
