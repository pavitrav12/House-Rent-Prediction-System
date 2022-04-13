setwd("C:/Users/PAVITRA/Desktop/Predictive Analytics/Project")
getwd()
## Chapter 2
rent_data=read.csv("Rent.csv")
View(rent_data)
names(rent_data)
### To check whether dataset contains missing values or not 
if(is.na(rent_data)){
  print("Missing values")
} else {
  print("No missing values")
}

## chapter 3
summary(rent_data)
library(ggplot2)
## 1 - geom_point
pvb<-bed4%>%ggplot()+geom_point(aes(x=bedroom,y=parking,color=floor_type))
print(pvb+ggtitle("Bedroom Vs Parking"))+theme(plot.title = element_text(hjust = 0.5))
## 2 - stat_count
fvp<-ggplot(data=bed4)%>%+stat_count(mapping=aes(x=floor_number,fill=propertyage))
print(fvp+ggtitle("Floor Number \n Property Age"))+theme(plot.title = element_text(hjust=0.5))
## 3 - geom_bar
area_rent <- ggplot(bed4,aes(x=bathrooms,y=mnt_amt))+geom_bar(stat="identity")
print(area_rent+ggtitle("Bathrooms Vs Maintenance Charge"))+theme(plot.title=element_text(hjust = 0.5))

boxplot(rent_data$bedroom)
cor(rent_data[,-c(4,5,6,8,9,10,11,13,14,19,26)],rent_data$rent)

### Outliers 
library(dplyr)
bed4<-rent_data%>%filter(bedroom<=15)
bed4 <- rent_data%>%filter(area<=10000)
bed4 <- bed4%>%filter(bedroom<=10)
nrow(bed4)
summary(bed4)
cor(bed4[,-c(4,5,6,8,9,10,11,13,14,19,26)],bed4$rent)
boxplot(bed4$area)

####Split Train and Test data
set.seed(1)
train_data=sample(1:nrow(bed4),nrow(bed4)*0.60)
train_data
train1=bed4[train_data,]
dim(train1)
test_data=bed4[-train_data,]
test_data
dim(test_data)

#### Tree
names(train1)
library(tree)
set.seed(1)
train1<-na.omit(train1)
tree_model <- tree(rent~.,data=train1)
tree_model
summary(tree_model)
plot(tree_model)
text(tree_model,pretty=0)
cv_rent = cv.tree(tree_model)
cv_rent
er_rent<-which.min(cv_rent$dev)
er_rent
plot(cv_rent$size,cv_rent$dev,type='b')
points(cv_rent$size[er_rent],cv_rent$dev[er_rent],col="green",pch=20)

tr_predict=predict(tree_model,newdata=test_data)
tr_predict
plot(tr_predict,test_data$rent)
abline(0,1)
err=mean((tr_predict-test_data$rent)^2)
err
tss=mean((mean(tr_predict) - test_data$rent)^2)
tss
rss=1-(err/tss)
rss
###Random Forest#### mtry= p/3
library(randomForest)
set.seed(1)
bag_model=randomForest(rent~.,bed4,mtry=10,importance=TRUE)
bag_model
importance(bag_model)
predict_model=predict(bag_model,test_data)
err <- mean((predict_model - test_data$rent)^2)
err
tss <- mean((mean(predict_model) - test_data$rent)^2)
tss
rss <- 1-(err/tss)
rss

####Boosting####
library(gbm)
set.seed(1)
View(train_data)
str(train1)
train1[sapply(train1,is.character)]<-lapply(train1[sapply(train1,is.character)],as.factor)
train1=train1[,-c(6)]
str(train1)
names(train1)
Boost_model=gbm(rent~.,distribution = "gaussian",train1)
Boost_model
summary(Boost_model)
yhat.boost=predict(Boost_model,newdata=bed4[-train_data,])
err1 <- mean((test_data$rent - yhat.boost)^2)
err1
tss1 <- mean((mean(yhat.boost) - test_data$rent)^2)
tss1
rss1 <- 1 -err/tss
rss1




