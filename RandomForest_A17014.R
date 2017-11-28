# Random Forest Algorithm
#_________________________________________________________________

#1 Data reading 
cars=read.csv("C:/Users/Mahavir/Downloads/cars.csv")
#C:\Users\Mahavir\Desktop\Desktop data\l
View(cars)
dim(cars)
sum(is.na(cars$cubicinches))
#2 Data cleaning and data prepration 
colSums(is.na(cars))
cars[c(41,181),"cubicinches"]=mean(cars$cubicinches,na.rm = TRUE)
mn=mean(cars$weightlbs,na.rm = TRUE)
mn
for(i in 1:nrow(cars))
{
  if(is.na(cars$weightlbs[i])==TRUE)
  {
    print(cars$weightlbs[i])
    cars$weightlbs[i]=mn
    print(cars$weightlbs[i])
    mn
    print("hi")
  }
}


# 3 Training and test data prepration and cross validation on data 
index=sample(x=nrow(cars),size = 181)
index
train_data=cars[index,]
nrow(test_data)
View(test_data)
test_data=cars[-index,]
Test_Brand=test_data$brand
View(cars)
colSums(is.na(test_data==TRUE))
#test_data=test_data[-8,]

View(train_data)
View(test_data)
library(caret)
cnt=trainControl(method="cv",number = 10)
sample()

# 4 Need of RandomForest Algorithm 
library(randomForest)
#tunegrid <- expand.grid(.mtry=mtry)
mdl=train(x=(train_data[,-c(8)]),y=train_data$brand,trControl = cnt,method="rpart")
mdl$bestTune
mdl$finalModel

pred=predict(mdl,newdata=(test_data[,-8]))

confusionMatrix(pred,test_data$brand)


# 5 RandomForest Algorithm 
library(randomForest)
#tunegrid <- expand.grid(.mtry=mtry)
mdl=train(x=(train_data[,-c(8)]),y=train_data$brand,trControl = cnt,method="rf")
mdl$bestTune
mdl$finalModel

pred=predict(mdl,newdata=(test_data[,-8]))

confusionMatrix(pred,test_data$brand)
