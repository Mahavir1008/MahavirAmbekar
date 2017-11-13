#Naive bayes: BDSP Assingment 
#---------------------------
cars=read.csv("C:/Users/Mahavir/Downloads/cars.csv")

#Data prepration and cleaning

View(cars)
dim(cars)
sum(is.na(cars$cubicinches))

colSums(is.na(cars))
type(cars$cubicinches)
class(cars$cubicinches)
(is.na(cars$cubicinches)==TRUE)

sss=order(cars[(is.na(cars$cubicinches)==TRUE),])
sss
xc=cars[(is.na(cars$cubicinches)==TRUE),]
xc
cars[c(41,181),"cubicinches"]=mean(cars$cubicinches,na.rm = TRUE)

cars[cars$cubicinches==8,]
ind
order(... = )
cars[c(15,34,173)]
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
#Training and test data prepration 
index=sample(x=nrow(cars),size = 181)
index
train_data=cars[index,]
nrow(test_data)
View(test_data)
test_data=cars[-index,]
Test_Brand=test_data$brand
View(cars)
names(cars)
colSums(is.na(test_data==TRUE))
test_data=test_data[-8,]
nrow(test_data)
library(e1071)
model_nb_new=naiveBayes(brand~.,data=(train_data),laplace=1)

pred_nb=predict(model_nb_new,newdata = test_data[,-8])

pred_nb
library(caret)
library()
confusionMatrix(pred_nb,test_data$brand)
plot(table(pred_nb,test_data$brand))
