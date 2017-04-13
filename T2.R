install.packages("ISLR")
library(ISLR)
attach(Carseats)
data <- Carseats
help(Carseats)
head(data)
range(data$Sales)
summary(data)
data$StrongSales <- as.factor( ifelse(data$Sales >=10, "Yes", "No") )
data$Sales <- NULL

set.seed(1234)
ind <- sample(2, nrow(data), replace=TRUE, prob=c(0.8, 0.2))
train <- data[ind==1,]
test <- data[ind==2,]

install.packages("rpart") #If you have not downloaded it already
library(rpart)
help(rpart)
model.tree <- rpart(StrongSales ~ . , data=train)
summary(model.tree)

plot(model.tree)
text(model.tree)
plot(model.tree)
text(model.tree, pretty=0)

pred.train.tree <- predict(model.tree, train, type = "class")
mean(pred.train.tree != train$StrongSales)

pred.test.tree <- predict(model.tree, test, type = "class")
mean(pred.test.tree != test$StrongSales)

table(pred.test.tree,test$StrongSales)

install.packages("e1071") #If you have not downloaded it already
library(e1071)
help(naiveBayes)
model.nb <- naiveBayes(StrongSales ~ . , data=train)
model.nb

pred.train.nb <- predict(model.nb, train, type = "class")
mean(pred.train.nb != train$StrongSales)

pred.test.nb <- predict(model.nb, test, type = "class")
mean(pred.test.nb != test$StrongSales)

table(pred.test.nb,test$StrongSales)

install.packages("class") #If you have not downloaded it already
library(class)
help(knn)

set.seed(1234)
pred.test.knn <- knn(train[,1:5], test[,1:5], train[,11], k = 3)
mean(pred.test.knn != test$StrongSales) 

pred.test.knn <- knn(train[,1:5], test[,1:5], train[,11], k = 5)
mean(pred.test.knn != test$StrongSales)

pred.test.knn10 <- knn(train[,1:5], test[,1:5], train[,11], k = 10)
mean(pred.test.knn10 != test$StrongSales)

table(pred.test.knn10,test$StrongSales)
