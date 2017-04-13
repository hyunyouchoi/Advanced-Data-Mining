ForestCover2500= read.csv("ForestCover2500.csv", header = TRUE, sep=",")

set.seed(1234)
ind <- sample(2, nrow(ForestCover2500), replace=TRUE, prob=c(0.8, 0.2))
train <- ForestCover2500[ind==1,]
test <- ForestCover2500[ind==2,]

library(rpart)
tree.a <- rpart(Cover ~ . , data=train)
plot(tree.a)
text(tree.a)

library(caret)
pred.a <- predict(tree.a, test, type = "class")
t <- table(pred.a,test$Cover)
confusionMatrix(t)

set.seed(1234)
sample(10)
sample(10, replace=T)

set.seed(1234)
b <- sample(nrow(train), replace=T)

u <- unique(b)

b.train.1 <- train[b,c(1:4,16)]

tree.1 <- rpart(Cover ~ . , data=b.train.1)
pred.1 <- predict(tree.1, test, type = "class")
t <- table(pred.1,test$Cover)
confusionMatrix(t)

b <- sample(nrow(train), replace=T)
b.train.2 <- train[b,c(5:8,16)]
tree.2 <- rpart(Cover ~ . , data=b.train.2)
pred.2 <- predict(tree.2, test, type = "class")

t <- table(pred.2,test$Cover)
confusionMatrix(t)

b <- sample(nrow(train), replace=T)
b.train.3 <- train[b,c(9:15,16)]
tree.3 <- rpart(Cover ~ . , data=b.train.3)
pred.3 <- predict(tree.3, test, type = "class")
t <- table(pred.3,test$Cover)
confusionMatrix(t)

pred.1.p <- predict(tree.1, test)
pred.2.p <- predict(tree.2, test)
pred.3.p <- predict(tree.3, test)

predictions <- (pred.1.p + pred.2.p + pred.3.p)/3

col <- apply(predictions,1,which.max)
labels <- colnames(predictions)
p <- labels[col]
t <- table(p,test$Cover)
confusionMatrix(t)

install.packages("randomForest")
library(randomForest)
help(randomForest)

rf.3 <- randomForest(Cover ~ ., data=train, ntree=3)
pred.rf.3 <- predict(rf.3, test, type = "class")
t <- table(pred.rf.3,test$Cover)
confusionMatrix(t)

rf.50 <- randomForest(Cover ~ ., data=train, ntree=50)
pred.rf.50 <- predict(rf.50, test, type = "class")
t <- table(pred.rf.50,test$Cover)
confusionMatrix(t)

rf.500 <- randomForest(Cover ~ ., data=train, ntree=500)
pred.rf.500 <- predict(rf.500, test, type = "class")
t <- table(pred.rf.500,test$Cover)
confusionMatrix(t)




