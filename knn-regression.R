install.packages("gdata")
library(gdata)                 
                  
crash = read.csv("crash.csv", header = TRUE, sep=" ")

set.seed(1234)
ind <- sample(2, nrow(crash), replace=TRUE, prob=c(0.8, 0.2))
train <- crash[ind==1,]
test <- crash[ind==2,]

library(rpart)

model.tree <- rpart(ACCIDENT ~ . , data=train)
pred.model.tree <- predict(model.tree, test, type = "class")
table(pred.model.tree,test$ACCIDENT)

install.packages("e1071")
library(e1071)
install.packages("caret")
library(caret)
t <- table(pred.model.tree,test$ACCIDENT)
confusionMatrix(t)

crash$ACCIDENT <- factor( crash$ACCIDENT, levels=c("YES","NO") )
set.seed(1234)
ind <- sample(2, nrow(crash), replace=TRUE, prob=c(0.8, 0.2))
train <- crash[ind==1,]
test <- crash[ind==2,]

model.tree <- rpart(ACCIDENT ~ . , data=train)
pred.model.tree <- predict(model.tree, test, type = "class")
table(pred.model.tree,test$ACCIDENT)

t <- table(pred.model.tree,test$ACCIDENT)
confusionMatrix(t)

set.seed(1234)
train_control <- trainControl(method="cv", number=10)
model <- train(ACCIDENT~., data=crash, trControl=train_control, method="rpart")
pred.cv.tree <- predict(model,crash)
t <- table(pred.cv.tree,crash$ACCIDENT)
confusionMatrix(t)

set.seed(1234)
train_control <- trainControl(method="cv", number=10)
knnFit <- train(ACCIDENT ~ ., data = crash, method = "knn", trControl = train_control, preProcess = c("center","scale"), tuneLength = 20)

knnFit

pred.cv.tree <- predict(knnFit,crash)
t <- table(pred.cv.tree,crash$ACCIDENT)
confusionMatrix(t)


