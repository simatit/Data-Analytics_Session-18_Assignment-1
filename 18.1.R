# 2. Perform the below given activities:
#  a. Create classification model using different decision trees.
#  b. Verify model goodness of fit.
#  c. Apply all the model validation techniques.
#  d. Make conclusions

View(weight_lifting_exercises)
str(weight_lifting_exercises)
weight_lifting_exercises<-data.frame(weight_lifting_exercises[,-c(11:35,49:58,68:82,86:100,102:111,124:138,140:149)])

str(weight_lifting_exercises)
summary(weight_lifting_exercises)

weightTrain<-weight_lifting_exercises[1:2012,]
weightTest<-weight_lifting_exercises[2013:4024,]
summary(weightTrain)
names(weightTrain)

# a. Create classification model using different decision trees.

weightTrain<-data.frame(weightTrain[,-c(11:35,49:58,68:82,86:100,102:111,124:138,140:149)])
library(caret)
library(Hmisc)
weightTrain$raw_timestamp_part_1<-impute(weightTrain$raw_timestamp_part_1,mean)
weightTrain$raw_timestamp_part_2<-impute(weightTrain$raw_timestamp_part_2,mean)
weightTrain$cvtd_timestamp<-impute(weightTrain$cvtd_timestamp,mean)
weightTrain$new_window<-impute(weightTrain$new_window,mean)
weightTrain$num_window<-impute(weightTrain$num_window,mean)
weightTrain$roll_belt<-impute(weightTrain$roll_belt,mean)
weightTrain$pitch_belt<-impute(weightTrain$pitch_belt,mean)
weightTrain$yaw_belt<-impute(weightTrain$yaw_belt,mean)
summary(weightTrain)
str(weightTrain)

weightTrain$cvtd_timestamp<-as.integer(weightTrain$cvtd_timestamp)
weightTrain$new_window<-as.integer(weightTrain$new_window)
library(tree)
tree<-tree(classe~. , 
           data = weightTrain)
plot(tree,pretty = 0.1)
text(tree,pretty = 1.2)
summary(tree)

library(caret)
pred <- predict(tree,weightTrain,type='class')
str(pred)
dim(pred)
dim(weightTest$classe)

weightTest$classe<-as.factor(weightTest$classe)
dim(weightTest$classe)
table(weightTest$classe,pred)

length(pred)
length(weightTest$classe)
confusionMatrix(pred,weightTest$classe)
#.........

install.packages("rpart")
library(rpart)
fit1 <- rpart(classe~.,data=weightTrain[,-1]) 
class(fit1)
summary(fit1)

rpart.plot::rpart.plot(fit1)

pred1<-predict(fit1,weightTrain,type = "class")
summary(pred1)
dim(pred1)
weightTest$classe<-as.factor(weightTest$classe)
table(weightTest$classe,pred1)
confusionMatrix(weightTest$classe,pred1)



# b. Verify model goodness of fit.
#........for pred.....

weightTest$classe<-as.factor(weightTest$classe)
dim(weightTest$classe)
table(weightTest$classe,pred)

length(pred)
length(weightTest$classe)
confusionMatrix(pred,weightTest$classe)


#...for fit1....

weightTest$classe<-as.factor(weightTest$classe)
table(weightTest$classe,pred1)
confusionMatrix(weightTest$classe,pred1)


# c. Apply all the model validation techniques.

set.seed(3)
cv.weight<-cv.tree(tree,FUN = prune.misclass)    #cv->cross validation

names(cv.weight)
cv.weight

par(mfrow = c(1,2))
plot(cv.weight$size,cv.weight$dev,type = 'b',col = 'red')

prune.weight<-prune.misclass(tree,best = 9)
plot(prune.weight)
text(prune.weight,pretty = 0)

weightTrain$cvtd_timestamp<-as.integer(weightTrain$cvtd_timestamp)
weightTrain$new_window<-as.integer(weightTrain$new_window)
tree.pred1<-predict(prune.weight,weightTrain,type = 'class')
table(tree.pred1,weightTest)



#............Random forest.........

set.seed(1)
library(randomForest)
a.weight_lifting_exercises<-randomForest(classe~.,weight_lifting_exercises,
                                         subset = weightTrain,mtry = 3,importance = TRUE)
rf.Carseats<-randomForest(High~. -Sales,Carseats,subset = train,mtry = 3,importance = TRUE)
dim(Carseats)
importance(a.weight_lifting_exercises)

varImpPlot(a.weight_lifting_exercises,col = 'blue',pch = 10, cex = 1.25)

a.weight_lifting_exercises

test.pred.rf<-predict(a.weight_lifting_exercises, newdata = weight_lifting_exercises[-weightTrain,],type = 'class')
table(test.pred.rf,weightTest)


#.........adaboost..........

library(adabag)
set.seed(300)
weight_lifting_exercises$classe<-as.character(weight_lifting_exercises$classe)
weight_adaboost<-boosting(classes~.,data = weight_lifting_exercises)

p.weight_adaboost<-predict(weight_adaboost,weight_lifting_exercises)
head(p.weight_adaboost)
head(p.weight_adaboost$class)
p.weight_adaboost$confusion
set.seed(300)
car_adaboost_cv<-boosting.cv(classe,data = weight_lifting_exercises)
car_adaboost_cv$confusion


# d. Make conclusions

# adaboost is best techniques which gives better results with more accuracy.