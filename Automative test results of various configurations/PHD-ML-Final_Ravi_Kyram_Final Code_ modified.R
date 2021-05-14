
## Managing working directories
getwd()
setwd("C:/Users/rk113490/Desktop/PHD-Machine Learning")

##  Read Train & Test data set
train <- read.csv("Train.csv",header = T)
test <- read.csv("Test.csv",header = T)

##  Undertanding data structure

str(train)
str(test)
dim(train)
dim(test)
head(train)
head(test) 
tail(train)
tail(test)
summary(train)
summary(test)
View(train)
View(test)
names(train)
names(test)
attach(train)
attach(test)

#### Data Preparation

library(Hmisc)
describe(train)
describe(test)

## Type conversion

str(train)

train$Number.of.Cylinders <-  as.factor(as.character(train$Number.of.Cylinders))
test$Number.of.Cylinders <-  as.factor(as.character(test$Number.of.Cylinders))

str(train)
str(test)

## Handling misssing values

sum(is.na(train))
sum(is.na(test))

#Finding count of missing values in columns

colSums(is.na(train))
colSums(is.na(test))

## Imputation on training & test data

library(DMwR)
data_train <- knnImputation(train,k=5)
sum(is.na(data_train))

test_imp <-knnImputation(test,k=5) #KNN Imputation
sum(is.na(test_imp))
View(test_imp)

## Preparing the data sets

Train <- data_train
Test <- test_imp

names(Train)
names(Test)

### rEmoving main bearing column as it shows only 1 factor

Train <- Train[,-c(16)]
Test <- Test[,-c(15)]


sum(is.na(Test))

### Merging additional data to actual data

AddTrain <- read.csv("Train_AdditionalData.csv",header = T)
AddTest <- read.csv("Test_AdditionalData.csv",header = T)

Train_TestA = data.frame(AddTrain$TestA)
colnames(Train_TestA) = "ID"
Train_TestA[is.na(Train_TestA) == TRUE]
Train_TestA$TestA = ifelse(is.na(Train_TestA) == TRUE,0,1)
Train = merge(x = Train, y = Train_TestA, by.x = c("ID"), all = TRUE)
str(Train)
   

Test_TestA = data.frame(AddTest$TestA)
colnames(Test_TestA) = "ID"
Test_TestA[is.na(Test_TestA) == TRUE]
Test_TestA$TestA = ifelse(is.na(Test_TestA) == TRUE,0,1)
Test = merge(x = Test, y = Test_TestA, by = "ID", all = TRUE)
str(Test)

Train_TestB = data.frame(AddTrain$TestB)
colnames(Train_TestB) = "ID"
Train_TestB[is.na(Train_TestB) == TRUE]
Train_TestB$TestB = ifelse(is.na(Train_TestB) == TRUE,0,1)
unique(Train_TestB$TestB)
Train = merge(x = Train, y = Train_TestB, by = "ID", all = TRUE)
str(Train)

Test_TestB = data.frame(AddTest$TestB)
colnames(Test_TestB) = "ID"
Test_TestB[is.na(Test_TestB) == TRUE]
Test_TestB$TestB = ifelse(is.na(Test_TestB) == TRUE,0,1)
unique(Test_TestB$TestB)
Test = merge(x = Test, y = Test_TestB, by = "ID", all = TRUE)
str(Test)


## Handling misssing values

sum(is.na(Train))
sum(is.na(Test))

#Finding count of missing values in columns

colSums(is.na(Train))
colSums(is.na(Test))

### To remove the rows from 3157 in Train as engine id is not available for both test
### and train data

Train <- Train[-c(3157:3195),c(1:23)]
Test <- Test[-c(1054:1066),c(1:22)]

## Handling misssing values

sum(is.na(Train))
sum(is.na(Test))

#Finding count of missing values in columns
colSums(is.na(Train))
colSums(is.na(Test))

## Changing NAs to 0 in TestA & TestB columns in both Train & Test data
Train$TestA[is.na(Train$TestA)] <- 0
Train$TestB[is.na(Train$TestB)] <- 0

Test$TestA[is.na(Test$TestA)] <- 0
Test$TestB[is.na(Test$TestB)] <- 0

str(Train$TestA)

# converting into factors of Test A & TestB columns in both Train & Test data

Train$TestA <-  as.factor(as.character(Train$TestA))
str(Train$TestA)

Train$TestB <-  as.factor(as.character(Train$TestB))
str(Train$TestB)

Test$TestA <-  as.factor(as.character(Test$TestA))
str(Test$TestA)

Test$TestB <-  as.factor(as.character(Test$TestB))
str(Test$TestB)


#Removing the  ID Column

data <- Train[,-c(1)]

# To check the dimension matching of Train & Test data

dim(data)
dim(Test)

#Recode the levels of y:

data$y <- ifelse(data$y=="pass",1,0)
data$y <- as.factor(as.character(data$y))


#Split the data using Caret Package

#Using caret package:

library(caret)
set.seed(123)
train_rows <- createDataPartition(data$y, p = 0.7, list = F)
train_data <- data[train_rows, ]
valid_data <- data[-train_rows, ]

write.csv(train_data, "train_data.csv")#writing the file
str(train_data)


#### Data Visualization:


install.packages("DataExplorer")

library(DataEXplorer)
require(DataExplorer)

create_report(train_data)

library(ggplot2)

############################ Decision tree ################################################

######### 85% accuracy ################
#Decision Trees using CART (For Classification Problem)
library(rpart)
dtCart=rpart(y~.,data=train_data,method="class")   
plot(dtCart,main="Engine Suceess",margin=0.15,uniform=TRUE)
text(dtCart,use.n=T)
summary(dtCart)

###Another way of plotting rpart plot
install.packages("rpart.plot")
library(rpart.plot)
rpart.plot(dtCart,fallen.leaves = T)

###Model Evaluation- Error Metrics

a=table(train_data$y, predict(dtCart, newdata=train_data, type="class"))
a
(a[2,2])/(a[2,1]+a[2,2])*100 # True positive rate/Recall/Sensitivity # 86%
(a[1,1])/(a[1,1]+a[1,2])*100 #specificity/True negative rate # 84%
(a[2,2]+a[1,1])/(a[1,2]+a[2,1]+a[2,2]+a[1,1])*100 # accuracy #86%
(a[2,2])/(a[1,2]+a[2,2])*100 # Precison # 85%

a=table(valid_data$y, predict(dtCart, newdata=valid_data, type="class"))
a
(a[2,2])/(a[2,1]+a[2,2])*100 # True positive rate/Recall/Sensitivity # 91%
(a[1,1])/(a[1,1]+a[1,2])*100 #specificity/True negative rate # 87%
(a[2,2]+a[1,1])/(a[1,2]+a[2,1]+a[2,2]+a[1,1])*100 # accuracy # 89%
(a[2,2])/(a[1,2]+a[2,2])*100 # Precison # 88%



###Importance of cp parameter
#Start with cp=0 and observe the tree 
dtCart=rpart(y ~.,data=train_data,method="class", cp=0.001)
printcp(dtCart)

#Checking with different values of cp parameter
dtCart=rpart(y~.,data=train_data,method="class",control = rpart.control(cp =0.0032710 ))    
plot(dtCart,main="Classification Tree for bankruptClass",margin=0.15,uniform=TRUE)
text(dtCart,use.n=T)

summary(dtCart)

# other style of plot

rpart.plot(dtCart,fallen.leaves = T)

a=table(train_data$y, predict(dtCart, newdata=train_data, type="class"))
a
(a[2,2])/(a[2,1]+a[2,2])*100 # True positive rate/Recall/Sensitivity #91%
(a[1,1])/(a[1,1]+a[1,2])*100 #specificity/True negative rate # 81%
(a[2,2]+a[1,1])/(a[1,2]+a[2,1]+a[2,2]+a[1,1])*100 # accuracy # 86%
(a[2,2])/(a[1,2]+a[2,2])*100 # Precison # 83%

a=table(valid_data$y, predict(dtCart, newdata=valid_data, type="class"))
a
(a[2,2])/(a[2,1]+a[2,2])*100 # True positive rate/Recall/Sensitivity # 94%
(a[1,1])/(a[1,1]+a[1,2])*100 #specificity/True negative rate # 81%
(a[2,2]+a[1,1])/(a[1,2]+a[2,1]+a[2,2]+a[1,1])*100 # accuracy # 87%
(a[2,2])/(a[1,2]+a[2,2])*100 # Precison # 84%

### to predict the values on test data

pred<-predict(dtCart,Test)
pred

pred1 <- colnames(pred)[apply(pred,1,which.max)]
pred1

submission <- read.csv("Sample_Submission.csv",header = T)

submission$ID <- Test$ID
submission$y <- pred1
write.csv(submission,"submission1.csv",row.names = F)

submission$y <- ifelse(submission$y==1,"pass","fail")
submission$y <- as.factor(as.character(submission$y))

write.csv(submission,"submission1.csv",row.names = F)

### 85% accuracy after uploading ###########

#Decision Trees using C5.0 (For Classification Problem)
#Loading library for C5.0


#calling C5.0 function
install.packages("C50")
library(C50)
dtC50= C5.0(y ~ ., data = train_data, rules=TRUE)
summary(dtC50)
C5imp(dtC50, pct=TRUE)


#calling C5.0 function
install.packages("C50")
library(C50)
dtC50= C5.0(y ~ ., data = valid_data, rules=TRUE)
summary(dtC50)
C5imp(dtC50, pct=TRUE)


#Model Evaluation- Error Metrics 
a=table(train_data$y, predict(dtC50, newdata=train_data, type="class"))
(a[2,2])/(a[2,1]+a[2,2])*100 # True positive rate/Recall/Sensitivity # 87%
(a[1,1])/(a[1,1]+a[1,2])*100 #specificity/True negative rate # 84%
(a[2,2]+a[1,1])/(a[1,2]+a[2,1]+a[2,2]+a[1,1])*100 # accuracy # 86%
(a[2,2])/(a[1,2]+a[2,2])*100 # Precison # 86%

a=table(valid_data$y, predict(dtC50, newdata=valid_data, type="class"))
(a[2,2])/(a[2,1]+a[2,2])*100 # True positive rate/Recall/Sensitivity # 91%
(a[1,1])/(a[1,1]+a[1,2])*100 #specificity/True negative rate # 87%
(a[2,2]+a[1,1])/(a[1,2]+a[2,1]+a[2,2]+a[1,1])*100 # accuracy # 89%
(a[2,2])/(a[1,2]+a[2,2])*100 # Precison # 88%

############################ Random Forest #######################################


################# 86% Accuracy  ###########################


# Build the classification model using randomForest
library(randomForest)
rf <- randomForest(y ~ ., data=train_data, keep.forest=TRUE, ntree=1800) 

# View results and understand important attributes
print(rf)
rf$predicted 
rf$importance  

# or importance(hepatitis_rf)
round(importance(rf), 2)   

# Extract and store important variables obtained from the random forest model
Imp_rf <- data.frame(rf$importance)
Imp_rf <- data.frame(row.names(Imp_rf),Imp_rf[,1])
colnames(Imp_rf) = c('Attributes','Importance')
Imp_rf <- Imp_rf[order(Imp_rf$Importance , decreasing = TRUE),]
Imp_rf <- Imp_rf[1:6,]

# plot (directly prints the important attributes) 
varImpPlot(rf)

# Predict on Train data 
pred_model_train <-predict(rf,train_data,type="response", norm.votes=TRUE)
result_train <- table("actual _values"= train_data$y,pred_model_train)
result_train

# Predicton Test Data
pred_model_test <-predict(rf,valid_data,type="response", norm.votes=TRUE)
result_test <- table("actual _values"= valid_data$y,pred_model_test)
result_test

# Accuracy
train_accuracy <- sum(diag(result_train))/sum(result_train)*100
train_accuracy  ### 94 %

test_accuracy <- sum(diag(result_test))/sum(result_test)*100
test_accuracy ### 86%


##Parameter Tuning in RandomForest ####################

# increase/decrease "ntree", mtry, nodesize and see if there is any improvement in the above metrics

rf2 <- randomForest(y ~ ., data=train_data, keep.forest=TRUE, ntree=1400, mtry = 5, nodesize = 3) 

pred_model_train2 <-predict(rf2,train_data,type="response", norm.votes=TRUE)
result_train2 <- table("actual _values2"= train_data$y,pred_model_train2)
result_train2
#OR
#table(trainR$target, predict(hepatitis_rf, trainR, type="response", norm.votes=TRUE)) 

# Predicton Test Data
pred_model_test2 <-predict(rf2,valid_data,type="response", norm.votes=TRUE)
result_test2 <- table("actual _values"= valid_data$y,pred_model_test2)
result_test2

# Accuracy

train_accuracy <- sum(diag(result_train2))/sum(result_train2)*100
train_accuracy # 94 %

test_accuracy2 <- sum(diag(result_test2))/sum(result_test2)*100
test_accuracy2 # 86%

rf4 <- randomForest(y ~ ., data=train_data, keep.forest=TRUE, ntree=1900, mtry = 7, nodesize = 8) 

pred_model_train4 <-predict(rf4,train_data,type="response", norm.votes=TRUE)
result_train3 <- table("actual _values2"= train_data$y,pred_model_train2)
result_train3

e <- result_train3


(e[2,2])/(e[2,1]+e[2,2])*100 # True positive rate/Recall/Sensitivity # 98%
(e[1,1])/(e[1,1]+e[1,2])*100 #specificity/True negative rate # 90%
(e[2,2]+e[1,1])/(e[1,2]+e[2,1]+e[2,2]+e[1,1])*100 # accuracy # 94%
(e[2,2])/(e[1,2]+e[2,2])*100 # Precison 90 %

#OR
#table(trainR$target, predict(rf, train, type="response", norm.votes=TRUE)) 

# Predicton Test Data
pred_model_test3 <-predict(rf4,valid_data,type="response", norm.votes=TRUE)
result_test3 <- table("actual _values"= valid_data$y,pred_model_test2)
result_test 

b <- result_test3

(b[2,2])/(b[2,1]+b[2,2])*100 # True positive rate/Recall/Sensitivity # 94%
(b[1,1])/(b[1,1]+b[1,2])*100 #specificity/True negative rate # 79%
(b[2,2]+b[1,1])/(b[1,2]+b[2,1]+b[2,2]+b[1,1])*100 # accuracy # 86 %
(b[2,2])/(b[1,2]+b[2,2])*100 # Precison # 82%

pred2 <- predict(rf4,Test,type="response", norm.votes=TRUE)

submission <- read.csv("Sample_Submission.csv",header = T)

submission$ID <- Test$ID
submission$y <- pred2
write.csv(submission,"submission3.csv",row.names = F)

submission$y <- ifelse(submission$y==1,"pass","fail")
submission$y <- as.factor(as.character(submission$y))

write.csv(submission,"submission3.csv",row.names = F)

######   86 % ####################3
#########################Naive Bayes ##########################################


##### Less than 85% #####

library(e1071)
model = naiveBayes(y ~ ., data = train_data)
model

pred = predict(model, train_data)
naive1 <- table(pred, train_data$y)

pred = predict(model, valid_data)
naive2 <- table(pred, valid_data$y)

pred_Naive <- predict(model,Test)

train_accuracy_Naive <- sum(diag(naive1))/sum(naive1)*100
train_accuracy # 94%

test_accuracy2 <- sum(diag(naive2))/sum(naive2)*100
test_accuracy2 # 79%


submission <- read.csv("Sample_Submission.csv",header = T)

submission$ID <- Test$ID
submission$y <- pred_Naive
write.csv(submission,"submission2.csv",row.names = F)

submission$y <- ifelse(submission$y==1,"pass","fail")
submission$y <- as.factor(as.character(submission$y))

write.csv(submission,"submission2.csv",row.names = F)


########################### SVM ##############################################
#########86%######################
# create model

library(e1071)
svmmodel <- svm(y ~.,data = train_data,kernel="radial",cost=5,scale = F)
summary(svmmodel)



svmpred_train = predict(svmmodel, train_data)
svm1 <- table(svmpred_train,train_data$y)

train_accuracy_svm <- sum(diag(svm1))/sum(svm1)*100
train_accuracy  ### 94%

svmpred_test = predict(svmmodel,valid_data)
svm2 <- table(svmpred_test,valid_data$y)

test_accuracy_svm <- sum(diag(svm2))/sum(svm2)*100
test_accuracy # 86


svm_tune <- tune.svm(y ~., data = train_data, gamma = 10^(-6:-1), cost = 10^(1:2))

summary (svm_tune) # to select best gamma and cost

svmmodel1 <- svm(y ~.,data = train_data,kernel="radial",cost=10,gamma =0.01,scale = F)
summary(svmmodel1)

svmpred_train1 = predict(svmmodel1, train_data)
svm1 <- table(svmpred_train1,train_data$y)

train_accuracy_svm <- sum(diag(svm1))/sum(svm1)*100
train_accuracy_svm

svmpred_test1 = predict(svmmodel1,valid_data)
svm2 <- table(svmpred_test1,valid_data$y)

test_accuracy_svm <- sum(diag(svm2))/sum(svm2)*100
test_accuracy_svm # 86%

### prediction on output file

pred1 <- predict(svmmodel1,Test)
str(Test)

submission <- read.csv("Sample_Submission.csv",header = T)

submission$ID <- Test$ID
submission$y <- pred1
write.csv(submission,"submission4.csv",row.names = F)

submission$y <- ifelse(submission$y==1,"pass","fail")
submission$y <- as.factor(as.character(submission$y))

write.csv(submission,"submission4.csv",row.names = F)


###############Gradient Boosting############################################
library(caret)
xgb.ctrl <- trainControl(method = "repeatedcv", repeats = 3, number = 3,
                         search='random',
                         allowParallel=T)
xgb.tune <-train(y~.,
                 data = train_data,
                 method="xgbTree",
                 trControl=xgb.ctrl,
                 # tuneGrid=xgb.grid,
                 tuneLength=20,
                 verbose=T,
                 metric="Accuracy",
                 nthread=3)
xgb.tune

View(xgb.tune$results)

a <- xgb.tune$results[order(xgb.tune$results$Accuracy, decreasing = TRUE),]
View(a)
par(mfrow=c(2,1))
hist(a$nrounds[1:10])
hist(a$nrounds[40:50])

# plot(xgb.tune)
preds <- predict(xgb.tune, valid_data)
confusionMatrix(valid_data$y, preds)

# plot(xgb.tune)
preds <- predict(xgb.tune, train_data)
confusionMatrix(train_data$y, preds)

pred1 <- predict(xgb.tune,Test)
str(Test)



submission <- read.csv("Sample_Submission.csv",header = T)

submission$ID <- Test$ID
submission$y <- pred1
write.csv(submission,"submission5.csv",row.names = F)

submission$y <- ifelse(submission$y==1,"pass","fail")
submission$y <- as.factor(as.character(submission$y))

write.csv(submission,"submission5.csv",row.names = F)



############### 85 % ########################

#########################Logistic Regression ################################

log_reg <- glm(y~., data = train_data, family = binomial)
summary(log_reg)

prob_train <- predict(log_reg, type="response")
# By default if no dataset is mentioned, training data is used
prob_test <- predict(log_reg, valid_data, type="response") # Predicting on test data



library(ROCR) 
pred <- prediction(prob_train, train_data$y)
perf <- performance(pred, measure="tpr", x.measure="fpr")

#Plot the ROC curve using the extracted performance measures (TPR and FPR)
plot(perf, col=rainbow(10), colorize=T, print.cutoffs.at=seq(0,1,0.05))
perf_auc <- performance(pred, measure="auc")
auc <- perf_auc@y.values[[1]]
print(auc)

pred_class <- ifelse(prob_train > 0.1, 1, 0)
table(train_data$y,pred_class)


prob_test <- predict(log_reg, valid_data, type = "response")
preds_test <- ifelse(prob_test > 0.1, "yes", "no")
table(preds_test)


conf_matrix <- table(valid_data$y, preds_test)
print(conf_matrix)
specificity <- conf_matrix[1, 1]/sum(conf_matrix[1, ])
sensitivity <- conf_matrix[2, 2]/sum(conf_matrix[2, ])
accuracy <- sum(diag(conf_matrix))/sum(conf_matrix)
accuracy

##### 81% ###############
