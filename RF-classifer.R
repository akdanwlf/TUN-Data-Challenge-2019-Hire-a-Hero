library(data.table)  
data <- fread("Data/complete_data_blank.csv", sep=",", header=T, strip.white = T, na.strings = c("NA","NaN","","?"))

summary(data)

library(tidyverse)

data <- data %>% mutate_all(as.factor)

train_index = sample(1:nrow(data),round(0.7*nrow(data)))
train_data = data[train_index,]
test_data = data[-train_index,]

nrow(train_data)
nrow(test_data)

summary(train_data)
summary(test_data)


library(randomForest)


set.seed(42)
rf_model1 = randomForest(Hire_Heroes_USA_Confirmed_Hire__c~.,data=train_data,ntree=10,na.action=na.exclude)
print(rf_model1)
rf_model2 = randomForest(Hire_Heroes_USA_Confirmed_Hire__c~.,data=train_data,ntree=20,na.action=na.exclude)
print(rf_model2)
rf_model3 = randomForest(Hire_Heroes_USA_Confirmed_Hire__c~.,data=train_data,ntree=30,na.action=na.exclude)
print(rf_model3)
rf_model4 = randomForest(Hire_Heroes_USA_Confirmed_Hire__c~.,data=train_data,ntree=40,na.action=na.exclude)
print(rf_model4)
rf_model5 = randomForest(Hire_Heroes_USA_Confirmed_Hire__c~.,data=train_data,ntree=50,na.action=na.exclude)
print(rf_model5)
rf_model6 = randomForest(Hire_Heroes_USA_Confirmed_Hire__c~.,data=train_data,ntree=60,na.action=na.exclude)
print(rf_model6)
rf_model7 = randomForest(Hire_Heroes_USA_Confirmed_Hire__c~.,data=train_data,ntree=70,na.action=na.exclude)
print(rf_model7)
rf_model8 = randomForest(Hire_Heroes_USA_Confirmed_Hire__c~.,data=train_data,ntree=80,na.action=na.exclude)
print(rf_model8)
rf_model9 = randomForest(Hire_Heroes_USA_Confirmed_Hire__c~.,data=train_data,ntree=90,na.action=na.exclude)
print(rf_model9)
rf_model10 = randomForest(Hire_Heroes_USA_Confirmed_Hire__c~.,data=train_data,ntree=100,na.action=na.exclude)
print(rf_model10)

colnames(train_data)

set.seed(42)
mtry = tuneRF(train_data[-12],train_data$Hire_Heroes_USA_Confirmed_Hire__c,ntreeTry = 90,stepFactor=1.5,improve=0.01, 
              trace=TRUE, plot=TRUE, na.action=na.exclude)

rf_model11 = randomForest(Hire_Heroes_USA_Confirmed_Hire__c~.,data=train_data,
                          ntree=90,mtry=3,na.action=na.exclude,importance=TRUE)
print(rf_model11)


library(caret)
predicted_values = predict(rf_model11, test_data,type= "prob")
threshold = 0.5 
pred = factor( ifelse(predicted_values[,2] > threshold, '1', '0') )

levels(test_data$Hire_Heroes_USA_Confirmed_Hire__c)

levels(pred)

confusionMatrix(pred, test_data$Hire_Heroes_USA_Confirmed_Hire__c, 
                positive = levels(test_data$Hire_Heroes_USA_Confirmed_Hire__c)[2])

library(ROCR)
library(ggplot2)

predicted_values = predicted_values[,2]
pred = prediction(predicted_values, test_data$Hire_Heroes_USA_Confirmed_Hire__c)
perf = performance(pred, measure = "tpr", x.measure = "fpr")
auc = performance(pred, measure = "auc")
auc = auc@y.values[[1]]

roc.data <- data.frame(fpr=unlist(perf@x.values),
                       tpr=unlist(perf@y.values),
                       model="RF")
ggplot(roc.data, aes(x=fpr, ymin=0, ymax=tpr)) +
  geom_ribbon(alpha=0.2) +
  geom_line(aes(y=tpr)) +
  ggtitle(paste0("ROC Curve w/ AUC=", auc))

#Plotting variable importance 
varImpPlot(rf_model11)


#______________________________________________________________________________________________________________________
#Caret Models

rf <- train(Hire_Heroes_USA_Confirmed_Hire__c ~ ., 
            data = train_data, 
            method = "rf",
            tuneLength = 1)

rf
plot(rf)

## Model Evaluation:
predicted_values <- predict(knnFit, test_data, type= "prob") # Use the classifier to make the predictions

head(predicted_values)
threshold <- 0.5
pred <- factor( ifelse(predicted_values[,2] > threshold, 1, 0))
head(pred)
levels(pred)
levels(test_data$salary.class)
confusionMatrix(pred, test_data$salary.class, 
                positive = levels(test_data$salary.class)[2])

predicted_values <- predict(knnFit, test_data, type= "prob") # Use the classifier to make the predictions
pred <- prediction(predicted_values[,2], test_data$salary.class)
perf <- performance(pred, measure = "tpr", x.measure = "fpr")
auc <- performance(pred, measure = "auc")
auc <- auc@y.values[[1]]

roc.data <- data.frame(fpr=unlist(perf@x.values),
                       tpr=unlist(perf@y.values),
                       model="Decision Tree")

ggplot(roc.data, aes(x=fpr, ymin=0, ymax=tpr)) +
  geom_ribbon(alpha=0.2) +
  geom_line(aes(y=tpr)) +
  ggtitle(paste0("ROC Curve w/ AUC=", auc))

#_______________________________________________________________________________________________________________________
#ANN

#ANN using nnet
#install.packages("nnet")
library(nnet)

set.seed(42)
ann <- nnet(Hire_Heroes_USA_Confirmed_Hire__c~., data = train_data, size = 10, maxit = 1000)
summary(ann)

#Confusion matrix with caret (previously loaded) to evaluate the ANN
predicted_values_ann <- predict(ann, test_data,type= "raw") #type = "raw" gives us probabilites
head(predicted_values_ann, n=200)

threshold_ann <- 0.5 
pred_ann <- factor( ifelse(predicted_values_ann[,1] > threshold_ann, 1, 0) )
head(pred_ann, n=200)

levels(pred_ann) #classes 0 and 1
class(pred_ann) #factor
levels(test_data$Hire_Heroes_USA_Confirmed_Hire__c) #classes L and W
class(test_data$Hire_Heroes_USA_Confirmed_Hire__c) #factor

#levels(pred_ann)[1] == 0
#levels(test_data$gen_election)[2]

#predicition and target variables do not match.  We will reclassify our predictions
#pred_ann_reclass <- factor( ifelse(pred_ann == levels(pred_ann)[1],"L","W") )
#levels(pred_ann_reclass)

confusionMatrix(pred_ann, test_data$Hire_Heroes_USA_Confirmed_Hire__c, 
                positive = levels(test_data$Hire_Heroes_USA_Confirmed_Hire__c)[2])

#Generating an ROC for our ANN
pred_ann_roc <- prediction(predicted_values_ann, test_data$Hire_Heroes_USA_Confirmed_Hire__c)
class(pred_ann_roc)
slotNames(pred_ann_roc)

perf_ann <- performance(pred_ann_roc, measure = "tpr", x.measure = "fpr")

#head(predicted_values_ann)
#head(test_data$Hire_Heroes_USA_Confirmed_Hire__c)
#head(cbind(predicted_values_ann, test_data$Hire_Heroes_USA_Confirmed_Hire__c),n=100)

auc_ann <- performance(pred_ann_roc, measure = "auc")

auc_ann <- auc_ann@y.values[[1]]
roc.data_ann <- data.frame(fpr=unlist(perf@x.values),
                           tpr=unlist(perf@y.values),
                           model="ANN")
ggplot(roc.data_ann, aes(x=fpr, ymin=0, ymax=tpr)) +
  geom_ribbon(alpha=0.2) +
  geom_line(aes(y=tpr)) +
  ggtitle(paste0("ROC Curve w/ AUC=", auc_ann))

#Evaluating social media campaigns using frequency table
colnames(cleaned_data)

ftable(xtabs(~last_service_era+Hire_Heroes_USA_Confirmed_Hire__c, data = data))
ftable(xtabs(~Gender__c+Hire_Heroes_USA_Confirmed_Hire__c, data = data))
ftable(xtabs(~Race__c+Hire_Heroes_USA_Confirmed_Hire__c, data = data))
ftable(xtabs(~Foreign_Service__c+Hire_Heroes_USA_Confirmed_Hire__c, data = data))

ftable(xtabs(~Length_of_Service_bin+Hire_Heroes_USA_Confirmed_Hire__c, data = data))
ftable(xtabs(~Status__c+Hire_Heroes_USA_Confirmed_Hire__c, data = data))

ftable(xtabs(~last_service_era+Hire_Heroes_USA_Confirmed_Hire__c, data = data))
ftable(xtabs(~Foreign_Service__c+Hire_Heroes_USA_Confirmed_Hire__c, data = data))
ftable(xtabs(~Status__c+Hire_Heroes_USA_Confirmed_Hire__c, data = data))


