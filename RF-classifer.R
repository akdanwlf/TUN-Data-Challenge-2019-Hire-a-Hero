data <- fread("Data/complete_data1.csv", sep=",", header=T, strip.white = T, na.strings = c("NA","NaN","","?"))

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


mtry = tuneRF(train_data[-10],train_data$Hire_Heroes_USA_Confirmed_Hire__c,ntreeTry = 70,stepFactor=1.5,improve=0.01, 
              trace=TRUE, plot=TRUE, na.action=na.exclude)

rf_model11 = randomForest(Hire_Heroes_USA_Confirmed_Hire__c~.,data=train_data,
                          ntree=70,mtry=2,na.action=na.exclude,importance=TRUE)
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

