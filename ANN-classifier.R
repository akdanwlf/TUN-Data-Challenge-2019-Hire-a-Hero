library(nnet)

data <- fread("Data/complete_data1.csv", sep=",", header=T, strip.white = T, na.strings = c("NA","NaN","","?"))

library(tidyverse)

data <- data %>% mutate_all(as.factor)

train_index = sample(1:nrow(data),round(0.7*nrow(data)))
train_data = data[train_index,]
test_data = data[-train_index,]


nrow(train_data)
nrow(test_data)

summary(train_data)
summary(test_data)

ann_model = nnet(Hire_Heroes_USA_Confirmed_Hire__c ~ ., data=train_data, size=5, maxit=1000)
summary(ann_model)
predicted_values = predict(ann_model, test_data, type= "raw")


pred = factor( ifelse(predicted_values > 0.5, '1', '0') )

library(caret)

confusionMatrix(pred, test_data$Hire_Heroes_USA_Confirmed_Hire__c, 
                positive = levels(test_data$Hire_Heroes_USA_Confirmed_Hire__c)[2])

library(ROCR)
library(ggplot2)

pred = prediction(predicted_values, test_data$Hire_Heroes_USA_Confirmed_Hire__c)
perf = performance(pred, measure = "tpr", x.measure = "fpr")
auc = performance(pred, measure = "auc")
auc = auc@y.values[[1]]
roc.data = data.frame(fpr=unlist(perf@x.values),
                      tpr=unlist(perf@y.values),
                      model="ANN")
ggplot(roc.data, aes(x=fpr, ymin=0, ymax=tpr)) +
  geom_ribbon(alpha=0.2) +
  geom_line(aes(y=tpr)) +
  ggtitle(paste0("ROC Curve w/ AUC=", auc))
