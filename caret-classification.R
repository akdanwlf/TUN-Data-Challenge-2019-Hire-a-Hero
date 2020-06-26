library(data.table)
library(rpart)
data_blank <- fread("Data/complete_data_blank.csv", sep=",", header = T, strip.white = T
                    , na.strings = c("NA","NaN","","?"))

data_mode <-fread("Data/complete_data_mode.csv",sep=",",header = T, strip.white = T,
                 na.strings = c("NA","NaN","","?"))

data_mice <- fread("Data/complete_data_mice.csv",sep=",",header = T, strip.white = T,
                   na.strings = c("NA","NaN","","?"))

library(skimr)
library(tidyverse)
library(caret)


data_mode <- data_mode %>% mutate_all(as.factor)
data_blank[,"Hire_Heroes_USA_Confirmed_Hire__c"] <- make.names(data_blank[,"Hire_Heroes_USA_Confirmed_Hire__c"])
data_blank <- data_blank %>% mutate_all(as.factor)
skim(data_blank)

set.seed(42)
train_index = sample(1:nrow(data_blank),round(0.7*nrow(data_blank)))
train_data = data_blank[train_index,]
test_data = data_blank[-train_index,]
data_blank$Status__c<-NULL
TrainClasses <- data_blank[,"Hire_Heroes_USA_Confirmed_Hire__c"]
train_data<- data_blank[,-match("Hire_Heroes_USA_Confirmed_Hire__c",names(data_blank))]
data_blank[,"Hire_Heroes_USA_Confirmed_Hire__c"]
data_blank[,12]
colnames(data_blank)
skim(TrainClasses)
skim(train_data)

library(fastAdaboost)



grid <- expand.grid(mfinal=70, maxdepth=3,coeflearn='Breiman')
grid <- expand.grid(mtry=2)
control <- trainControl(method = "cv",number= 10, classProbs = TRUE,summaryFunction = twoClassSummary,verboseIter = TRUE)

nb <- train(train_data, TrainClasses,method="rf", trControl = control, verbose=TRUE,tuneGrid = grid)
 importanceplot(nb)

plot(varImp(nb), top = 20)
varImp(nb)
nb
plot(nb)

predicted_values <- predict(nb, test_data, type= "prob") # Use the classifier to make the predictions

head(predicted_values)
threshold <- 0.5
pred <- factor( ifelse(predicted_values[,2] > threshold, 1, 0))
head(pred)
levels(pred)
levels(test_data$Hire_Heroes_USA_Confirmed_Hire__c)
confusionMatrix(pred, test_data$Hire_Heroes_USA_Confirmed_Hire__c, 
                positive = levels(test_data$Hire_Heroes_USA_Confirmed_Hire__c)[2])

pred <- prediction(predicted_values[,2], test_data$Hire_Heroes_USA_Confirmed_Hire__c)
perf <- performance(pred, measure = "tpr", x.measure = "fpr")
auc <- performance(pred, measure = "auc")
auc <- auc@y.values[[1]]
roc.data <- data.frame(fpr=unlist(perf@x.values),
                       tpr=unlist(perf@y.values),
                       model="RF")
ggplot(roc.data, aes(x=fpr, ymin=0, ymax=tpr)) +
  geom_ribbon(alpha=0.2) +
  geom_line(aes(y=tpr)) +
  ggtitle(paste0("ROC Curve w/ AUC=", auc))
