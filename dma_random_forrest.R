library(dplyr)
#install.packages("ROSE")
library(ROSE)
library(tidyverse)
library(caret)
library(leaps)
library(MASS)
library(randomForest)

# Import Data

#pima <- read.csv("/Users/rithwikshetty/Documents/DMA_Project/diabetes.csv",sep=",")
setwd("C:/Users/Sahil/Documents/Nottingham/Masters/Data Modelling and Analysis/Project")
pima =  read.csv("diabetes.csv")
head(pima)


# Mean Imputation on all data for impossible values
pima$BloodPressure[pima$BloodPressure == 0] <-  mean(pima$BloodPressure)
pima$SkinThickness[pima$SkinThickness == 0] <-  mean(pima$SkinThickness)
pima$Glucose[pima$Glucose == 0] <-  mean(pima$Glucose)
pima$Insulin[pima$Insulin == 0] <-  mean(pima$Insulin)
pima$BMI[pima$BMI == 0] <-  mean(pima$BMI)

# Refactor Outcome column to remove errors from building the model
pima$Outcome[pima$Outcome == 1] <- "Yes"
pima$Outcome[pima$Outcome == 0] <- "No"

# Split into Train and Test
set.seed(123)
smp_size <- floor(0.8 * nrow(pima))
train_ind <- sample(seq_len(nrow(pima)), size = smp_size)

training <- pima[train_ind, ]
testing <- pima[-train_ind, ]

# Train and test length
print("Number of datapoints in the dataset")
table(training$Outcome)
table(testing$Outcome)


#Checking for NA values in each attribute
for(i in names(training)){
  print(paste("NA values in ",toString(i)," -- ", sum(is.na(pima[i]))))
}

#Statistics
summary(training)

# Random oversampling of minority class
training_balanced <- ovun.sample(Outcome~.,data=training, method = "over", N=2*nrow(training[training$Outcome=="No",]))$data

table(training$Outcome)
table(training_balanced$Outcome)


# Train the model

control <- trainControl(summaryFunction=twoClassSummary,   
                        classProbs=TRUE)

mtry <- sqrt(ncol(training_balanced))
pima_RF <- train(as.factor(Outcome)~., data=training_balanced, method="rf", 
                   metric="ROC", tuneLength=7, trControl=control, 
                   savePredictions = "final")
print(pima_RF)
plot(pima_RF)


# Test
prediction <-predict(pima_RF$finalModel, testing)


# Confusion Matrices for training and testing data
cm_train = table(training_balanced$Outcome, pima_RF$finalModel$predicted)
cm_pred = table(testing$Outcome, prediction)


cm_train
cm_pred

# Evaluation Metrics
recognitionRate = function(confusMatrx){
  tp = confusMatrx[4]
  tn = confusMatrx[1]
  fp = confusMatrx[3]
  fn = confusMatrx[2]
  return((tp+tn)/(tp+tn+fp+fn))
}


recallRate = function(confusMatrx){
  tp = confusMatrx[4]
  tn = confusMatrx[1]
  fp = confusMatrx[3]
  fn = confusMatrx[2]
  
  return(tp/(tp+fn))
}

precisionRate = function(confusMatrx){
  tp = confusMatrx[4]
  tn = confusMatrx[1]
  fp = confusMatrx[3]
  fn = confusMatrx[2]
  
  return(tp/(tp+fp))
}

f1_measure = function(recall, precision){
  return(2*(recall*precision)/(recall+precision))
}


train_rr = recognitionRate(cm_train)
train_recall = recallRate(cm_train)
train_pres = precisionRate(cm_train)
f1_train = f1_measure(train_recall,train_pres)

pred_rr = recognitionRate(cm_pred)
pred_recall = recallRate(cm_pred)
pred_precision = precisionRate(cm_pred)
f1_test = f1_measure(pred_recall,pred_precision)

eval_df = data.frame(Data=c("Training","Test"),
                     Recognition_Rate=c(train_rr, pred_rr),
                     Recall_Rate = c(train_recall,pred_recall),
                     Precision_Rate = c(train_pres,pred_precision),
                     F1_measure = c(f1_train,f1_test))
eval_df






