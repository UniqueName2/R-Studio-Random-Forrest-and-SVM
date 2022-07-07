library(dplyr)
library(e1071)
library(caret)
library(kernlab)
library(ROSE)

# Import Data
setwd("C:/Users/Sahil/Documents/Nottingham/Masters/Data Modelling and Analysis/Project")
df =  read.csv("diabetes.csv")
head(df)

# Input median values for impossible values

df$BloodPressure[df$BloodPressure == 0] <-  median(df$BloodPressure)
df$SkinThickness[df$SkinThickness == 0] <-  median(df$SkinThickness)
df$Glucose[df$Glucose == 0] <-  median(df$Glucose)
df$Insulin[df$Insulin == 0] <-  median(df$Insulin)
df$BMI[df$BMI == 0] <-  median(df$BMI)

# Normalize all data z score
z_norm <- function(x) {
  (x - mean(x)) / sd(x)
}


df_norm <- as.data.frame(lapply(df[1:8], z_norm))
df_norm$Outcome <-df$Outcome
df = df_norm

# Refactor Outcome column to remove errors from building the model
df$Outcome[df$Outcome == 1] <- "Yes"
df$Outcome[df$Outcome == 0] <- "No"

# Split data into Train and Test
set.seed(123)
smp_size <- floor(0.8 * nrow(df))
train_ind <- sample(seq_len(nrow(df)), size = smp_size)

train <- df[train_ind, ]
test <- df[-train_ind, ]

# Train and test length
print("Number of datapoints in the dataset")
table(train$Outcome)
table(test$Outcome)




# Statistics
summary(train)



# Imbalanced data - Synthetic data generation
train_balanced <-ROSE(Outcome~., data=train,seed=123)$data


table(train$Outcome)
table(train_balanced$Outcome)


# Train and Tune the SVM
ctrl <- trainControl(summaryFunction=twoClassSummary,   
                     classProbs=TRUE)



svm <- train(x=train_balanced[1:8],
                  y= as.factor(train_balanced$Outcome),
                  method = "svmLinear",
                  tuneGrid = expand.grid(C = seq(0, 2, length = 20)),
                  metric="ROC",
                  trControl=ctrl)

svm


# Predict for train and test

y_train_pred = predict(svm, newdata = train_balanced[1:8])
y_pred = predict(svm, newdata = test[1:8])

# Confusion Matrices
cm_train = table(train_balanced$Outcome, y_train_pred )
cm_pred = table(test$Outcome, y_pred)

cm_train

cm_pred

# Evaluation metrics
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



















