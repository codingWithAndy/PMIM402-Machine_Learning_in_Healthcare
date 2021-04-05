install.packages("randomForest")
install.packages("ROCR")
install.packages("caret")
install.packages("yardstick")
library(e1071) # all purpose machine learning package
options(scipen=999)
options(repr.plot.width = 15, repr.plot.height = 20)
library(caret)
library(pROC)
library(tidyverse)
library(yardstick)
library(tidyverse)
library(randomForest)
library(rpart)
library(ggplot2)
options(repr.plot.width = 15, repr.plot.height = 20)

df<- read.csv("2. Classification/heart_disease_modified.csv")

View(df)
summary(df)

df <- na.omit(df)

df_labels <- df %>% select(class)

df <- within(df, rm(X, Patient_ID))

print(df_labels)
print(df)

df$class <- as.factor(df$class)


## Split data into training and testing 

bound <- floor((nrow(df)/4)*3)   # 75-25% training testing split

df <- df[sample(nrow(df)),]          # shuffles the data 
df_train <- df[1:bound, ]              # get training set
df_test <- df[(bound+1):nrow(df), ]    # get test set



### Naive Bayes
nb_model <- naiveBayes(class ~ ., data = df_train)
#
##Output apriori and conditional probabilities
print(nb_model)



####### predicting and plotting CM
nb_pred <- predict(nb_model, newdata = df_test)
table(predicted = nb_pred, observed = df_test$class)

print(nb_pred)


test.nb = predict(nb_model, type = "raw", newdata = df_test)
nbpred = prediction(test.nb[,2], df_test$class)
nbperf = performance(nbpred, "tpr", "fpr")
plot(nbperf, main="ROC", colorize=T)#
plot(forestperf, col=1, add=TRUE)#
#plot(perf, col=1, add=TRUE)#
plot(nbperf, col=2, add=TRUE)
legend(0.6, 0.6, c('rforest', 'Naive Bayes'), 1:3)

ls(nb_model)
#ls(nb_pred)


trainControl()



## Trying to print ROC curve
nb_pred %>% 
  roc_curve(truth=df_test$class , nb_pred) %>%
  autoplot()



pr <- prediction(nb_pred, df_test$class)
prf <- performance(nb_pred, "tpr", "fpr")
plot(prf)

roc(nb_pred, df_test$class, plot=TRUE)


print(df_train)


#### Random Forrest
install.packages("randomForest")
library(randomForest)
library(ROCR)

forest_train <- randomForest(class ~ ., data = df_train)
print(forest_train) #notice the number of trees, number of splits and the confusion matrix
plot(forest_train)

testforest = predict(forest_train, newdata=df_test)
table(testforest, df_test$class) #confusion matrix for test set


test.forest = predict(forest_train, type = "prob", newdata = df_test)
forestpred = prediction(test.forest[,2], df_test$class)
forestperf = performance(forestpred, "tpr", "fpr")
plot(forestperf, main="ROC", colorize=T)#
#plot(bagperf, col=2, add=TRUE)#
#plot(perf, col=1, add=TRUE)#
plot(forestperf, col=3, add=TRUE)
#legend(0.6, 0.6, c('rforest'), 1:3)


varImpPlot(rffit)

rf_pred <- predicted(rffit, newdata = df_test, type = "raw")

roc(nb_pred, df_test$class, plot=TRUE)

#prediction_for_table <- predict(rffit,df_test[,-5])
#table(observed=df_test[,5],predicted=prediction_for_table)










forest_train2 <- randomForest(class ~ ., data = df_train, ntree = 900)
print(forest_train2) #notice the number of trees, number of splits and the confusion matrix
plot(forest_train2)

testforest = predict(forest_train2, newdata=df_test)
table(testforest, df_test$class) #confusion matrix for test set


test.forest2 = predict(forest_train2, type = "prob", newdata = df_test)
forestpred2 = prediction(test.forest2[,2], df_test$class)
forestperf2 = performance(forestpred2, "tpr", "fpr")
plot(forestperf2, main="ROC", colorize=T)#
plot(nbperf, col=2, add=TRUE)#
plot(forestperf, col=1, add=TRUE)#
plot(forestperf2, col=3, add=TRUE)
legend(0.6, 0.6, c('rforest', 'nb', 'rforest opt'), 1:3)







control <- trainControl(method="cv", number=5, savePredictions = TRUE, classProbs = TRUE)
control

head(df_train)

svmFit <- train(class ~ sex + age + cp + trestbps + chol + fbs + restecg + thalach, data = df_train, 
                method = "svmLinear",
                trControl = control
)

str(df_train$class)


### Experimenting
library(ROCR)
# Calculate the probability of new observations belonging to each class
# prediction_for_roc_curve will be a matrix with dimensions data_set_size x number_of_classes
prediction_for_roc_curve <- predict(rffit,df_test[,-5],type="prob")
# Use pretty colours:
pretty_colours <- c("#F8766D","#00BA38","#619CFF")
# Specify the different classes 
classes <- levels(df_test$class)
# For each class
for (i in 1:2)
{
  # Define which observations belong to class[i]
  true_values <- ifelse(df_test[,5]==classes[i],1,0)
  # Assess the performance of classifier for class[i]
  pred <- prediction(prediction_for_roc_curve[,i],true_values)
  perf <- performance(pred, "tpr", "fpr")
  if (i==1)
  {
    plot(perf,main="ROC Curve",col=pretty_colours[i]) 
  }
  else
  {
    plot(perf,main="ROC Curve",col=pretty_colours[i],add=TRUE) 
  }
  # Calculate the AUC and print it to screen
  auc.perf <- performance(pred, measure = "auc")
  print(auc.perf@y.values)
}
