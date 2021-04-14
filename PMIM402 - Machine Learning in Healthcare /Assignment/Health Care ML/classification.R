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
#install.packages("randomForest")
library(randomForest)
library(ROCR)

########################### Load in dataset ####################################
df<- read.csv("2. Classification/heart_disease_modified.csv")
View(df)
summary(df)

################# Removing non-required data features ##########################
df <- within(df, rm(X, Patient_ID))
df <- na.omit(df)
df$class <- as.factor(df$class)

#df_labels <- df %>% select(class)
print(df_labels)
print(df)

############### Split data into training and testin ############################
bound <- floor((nrow(df)/4)*3)   # 75-25% training testing split
df <- df[sample(nrow(df)),]          # shuffles the data 
df_train <- df[1:bound, ]              # get training set
df_test <- df[(bound+1):nrow(df), ]    # get test set

######################## Naive Bayes Model #####################################
nb_model <- naiveBayes(class ~ ., data = df_train)
print(nb_model)

# predicting and plotting simple CM
nb_pred <- predict(nb_model, newdata = df_test)
table(predicted = nb_pred, observed = df_test$class)

print(nb_pred)

# ROC
test.nb = predict(nb_model, type = "raw", newdata = df_test)
nbpred = prediction(test.nb[,2], df_test$class)
nbperf = performance(nbpred, "tpr", "fpr")
plot(nbperf, main="Naive Bayes ROC", colorize=T)#
plot(nbperf, col=1, add=TRUE)
legend(0.6, 0.6, c('Naive Bayes'), 1:3)


# More detailed CM
confusionMatrix(nb_pred, df_test$class)

##### AUC values
auc_nb <- performance(nbpred, measure = "auc")
auc_nb <- auc_nb@y.values[[1]]

print(auc_nb)

###################### Random Forrest ##########################################
forest_train <- randomForest(class ~ ., data = df_train)
print(forest_train)
plot(forest_train)

# Prediction and simple CM
testforest = predict(forest_train, newdata=df_test)
table(testforest, df_test$class)

# ROC
test.forest = predict(forest_train, type = "prob", newdata = df_test)
forestpred = prediction(test.forest[,2], df_test$class)
forestperf = performance(forestpred, "tpr", "fpr")
plot(forestperf, main="Random Forrest ROC", colorize=T)#
plot(forestperf, col=1, add=TRUE)
legend(0.6, 0.6, c('rforest'), 1:3)

# Feature Importance
varImpPlot(forest_train)

# More detailed CM
confusionMatrix(testforest, df_test$class)

# AUC
auc_rf <- performance(forestpred, measure = "auc")
auc_rf <- auc_rf@y.values[[1]]
print(auc_rf)

################ Optimizing A selected Model ################################### 
forest_train2 <- randomForest(class ~ ., data = df_train, 
                              ntree = 800, mtry = 4, 
                              maxnodes=24)

print(forest_train2)
plot(forest_train2)

# Predict and simple CM
testforest2 = predict(forest_train2, newdata=df_test)
table(testforest2, df_test$class)


# ROC compare against 3 Models
test.forest2 = predict(forest_train2, type = "prob", newdata = df_test)
forestpred2 = prediction(test.forest2[,2], df_test$class)
forestperf2 = performance(forestpred2, "tpr", "fpr")
plot(forestperf2, main="ROC for RF, NB and optimised Models", colorize=T)
plot(forestperf, col=1, add=TRUE)#
plot(nbperf, col=2, add=TRUE)#
plot(forestperf2, col=3, add=TRUE)
legend(0.6, 0.6, c('rforest', 'nb', 'rforest opt'), 1:3)

# AUC
auc_rf2 <- performance(forestpred2, measure = "auc")
auc_rf2 <- auc_rf2@y.values[[1]]
print(auc_rf2)

#################### Comparing Models ##########################################
# Confusion Matrix
confusionMatrix(nb_pred, df_test$class)
confusionMatrix(testforest, df_test$class)
confusionMatrix(testforest2, df_test$class)

# AUC
auc_nb <- performance(nbpred, measure = "auc")
auc_nb <- auc_nb@y.values[[1]]
auc_rf <- performance(forestpred, measure = "auc")
auc_rf <- auc_rf@y.values[[1]]
auc_rf2 <- performance(forestpred2, measure = "auc")
auc_rf2 <- auc_rf2@y.values[[1]]
print(auc_nb)
print(auc_rf)
print(auc_rf2)

######################## EOF ###################################################

############ Experimenting
#forest_train3 <- randomForest(class ~ cp + thal + thalach + exang + oldpeak + age + chol, data = df_train)
#print(forest_train3) #notice the number of trees, number of splits and the confusion matrix
#plot(forest_train3)
#
#testforest3 = predict(forest_train3, newdata=df_test)
#table(testforest3, df_test$class) #confusion matrix for test set
#
#
#test.forest3 = predict(forest_train3, type = "prob", newdata = df_test)
#forestpred3 = prediction(test.forest3[,2], df_test$class)
#forestperf3 = performance(forestpred3, "tpr", "fpr")
#plot(forestperf3, main="ROC for RF, NB and optimised Models", colorize=T)#
#plot(forestperf, col=1, add=TRUE)#
#plot(nbperf, col=2, add=TRUE)#
#plot(forestperf2, col=3, add=TRUE)
#plot(forestperf3, col=4, add=TRUE)
#legend(0.6, 0.6, c('rforest', 'nb', 'rforest opt', 'rforest opt 2'), 1:4)

