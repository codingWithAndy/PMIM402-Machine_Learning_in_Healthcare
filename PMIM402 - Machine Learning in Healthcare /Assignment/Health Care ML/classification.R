install.packages("randomForest")
install.packages("ROCR")
install.packages("pROC")
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

nb_pred<-predict(nb_model, df_test)
table(predicted = nb_pred,observed = df_test$class)

print(nb_pred)

ls(nb_model)
#ls(nb_pred)






## Trying to print ROC curve
nb_pred %>% 
  roc_curve(truth=df_test$class , probClass) %>%
  autoplot()



pr <- prediction(nb_model, df_test$class)
prf <- performance(pr, "tpr", "fpr")
plot(prf)

roc(nb_pred, df_test$class, plot=TRUE)


print(df_train)


#### Random Forrest
rffit <- randomForest(df_train,df_train$class)
plot(rffit)

varImpPlot(rffit)



#prediction_for_table <- predict(rffit,df_test[,-5])
#table(observed=df_test[,5],predicted=prediction_for_table)


control <- trainControl(method="cv", number=5, savePredictions = TRUE, classProbs = TRUE)
control

head(df_train)

svmFit <- train(class ~ sex + age + pace_maker + cp + trestbps + chol + fbs + restecg + thalach, data = df_train, 
                method = "svmLinear",
                trControl = control
)




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
