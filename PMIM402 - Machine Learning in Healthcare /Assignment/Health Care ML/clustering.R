#install.packages("e1071")
#install.packages('factoextra')
library(e1071) # all purpose machine learning package
library(reshape2) # library to reshape data
library(ggplot2)
options(scipen=999)


## Taken from unsuperised learning database notebook

library(tidyverse) # data manipulation
library(cluster) # clustering algorithms 
library(factoextra)
options(repr.plot.width = 15, repr.plot.height = 20)


########################### Load in dataset ####################################
df<- read.csv("1. Clustering/heart-c.csv")

View(df)
summary(df)


df_og <- df

# remove num and X variables from df
#df %>% select(-X, -num)
#df %>% drop_na()

df <- na.omit(df)

#print(df['cp'][4,1])

for (i in 1:nrow(df['sex'])){
  if (df['sex'][i,1] == 'male') {
    df['sex'][i,1] = as.numeric(1)
  }
  else if (df['sex'][i,1] == 'female') {
    df['sex'][i,1] = as.numeric(0)
  }
  
}
print(df['sex'])

for (i in 1:nrow(df['cp'])){
  if (df['cp'][i,1] == 'typ_angina') {
    df['cp'][i,1] = as.numeric(1)
  }
  else if (df['cp'][i,1] == 'atyp_angina') {
    df['cp'][i,1] = as.numeric(2)
  }
  else if (df['cp'][i,1] == 'non_anginal') {
    df['cp'][i,1] = as.numeric(3)
  }
  else if (df['cp'][i,1] == 'asympt') {
    df['cp'][i,1] = as.numeric(4)
  }
}
print(df['cp'])

for (i in 1:nrow(df['fbs'])){
  if (df['fbs'][i,1] == 't') {
    df['fbs'][i,1] = as.numeric(1)
  }
  else if (df['fbs'][i,1] == 'f') {
    df['fbs'][i,1] = as.numeric(0)
  }
  
}
print(df['fbs'])

for (i in 1:nrow(df['restecg'])){
  if (df['restecg'][i,1] == 'normal') {
    df['restecg'][i,1] = as.numeric(0)
  }
  else if (df['restecg'][i,1] == 'st_t_wave_abnormality') {
    df['restecg'][i,1] = as.numeric(1)
  }
  else if (df['restecg'][i,1] == 'left_vent_hyper') {
    df['restecg'][i,1] = as.numeric(2)
  }
  
}
print(df['restecg'])

for (i in 1:nrow(df['exang'])){
  if (df['exang'][i,1] == 'yes') {
    df['exang'][i,1] = as.numeric(1)
  }
  else if (df['exang'][i,1] == 'no') {
    df['exang'][i,1] = as.numeric(0)
  }
  
}
print(df$exang)

for (i in 1:nrow(df['slope'])){
  if (df['slope'][i,1] == 'up') {
    df['slope'][i,1] = as.numeric(1)
  }
  else if (df['slope'][i,1] == 'flat') {
    df['slope'][i,1] = as.numeric(2)
  }
  else if (df['slope'][i,1] == 'down') {
    df['slope'][i,1] = as.numeric(3)
  }
  
}
print(df['slope'])

for (i in 1:nrow(df['thal'])){
  if (df['thal'][i,1] == 'normal') {
    df['thal'][i,1] = as.numeric(3)
  }
  else if (df['thal'][i,1] == 'fixed_defect') {
    df['thal'][i,1] = as.numeric(6)
  }
  else if (df['thal'][i,1] == 'reversable_defect') {
    df['thal'][i,1] = as.numeric(7)
  }
  else if (df['thal'][i,1] == 'NA') {
    df['thal'][i,1] = as.numeric(0)
  }
  
}
#print(df$thal[1])
#print(typeof(df$sex[1]))
#view(df)
#
#df %>% select(-thal,-num, -X)
#
#
#df %>%
#  gather(attributes, value, 1:13) %>%    # gather dplyr verb will turn wide data into long format
#  ggplot(aes(x = value)) +
#  geom_histogram(fill = 'lightblue2', color = 'black') +
#  facet_wrap(~attributes, scales = 'free_x') +
#  labs(x="Values", y="Frequency") +
#  stat("count") +
#  theme_bw()
#
#
## Split data into training and testing 
#
#bound <- floor((nrow(df)/4)*3)   # 75-25% training testing split
#
#df <- df[sample(nrow(df)),]          # shuffles the data 
#df.train <- df[1:bound, ]              # get training set
#df.test <- df[(bound+1):nrow(df), ]    # get test set
#
#
##nb_model <- naiveBayes(labels ~ ., data = df.train)
##
###Output apriori and conditional probabilities
##print(nb_model)
#print(df)

df <- within(df, rm(num, X))

df$age <- as.numeric(df$age)
df$sex <- as.numeric(df$sex)
df$cp <- as.numeric(df$cp)
df$trestbps <- as.numeric(df$trestbps)
df$chol <- as.numeric(df$chol)
df$fbs <- as.numeric(df$fbs)
df$restecg <- as.numeric(df$restecg)
df$thalach <- as.numeric(df$thalach)
df$exang <- as.numeric(df$exang)
df$oldpeak <- as.numeric(df$oldpeak)
df$slope <- as.numeric(df$slope)
df$ca <- as.numeric(df$ca)
df$thal <- as.numeric(df$thal)

#clean_data <- na.omit(df)


df_norm <- as.data.frame(scale(df))
head(df_norm)


# K-Means
df_K <- kmeans(df_norm, centers = 2)
df_K

## Hierarchical
dist_mat <- dist(df_norm, method = 'euclidean')
hclust_avg <- hclust(dist_mat, method = 'average')
plot(hclust_avg)
