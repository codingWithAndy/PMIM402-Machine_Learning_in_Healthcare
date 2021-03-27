install.packages("e1071")
install.packages('reshape2')
library(e1071) # all purpose machine learning package
library(reshape2) # library to reshape data
library(ggplot2)
options(scipen=999)


########################### Load in dataset ####################################
df<- read.csv("1. Clustering/heart-c.csv")

View(df)
summary(df)

