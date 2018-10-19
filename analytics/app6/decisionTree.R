#Author: fullarray
#"Profit has a thousand parents, bankruptcy is an orphan"

#Read data from file
train <- read.csv("kddcup.data_10_percent_corrected")
test <- read.csv("kddcup.testdata.labeled_10_percent")

#Install and use c5.0 decision tree to train the model
install.packages("c50")
library(c50)
train_model <- c5.0(train[-42], train$labels)

#Summary the model
summary(train_model)

#Display crosstable
CrossTable(test$labels, pred, prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE, dnn = c('actual default', 'predicted default'))
