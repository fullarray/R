#Author: fullarray
#"Profit has a thousand parents, bankruptcy is an orphan"
#SVM
#Read data from file
train <- read.csv("kddcup.data_10_percent_corrected")
test <- read.csv("kddcup.testdata.labeled_10_percent")

#Install and use SVM to train the models
install.packages("kernlab")
library(kernlab)
classifier <- ksvm(labels ~ ., data = train, kernel = "vanilladot")

#Evaluating model performance
predictions <- predict(classifier, test)
table(predictions, test$labels)

