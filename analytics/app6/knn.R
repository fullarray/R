#Author: fullarray
#"Profit has a thousand parents, bankruptcy is an orphan"

#transfer symbolic value to continuous value
train_data$protocol_type <- as.numeric(train_data$protocol_type)
train_data$service <- as.numeric(train_data$service)
train_data$flag <- as.numeric(train_data$flag)
test_data$protocol_type <- as.numeric(test_data$protocol_type)
test_data$service <- as.numeric(test_data$service)
test_data$flag <- as.numeric(test_data$flag)

#normalizing numeric data between 0 and 1
normalize <- function(x){
	return ((x - min(x)) / (max(x) - min(x)))
}

train_features <- as.data.frame(lapply(train_data[1:41], normalize))
test_features <- as.data.frame(lapply(test_data[1:41], normalize))

#transfer NA to 0
train_features[is.na(train_features)] <- 0
test_features[is.na(test_features)] <- 0

#use the knn() function in the class package
install.packages("class")
library(class)

#knn() function returns a factor vector of predicted labels for each of
#the examples in the test dataset
test_predict <- knn(train=train_features, test=test_features, cl=train_labels, k=3)

#evaluating model performance
install.packages("gmodels")
library(gmodels)
CrossTable(x=test_labels, y=test_predict, prop.chisq=FALSE)

#sub-setting dataset
set.seed(15532)
n_train = nrow(train_data)
n_test = nrow(test_data)
train_rand <- train_data[order(runif(n_train)), ]
train_data <- train_rand[1:1000,]
test_rand <- test_data[order(runif(n_test)), ]
test_data <- test_rand[1:1000, ]
