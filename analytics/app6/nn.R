#Author: fullarray
#"Profit has a thousand parents, bankruptcy is an orphan"

#NN
#Transfer symbolic value to continous value
train_data$protocol_type <- as.numeric(train_data$protocol_type)
train_data$service <- as.numeric(train_data$service)
train_data$flag <- as.numeric(train_data$flag)
test_data$protocol_type <- as.numeric(test_data$protocol_type)
test_data$service <- as.numeric(test_data$service)
test_data$flag <- as.numeric(test_data$flag)

#Normalizing numeric data between 0 and 1
normalize <- function(x){
	return((x - min(x)) /  (max(x) - min(x)))
}
train_data <- as.data.frame(lapply(train_data[1:42], normalize))
test_data <- as.data.frame(lapply(test_data[1:42], normalize))

#Transfer NA to 0
train_data[is.na(train_data)] <- 0
test_data[is.na(test_data)] <- 0

#Training the feedforward network with single hidden node
install.packages("neuralnet", repos='http://cran.rstudio.com/')
library(neuralnet)
concrete_model <- neuralnet(labels ~ duration+protocol_type+service+flag+...+dst_host_srv_rerror_rate, data=train_data)

#Plot the model
plot(concrete_model)

#Transfer NA to 0
model_results <- compute(concrete_model, test_data[1:41])

#General predictions on the testing dataset
predicted_labels <- model_results$net.result

#Obtain a correlation between two numeric vectors
cor(predicted_labels, test_data$labels)
