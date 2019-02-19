#installing package
install.packages("data.table")
#importing package
library(data.table)
#retrieving data
mydata <- fread('http://www.stats.ox.ac.uk/pub/datasets/csb/97532.dat')
#printing data top records
head(mydata)
