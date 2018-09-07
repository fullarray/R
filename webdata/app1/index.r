install.packages("data.table")
library(data.table)
mydata <- fread('http://www.stats.ox.ac.uk/pub/datasets/csb/97532.dat')
head(mydata)