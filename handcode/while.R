mymat <- matrix(nrow=30, ncol=30)

for(i in 1:dim(mymat)[1]){
  for(j in 1:dim(mymat)[2]){
    mymat[i, j] = i*j 
  }
}

mymat[1:10, 1:10]