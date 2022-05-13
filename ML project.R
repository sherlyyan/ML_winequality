rm(list=ls())
############################
#### install package  ######
############################
installIfAbsentAndLoad <- function(neededVector) {
  for(thispackage in neededVector) {
    if( ! require(thispackage, character.only = T) )
    { install.packages(thispackage)}
    require(thispackage, character.only = T)
  }
}
needed <- c("class")  
installIfAbsentAndLoad(needed)
############################
#### read the data  ########
############################
mydata <- read.csv("winequality-white.csv")
set.seed(5072)
n <- nrow(mydata)
mydata[1:6,]
############################
#### scale predictors  #####
############################
mydata[,-12] <- scale(mydata[,-12])
mydata[1:6,]
############################
#### split the set  ########
############################
trainprop <- 0.75
validateprop <- 0.15
n <- nrow(mydata)
train  <-  sample(n, trainprop * n)
validate  <-  sample(setdiff(1:n, train), validateprop * n) 
test <- setdiff(setdiff(1:n, train), validate)
trainset <- mydata[train,]
validateset <- mydata[validate,]
testset <- mydata[test,]

# create x,y
train.x <- as.data.frame(trainset[,-12])   
train.y <- as.factor(trainset$quality)
validate.x <- as.data.frame(validateset[,-12])
validate.y <- as.factor(validateset$quality)
test.x <- as.data.frame(testset[,-12])
test.y <- as.factor(testset$quality)
levels(test.y) <- c(3,4,5,6,7,8,9)   
str(test.x)      # x's structured as dataframe
str(test.y)      # y as factor with 7 levels indicating wine quality

############################
#### validate for best k ###
############################
biggestk <- 19
kset <- seq(1, biggestk, by = 2)
validate.errors <- rep(0, length(kset))
train.errors <- rep(0, length(kset))           
for(k in kset) {
  knn.model <- knn(train.x, validate.x, train.y, k = k)
  validate.errors[k%/%2 + 1] <- sum(validate.y != knn.model)/length(validate.y)
  knn.model <- knn(train.x, train.x, train.y,k = k)
  train.errors[k%/%2 + 1] <- sum(train.y != knn.model)/length(train.y)
}

#which.min(validateMSEs)
print(paste("Best validate k is",kset[which.min(validate.errors)],"with an error rate of",validate.errors[which.min(validate.errors)]))
print(paste("Best training k is",kset[which.min(train.errors)],"with an error rate of",train.errors[which.min(train.errors)]))


############################
## Compute test error rate #
############################
knn.model <- knn(train.x, test.x, train.y, k = kset[which.min(validate.errors)])
sum(test.y != knn.model)/length(test.y)   #error rate

