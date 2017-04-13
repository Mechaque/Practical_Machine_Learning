                               
              
## getting and loading the data



set.seed(12345)
trainUrl <-"https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv"
testUrl <- "https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv"
training <- read.csv(url(trainUrl), na.strings=c("NA","#DIV/0!",""))
testing <- read.csv(url(testUrl), na.strings=c("NA","#DIV/0!",""))



## Exploratory Data

dim(training)
dim(testing)
merged <- match(FALSE, (names(testing) == names(training)))
names(training)[merged]
names(testing)[merged]
plot(training$classe, xlab = "Activity class", ylab = "count", main = "Exercises", col=c("green","yellow","black","red","blue"))

## Cleaning Data

natraining <- sum(is.na(training))
rateNaTraining <- sum(is.na(training))/(dim(training)[1]*dim(training)[2])
natesting <- sum(is.na(testing))
rateNaTesting <- sum(is.na(testing))/(dim(testing)[1]*dim(testing)[2])

## Removing NA

sum(colSums(is.na(training)) == nrow(training))
sum(colSums(is.na(testing)) == nrow(testing))
cleanedCol <- (colSums(is.na(training)) < nrow(training)) & (colSums(is.na(testing)) < nrow(testing))
training <- training[,cleanedCol]
testing <- testing[,cleanedCol]
dim(training)
dim(testing)

## validation
#we are going to compare for the below algorithms
# recursive partitioning
#Random Forest
#Linear Discriminate Analysis
#Naive Bayes

set.seed(123)
algorithms <- c("rpart", "rf", "gbm","lda","nb")
k <- length(algorithms)
folds <- createFolds(y=training$classe, k=k, list=TRUE)
sapply(folds, length)

## we have r dim(training)[2]-1 variables to predict with in the data set.

# empty vector to enter models
models <- vector(length = k)
# empty vector for acuracy 
accuracy <- vector(length = k)
training <- read.csv(url(trainUrl), na.strings=c("NA","#DIV/0!",""))
for(i in 1:k){
  trainingSub <- training[-folds[[i]],]
  testingSub <-  training[-folds[[i]],]
algorithms[i]
modFit <- training (as.factor(classe)~. ,data=trainingSub, method= algorithms[i], preProcess = "pca",na.action = na.omit)
models[i] <-modFit
pred <- predict(modFit, testingSub)
accuracy[i] <- complicatedMatrix(pred, testingSub$classe)$overall["accuracy"]
}

## check the accuracy to see wich has largest one

cbind(algorithms, accuracy)
max(accuracy)
models[[match(max(accuracy), accuracy)]]

## Given the results above, we will use the random forest model on the test data


i = match(max(accuracy), accuracy)
trainingSub <- training[-folds[[i]], ]
finalModel <- training(as.factor(classe) ~ ., data=trainingSub, method=algorithms[i], preProcess = "pca", na.action = na.omit)

# select the model with highest accuracy

predictionFunction <- models[match(max(accuracy), accuracy)]
# predict using model
result <- predict(predictionFunction, testing)

# calculate the sample error

1 - max(accuracy)

# The resulting prediction for the 20 testing values are.

result



