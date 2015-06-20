
#-----------------------------------------------------------------------------------------------------
data <- read.csv("~/Documents/ib031_project/winequality-red.csv", sep=";")
index <- 1:nrow(data)
testIndex <- sample(index, trunc(length(index)/2))
testSet <- data[testIndex, ]
trainSet <- data[-testIndex, ]

#-----------------------------------------------------------------------------------------------------

linearModel <- lm(quality ~ ., data = trainSet)
print(summary(linearModel))

linearModelPrediction <- predict(linearModel, testSet[, -12])

power = (linearModelPrediction - testSet[, 12])^2;
linearError = sum(power)/ length(power) 

#-----------------------------------------------------------------------------------------------------

myForest <- randomForest(quality ~ ., data = trainSet, method="anova")

forestPrediction <- predict(myForest, testSet[, -12])
print(myForest)
power = (forestPrediction - testSet[, 12])^2;
forestError = sum(power)/ length(power)

results = c(linearError, forestError)
View(results)

