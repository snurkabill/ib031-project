data <- read.table("~/Documents/ib031_project/data.txt", quote="\"", comment.char="")

index <- 1:nrow(data)
testIndex <- sample(index, trunc(length(index)/2))
testSet <- data[testIndex, ]
trainSet <- data[-testIndex, ]

#-----------------------------------------------------------------------------------------------------

linearModel <- lm(V18 ~ ., data = trainSet)
print(summary(linearModel))

linearModelPrediction <- predict(linearModel, testSet[, -18])

power = (linearModelPrediction - testSet[, 18])^2;
linearError = sum(power)/ length(power) 

#-----------------------------------------------------------------------------------------------------

myForest <- randomForest(V18 ~ ., data = trainSet, method="anova")

forestPrediction <- predict(myForest, testSet[, -18])
print(myForest)
power = (forestPrediction - testSet[, 18])^2;
forestError = sum(power)/ length(power)

results = c(linearError, forestError)
View(results)

