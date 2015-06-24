library(MASS)

data  <- read.csv("~/Documents/ib031_project/chess-data")

index <- 1:nrow(data)
testIndex <- sample(index, trunc(length(index)/3))
testSet <- data[testIndex,]
trainSet <- data[-testIndex,]

#-------------------------------------------------------------------------------------------------------

ldaFit <- lda(won~., data=trainSet)
print(summary(ldaFit))
predictions <- predict(ldaFit, testSet[1:36])$class
print(table(predictions, testSet$won))
print(mean(predictions == testSet[,ncol(testSet)]) * 100)

#-------------------------------------------------------------------------------------------------------

fit <- vglm(won~., family=multinomial, data=trainSet)
print(summary(fit))
# make predictions
probabilities <- predict(fit, testSet[1:36], type="response")
predictions <- apply(probabilities, 1, which.max)
for(i in 1:2) {
  predictions[which(predictions==i)] <- levels(testSet$won)[i]
}
# summarize accuracy
print(table(predictions, testSet$won))
print(mean(predictions == testSet[,ncol(testSet)]) * 100)

