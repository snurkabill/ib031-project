library(MASS)

data  <- read.csv("~/Documents/ib031_project/wdbc.data", header=FALSE)

index <- 1:nrow(data)
testIndex <- sample(index, trunc(length(index)/3))
testSet <- data[testIndex,]
trainSet <- data[-testIndex,]

#-------------------------------------------------------------------------------------------------------

ldaFit <- lda(V31~., data=trainSet)
print(summary(ldaFit))
predictions <- predict(ldaFit, testSet[,1:30])$class
print(table(predictions, testSet$V31))
print(mean(predictions == testSet[,ncol(testSet)]) * 100)

#-------------------------------------------------------------------------------------------------------

fit <- vglm(V31~., family=multinomial, data=trainSet)
print(summary(fit))
# make predictions
probabilities <- predict(fit, testSet[,1:30], type="response")
predictions <- apply(probabilities, 1, which.max)

for(i in 1:2) {
  predictions[which(predictions==i)] <- levels(testSet$V31)[i]
}
# summarize accuracy
print(table(predictions, testSet$V31))
print(mean(predictions == testSet[,ncol(testSet)]) * 100)

