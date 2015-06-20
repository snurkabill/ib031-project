library(MASS)
data(iris)

index <- 1:nrow(iris)
testIndex <- sample(index, trunc(length(index)/4))
testSet <- iris[testIndex,]
trainSet <- iris[-testIndex,]

#-------------------------------------------------------------------------------------------------------

ldaFit <- lda(Species~., data=trainSet)
print(summary(ldaFit))
predictions <- predict(ldaFit, testSet[,1:4])$class
print(table(predictions, testSet$Species))
print(mean(predictions == testSet[,ncol(testSet)]) * 100)

#-------------------------------------------------------------------------------------------------------

fit <- vglm(Species~., family=multinomial, data=trainSet)
print(summary(fit))
# make predictions
probabilities <- predict(fit, testSet[,1:4], type="response")
predictions <- apply(probabilities, 1, which.max)

for(i in 1:3) {
  predictions[which(predictions==i)] <- levels(testSet$Species)[i]
}
# summarize accuracy
print(table(predictions, testSet$Species))
print(mean(predictions == testSet[,ncol(testSet)]) * 100)

