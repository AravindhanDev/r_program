diabetesData <- read.csv(file = "diabetes.csv", header=TRUE, sep=",")
insulin <- diabetesData$Insulin
age <- diabetesData$Age
plot(insulin, age, xlab="Insulin levels", ylab="Age", main="Relationship between insulin levels and age")

diabetesModel <- lm(age ~ insulin, data = diabetesData)

plot(insulin, age, xlab="Insulin levels", ylab="Age", main="Relationship between insulin levels and age")
abline(diabetesModel, col="red")

predictedAge <- function(insulinInput) {
  value <- predict(diabetesModel, data.frame(insulin = insulinInput))
}

for (x in 1:100) {
  predicted <- predictedAge(insulin[x])
  cat("Predicted age of patient", x,"with insulin levels", insulin[x], "is", round(predicted, 2), "\n")
}
