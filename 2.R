mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

data <- read.csv("uci_diabetes.csv")
obesity = data$Obesity
obesity_numeric <- ifelse(obesity == "Yes", 1, 0)
summary(obesity_numeric)
var(obesity_numeric)
sd(obesity_numeric)
library(moments)
plot(density(obesity_numeric))
skewness(obesity_numeric)
kurtosis(obesity_numeric)
print(mean(obesity_numeric))
print(median(obesity_numeric))
print(mode(obesity_numeric))
