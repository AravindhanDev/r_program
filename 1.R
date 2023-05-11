mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

data <- read.csv("pima_diabetes.csv")
bloodPressure = data$BloodPressure
summary(bloodPressure)
var(bloodPressure)
sd(bloodPressure)
plot(density(bloodPressure))
library(moments)
skewness(bloodPressure)
kurtosis(bloodPressure)
print(mean(bloodPressure))
print(median(bloodPressure))
print(mode(bloodPressure))
gm <- exp(mean(log(bloodPressure)))
range <- max(bloodPressure) - min(bloodPressure)
print(paste("Geometric mean:", gm))
print(paste("Range:", range))
hist(bloodPressure)
iqr <- IQR(bloodPressure)
print(paste("Interquartile range:", iqr))