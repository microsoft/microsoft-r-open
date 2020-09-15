## allow backticks in rpart.matrix: see
## https://stat.ethz.ch/pipermail/r-help/2012-May/314081.html

set.seed(10)
library(rpart)
Iris <- iris
names(Iris) <- sub(".", " ", names(iris), fixed=TRUE)
rpart(Species ~ `Sepal Length`, data = Iris)
