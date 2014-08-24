#imbalanced sample
data(iris)
data <- iris[,c(1,2,5)]
data$Species <- factor(ifelse(data$Species == 'setosa','rare','common'))
newData <- SMOTE(Species~.,data,perc.over=600)
table(newData$Species)
table(data$Species)
