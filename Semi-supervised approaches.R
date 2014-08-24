# a test based on iris dataset
library(DMwR)
library(e1071)
data(iris)
idx <- sample(150,100)
tr <- iris[idx,]
ts <- iris[-idx,]
nb <- naiveBayes(Species~.,tr)
table(predict(nb,ts),ts$Species)
trST <- tr
nas <- sample(100,90)
trST[nas,'Species']<-NA
func <- function(m,d){
    p<-predict(m,d,type='raw')
    data.frame(cl=colnames(p)[apply(p,1,which.max)],
               p=apply(p,1,max))
}
nbSTbase <- naiveBayes(Species~.,trST[-nas,])
table(predict(nbSTbase,ts),ts$Species)
nbST <- SelfTrain(Species~.,trST,learner('naiveBayes',list()),'func')
table(predict(nbST,ts),ts$Species)

# implement to our dataset