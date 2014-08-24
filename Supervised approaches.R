## Naive Bayes
nb <- function(train,test){
    require(e1071,quietly=T)
    sup<-which(train$Insp !='unkn')
    data<-train[sup,c('ID','Prod','Uprice','Insp')]
    data$Insp <- factor(data$Insp,levels=c('ok','fraud'))
    model <- naiveBayes(Insp~.,data)
    preds <- predict(model,test[,c('ID','Prod','Uprice','Insp')],type='raw')
    return(list(rankOrder=order(preds[,'fraud'],decreasing=T),rankScore=preds[,'fraud']))
}

ho.nb<-function(form,train,test,...){
    res <- nb(train,test)
    structure(evalOutlierRanking(test,res$rankOrder,...),
              itInfo=list(preds=res$rankScore,trues=ifelse(test$Insp=='fraud',1,0)))
}

nb.res <- holdOut(learner('ho.nb',
                          pars=list(Threshold=.1,statsProds=globalStats)),
                  dataset(Insp~.,sales),
                  hldSettings(3,0.3,1234,T),
                  itsInfo=TRUE)