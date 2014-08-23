## Normalized distance to typical unit price
## The modified box plot rule
BPrule <- function(train,test){
    notF <- which(train$Insp != 'fraud')
    ms <- tapply(train$Uprice[notF],list(Prod=train$Prod[notF]),
                 function(x){
                     bp <- boxplot.stats(x)$stats
                     c(median=bp[3],iqr=bp[4]-bp[2])
                 })
    ms <- matrix(unlist(ms), length(ms),2,byrow=T,
                 dimnames=list(names(ms),c('median','iqr')))
    ms[which(ms[,'iqr']==0),'iqr']<-ms[which(ms[,'iqr']==0),'median']
    ORscore <- abs(test$Uprice-ms[test$Prod,'median'])/ms[test$Prod,'iqr']
    return(list(rankOrder=order(ORscore,decreasing=T),rankScore=ORscore))
}
