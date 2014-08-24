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

notF <- which(sales$Insp != 'fraud')
globalStats <- tapply(sales$Uprice[notF],list(Prod=sales$Prod[notF]),
                      function(x){
                          bp<-boxplot.stats(x)$stats
                          c(median=bp[3],iqr=bp[4]-bp[2])
                      })
globalStats<-matrix(unlist(globalStats),length(globalStats),2,byrow=T,
                    dimnames=list(names(globalStats),c('median','iqr')))
globalStats[which(globalStats[,'iqr']==0),'iqr']<-globalStats[which(globalStats[,'iqr']==0),'median']

## holdOut
ho.BPrule <- function(form,train,test,...){
    res<-BPrule(train,test)
    structure(evalOutlierRanking(test,res$rankOrder,...),
              itInfo=list(preds=res$rankScore,trues=ifelse(test$Insp=='fraud',1,0)
                          )
              )
    }
bp.res <- holdOut(learner('ho.BPrule',
                          pars=list(Threshold=0.1, statsProds=globalStats)),
                  dataset(Insp~.,sales),
                  hldSettings(3,0.3,1234,T),
                  itsInfo=T)
summary(bp.res)

par(mfrow=c(1,2))
info <- attr(bp.res,'itsInfo')
PTs.bp <- aperm(array(unlist(info),dim=c(length(info[[1]]),2,3)),c(1,3,2))
PRcurve(PTs.bp[,,1],PTs.bp[,,2],main='PR curve',avg='vertical')
CRchart(PTs.bp[,,1],PTs.bp[,,2],main='Cumulative Recall curve',avg='vertical')

## Local Outlier Factors
ho.LOF <-function(form, train,test,k,...){
    ntr<-nrow(train)
    all <- rbind(train,test)
    N<-nrow(all)
    ups<-split(all$Uprice,all$Prod)
    r<-list(length=ups)
    for(u in seq(along=ups))
        r[[u]]<-if(NROW(ups[[u]])>3)
            lofactor(ups[[u]],min(k,NROW(ups[[u]]) %/% 2))
    else if (NROW(ups[[u]])) rep(0,NROW(ups[[u]]))
    else NULL
    all$lof <- vector(length=N)
    split(all$lof,all$Prod)<-r
    all$lof[which(!(is.infinite(all$lof)|is.nan(all$lof)))]<-
        SoftMax(all$lof[which(!(is.infinite(all$lof)|is.nan(all$lof)))])
    structure(evalOutlierRanking(test,order(all[(ntr+1):N,'lof'],decreasing=T),...),
              itInfo=list(preds=all[(ntr+1):N,'lof'],
                          trues=ifelse(test$Insp=='fraud',1,0)))
}