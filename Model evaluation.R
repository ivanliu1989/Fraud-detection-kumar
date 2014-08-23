## ROCR - Precision/Recall curves
library(ROCR)
par(mfcol = c(1,2))
data(ROCR.simple)
pred <- prediction(ROCR.simple$predictions, ROCR.simple$labels)
perf <- performance(pred,'prec','rec')
plot(perf)

## Interpolated precision
PRcurve <- function(preds,trues,...){
    require(ROCR, quietly=T)
    pd <- prediction(preds,trues)
    pf<-performance(pd,'prec','rec')
    pf@y.values<- lapply(pf@y.values,function(x)rev(cummax(rev(x))))
    plot(pf,...)
}
PRcurve(ROCR.simple$predictions, ROCR.simple$labels)
