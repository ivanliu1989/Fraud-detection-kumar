## ROCR - Precision/Recall curves
library(ROCR)
par(mfcol = c(1,2))
data(ROCR.simple)
pred <- prediction(ROCR.simple$predictions, ROCR.simple$labels)
perf <- performance(pred,'prec','rec')
plot(perf)

## 