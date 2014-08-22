totS <- table(ID)
totP <- table(Prod)
nas <- sales[which(is.na(Quant)&is.na(Val)),c('ID','Prod')]
propS <- 100*table(nas$ID)/totS
propS[order(propS,decreasing=T)[1:10]]

propP <- 100*table(nas$Prod)/totP
propP[order(propP,decreasing=T)[1:10]]

## join small groups together
detach(sales)
sales <- sales[-which(is.na(sales$Quant)&is.na(sales$Val)),]
