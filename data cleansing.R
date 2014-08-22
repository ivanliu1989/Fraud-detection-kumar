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

## Percent of NA in VAL / QUANT by ID and PROD
nnasQp <- tapply(sales$Quant,list(sales$Prod),function(x)sum(is.na(x)))
propNAsQp <- nnasQp/table(sales$Prod)
propNAsQp[order(propNAsQp,decreasing=T)[1:10]]

sales <- sales[!sales$Prod %in% c('p2442','p2443'),]

nlevels(sales$Prod)
sales$Prod <- factor(sales$Prod)
nlevels(sales$Prod)

nnasQs <- tapply(sales$Quant,list(sales$ID),function(x)sum(is.na(x)))
propNAsQs <- nnasQs/table(sales$ID)
propNAsQs[order(propNAsQs,decreasing = T)[1:10]]

nnasVp <- tapply(sales$Val,list(sales$Prod),function(x)sum(is.na(x)))
propNAsVp<-nnasVp/table(sales$Prod)
propNAsVp[order(propNAsVp,decreasing = T)[1:10]]

nnasVs <- tapply(sales$Val,list(sales$ID),function(x)sum(is.na(x)))
propNAsVs<-nnasVs/table(sales$ID)
propNAsVs[order(propNAsVs,decreasing = T)[1:10]]
##178