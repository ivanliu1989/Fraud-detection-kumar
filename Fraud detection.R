## Load data
require(DMwR)
data(sales)
head(sales)
summary(sales)
nlevels(sales$ID)
nlevels(sales$Prod)
sum(is.na(sales$Quant)& is.na(sales$Val))
table(sales$Insp)/nrow(sales)*100

totS <- table(sales$ID)
totP <- table(sales$Prod)
par(mfcol = c(1,2))
barplot(totS, main="Transactions per salespeople", name.arg='',
        xlab='Salespeople',ylab='Amount')
barplot(totP, main='Transactions per product', names.arg='',
        xlab='Products',ylab='Amount')

