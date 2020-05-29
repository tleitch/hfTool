# getwd()
# setwd("/home/qsheng3/HedgeFundApp/dataProcess")
# install.packages("urca")
# install.packages("egcm")
# library(urca)
library(egcm)
getSymbols("AAPL")
getSymbols("GE")

# get nasdaq 100 first
# form pairs C100 2 = 4950 pairs
# train 13-1-1 ~ 18-3-31
# backtest 18-4-1 ~ 2020-4-1
# record pairs' name, p value and stat.
# sort.

dataNas <- read.csv("priceNas100.csv", header=T)
test = egcm(dataNas[200:290,55:56])
print(test)
plot(test)
is.cointegrated(test)
test

test = allpairs.egcm(dataNas[755:2074,1:90])
sum(test$is.cointegrated)
dataNas[755:756,1:2]





dataSP <- read.csv("priceSP500.csv", header=T)
test2 = allpairs.egcm(dataSP[755:2074,1:471])
sum(test2$is.cointegrated)


write.csv(test2,file="ResSP500.csv",quote=F,row.names = F)


resSP = test2
resSPTrue = resSP[which(resSP$is.cointegrated),]
write.csv(resSPTrue,file="ResSP500True.csv",quote=F,row.names = F)










