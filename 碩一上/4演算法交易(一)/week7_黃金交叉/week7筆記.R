rm(list=ls())#清除工作環境
library(quantmod)
STK=na.omit(get(
  getSymbols("AMZN",from = "2007-01-01",to = "2019-10-24" ,src = "yahoo")))
# chartSeries(STK)
# addSMA(5)#5日收盤價均線
##############課堂練習1  2330.TW(人工)
# 248.5(8/21賣)-254.5(8/26買)=-6
# 293(最後一天)-252(8/28買)=41
# 總共賺41-6=35
###############黃金交叉(收盤價向上穿越20MA買進，收盤價跌破20MA賣出)
# head(SMA(Cl(STK),20))--->前幾筆20日均線的值
# head(cbind(Cl(STK),SMA(Cl(STK),20)))
# 收盤價是否有大於五日均線: 1大於 、 -1小於
# row<-sign(Cl(STK)-SMA(Cl(STK),20))
# sign()--->判斷正負數
# 收盤價是否有大於五日均線: TRUE:1 、 FALSE:0
row1<-Cl(STK)>SMA(Cl(STK),20)
data<-as.data.frame(cbind(Cl(STK),SMA(Cl(STK),20),row1))
colnames(data)<-c("Close","MA20","Sign")
#############################課堂作業二 while迴圈
par(mfrow=c(2,1))
m=20+1
result=0
PL=c()
while(m<=nrow(data)){
  if(data$Sign[m-1]==0 && data$Sign[m]==1){
    Buy=data$Close[m]
    while(data$Sign[m]==1 && m<nrow(data)){
      m=m+1}
    Sell=data$Close[m]
    result=Sell-Buy
    PL=c(PL,result)
  }
  m=m+1
}
sum(PL)
plot(cumsum(PL),type="l",col="red",main="while迴圈")
################################回家作業
Buy=c()
Sell=c()
PL2=c()
count<-0
times<-0
for(i in 21:nrow(data)){
  if(data$Sign[i-1]==1 && data$Sign[i]==0 && count<1){
    count<-count+1
    next()}
  if(!(data$Sign[i]==data$Sign[i-1])){
    if(times%%2==0 && times>=0){Buy=c(Buy,data$Close[i])}
    if(times%%2==1 && times>=0){Sell=c(Sell,data$Close[i])}
    times=times+1
  }
}
if(length(Buy)!=length(Sell)){
  Sell=c(Sell,data$Close[nrow(data)])
}
PL2=Sell-Buy
sum(PL2)
plot(cumsum(PL2),type="l",col="red",main="for迴圈")
