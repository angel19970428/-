###黃金交叉(收盤價向上穿越20MA買進，收盤價跌破20MA賣出)作業
rm(list=ls())#清除工作環境
library(quantmod)
par(mfrow=c(2,1))#2row、1col
STK=na.omit(get(
  getSymbols("AMZN",from = "2007-01-01",to = "2019-10-24" ,src = "yahoo")))
## 收盤價是否有大於五日均線: TRUE:1 、 FALSE:0
row1<-Cl(STK)>SMA(Cl(STK),20)
data<-as.data.frame(cbind(Cl(STK),SMA(Cl(STK),20),row1))
colnames(data)<-c("Close","MA20","Sign")
#####while迴圈#####
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

#####for迴圈#####
#colnames(data)<-c("Close","MA20","Sign")
Buy=c()
Sell=c()
PL2=c()
count<-0
times<-1
for(i in 21:nrow(data)){
  ###如果一開始是1變0就不理他==>不符合條件
  if(data$Sign[i-1]==1 && data$Sign[i]==0 && count<1){
    count<-count+1
    next()}
  ###如果值(0->1或1->0)有變化就會進if
  ###奇數次進入Buy向量、偶數次進入Sell向量
  if(!(data$Sign[i]==data$Sign[i-1])){
    if(times%%2==1){Buy=c(Buy,data$Close[i])}
    if(times%%2==0){Sell=c(Sell,data$Close[i])}
    times=times+1
  }
}
###如果Buy的長度不等於Sell
###即最後一次買進後，沒有賣出
###我們用最後一天的收盤價當賣出點
if(length(Buy)!=length(Sell)){
  Sell=c(Sell,data$Close[nrow(data)])
}
PL2=Sell-Buy
sum(PL2)
plot(cumsum(PL2),type="l",col="red",main="for迴圈")