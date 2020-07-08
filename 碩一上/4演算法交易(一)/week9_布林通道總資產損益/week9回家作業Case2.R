####與作業Case1的差別在於是買一股，不是一張，所以資金也下調1000倍
rm(list=ls())#清除工作環境
options(scipen = 999)#不要科學符號
library(quantmod)
library(ggplot2)
library(ggthemes)
par(mfrow=c(2,3))
STK=na.omit(get(
  getSymbols("AMZN",from = "2007-01-01",to = "2019-11-07" ,src = "yahoo")))
STK=as.data.frame(STK)
#BB--->Bollinger Bands布林通道
BB=as.data.frame(BBands(STK$AMZN.Close))
#Close-->收盤價、Down--->下軌、Avg--->中軌、Up--->上軌
data<-data.frame(Close=STK$AMZN.Close,
                 Down=BB$dn,
                 Avg=BB$mavg,
                 Up=BB$up)
#data欄位新增
data$Sign1<-data$Close>data$Up
#收盤價是否大於上緣
#0:否、1:是
data$Sign2<-data$Close>data$Down
#收盤價是否大於下緣
#0:否、1:是
data$PL1<-0#損益累計1
data$Cash1<-10000#現金累計1、初始1萬
data$Capital1<-10000#資產累計1、初始1萬
data$PL2<-0#損益累計2
data$Cash2<-0#現金累計2、初始0
data$Capital2<-0#資產累計2、初始0
#####PL1 && Capital1#####
hasBuy1=FALSE
STKValue1=0
for(m in 21:nrow(data)){
  data$Cash1[m]=data$Cash1[m-1]
  data$Capital1[m]=data$Capital1[m-1]
  if(!hasBuy1){
    #若收盤價突破上緣買進
    if(data$Sign1[m-1]==0 && data$Sign1[m]==1){
      Buy1=data$Close[m]
      data$Cash1[m]=data$Cash1[m-1]-Buy1
      STKValue1=Buy1
      hasBuy1=TRUE
    }
  }else{
    STKValue1=data$Close[m]
    #若收盤價跌破上緣賣出
    if(data$Sign1[m-1]==1 && data$Sign1[m]==0){
      Sell1=data$Close[m]
      data$PL1[m]=Sell1-Buy1
      data$Cash1[m]=data$Cash1[m-1]+Sell1
      STKValue1=0
      hasBuy1=FALSE
    }
    data$Capital1[m]=data$Cash1[m]+STKValue1
  }
}
#####PL2 && Capital2#####
hasBuy2<-FALSE
STKValue2=0
for(m in 21:nrow(data)){
  data$Cash2[m]=data$Cash2[m-1]
  data$Capital2[m]=data$Capital2[m-1]
  if(!hasBuy2){
    #若收盤價跌破下緣賣空
    if(data$Sign2[m-1]==1 && data$Sign2[m]==0){
      Sell2=data$Close[m]
      data$Cash2[m]=data$Cash2[m-1]+Sell2
      hasBuy2=TRUE
    }
  }else{
    STKValue2=data$Close[m]
    #若收盤價突破下緣平倉
    if(data$Sign2[m-1]==0 && data$Sign2[m]==1){
      Buy2=data$Close[m]
      data$PL2[m]=Sell2-Buy2
      STKValue2=0
      data$Cash2[m]=data$Cash2[m-1]-Buy2
      hasBuy2=FALSE
    }
    data$Capital2[m]=data$Cash2[m]-STKValue2
  }
}
#####PL3 && Capital3#####
data$PL3<-data$PL1+data$PL2
data$Capital3<-data$Capital1+data$Capital2
#####六張圖輸出#####
plot(cumsum(data$PL1),type="l",lwd=2,col="red",main="損益累計1")
plot(cumsum(data$PL2),type="l",lwd=2,col="green",main="損益累計2")
plot(cumsum(data$PL3),type="l",lwd=2,col="blue",main="損益累計3")
plot(data$Capital1,type="l",lwd=2,col="red",main="資產累計1(初始1萬)")
plot(data$Capital2,type="l",lwd=2,col="green",main="資產累計2(初始0)")
plot(data$Capital3,type="l",lwd=2,col="blue",main="資產累計3(初始1萬)")
