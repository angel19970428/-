###創3日新高買進，創3日新低賣出while迴圈與for迴圈寫法
rm(list=ls())#清除工作環境
library(quantmod)
STK=na.omit(get(
  getSymbols("AMZN",from = "2007-01-01",to = "2019-10-31" ,src = "yahoo")))
STK=as.data.frame(STK)
par(mfrow=c(2,1))
#####while迴圈#####
m=3
STK$PL1<-0###PL--->Paper Loss 帳面損益
while(m<nrow(STK)){
  if(STK$AMZN.Close[m]>=max(STK$AMZN.High[(m-3):(m-1)])){
    Buy=STK$AMZN.Close[m]
    while(STK$AMZN.Close[m]>min(STK$AMZN.Low[(m-3):(m-1)]) && m<nrow(STK)){
      m=m+1}
    Sell=STK$AMZN.Close[m]
    STK$PL1[m]=Sell-Buy
  }
  m=m+1
}
sum(STK$PL1)
plot(cumsum(STK$PL1),type="l",col="red",main="while 迴圈")
#####for迴圈#####
STK$PL2<-0###PL--->Paper Loss 帳面損益
hasBuy=FALSE
for(m in 3:nrow(STK)){
  if(!hasBuy){
    if(STK$AMZN.Close[m]>=max(STK$AMZN.High[(m-3):(m-1)])){
      Buy=STK$AMZN.Close[m]
      hasBuy=TRUE
    }
  }else{
    if(STK$AMZN.Close[m]<min(STK$AMZN.Low[(m-3):(m-1)]) || m==nrow(STK)){
      Sell=STK$AMZN.Close[m]
      hasBuy=FALSE
      STK$PL2[m]=Sell-Buy
    }
  }
  
}
sum(STK$PL2)
plot(cumsum(STK$PL2),type="l",col="red",main="for 迴圈")