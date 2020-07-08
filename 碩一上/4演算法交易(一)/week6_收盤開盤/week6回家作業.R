rm(list=ls())#清除工作環境
par(mfrow=c(3,1))
library(quantmod)
AMZN=get(getSymbols("AMZN",from = "2007-01-01",to = "2018-09-30" ,src = "yahoo"))
Stocks=to.monthly(AMZN,indexAt = 'firstof')
Stocks=as.data.frame(Stocks)
###開高1%以內買進
Stocks$profit<-0
for(m in 2:nrow(Stocks)){
  if(Stocks$AMZN.Open[m]>Stocks$AMZN.Close[m-1]&&
     Stocks$AMZN.Open[m]<Stocks$AMZN.Close[m-1]*1.01){
    Stocks$profit[m]=Stocks$AMZN.Close[m]-Stocks$AMZN.Open[m]
  }
}
sum(Stocks$profit)
plot(cumsum(Stocks$profit),type="l",col="red",lwd=2,main="開高1%以內買進,收盤賣出")
###開高4%以內買進
Stocks$profit<-0
for(m in 2:nrow(Stocks)){
  if(Stocks$AMZN.Open[m]>Stocks$AMZN.Close[m-1]&&
     Stocks$AMZN.Open[m]<Stocks$AMZN.Close[m-1]*1.04){
    Stocks$profit[m]=Stocks$AMZN.Close[m]-Stocks$AMZN.Open[m]
  }
}
sum(Stocks$profit)
plot(cumsum(Stocks$profit),type="l",col="red",lwd=2,main="開高4%以內買進,收盤賣出")
###開高10%以內買進
Stocks$profit<-0
for(m in 2:nrow(Stocks)){
  if(Stocks$AMZN.Open[m]>Stocks$AMZN.Close[m-1]&&
     Stocks$AMZN.Open[m]<Stocks$AMZN.Close[m-1]*1.1){
    Stocks$profit[m]=Stocks$AMZN.Close[m]-Stocks$AMZN.Open[m]
  }
}
sum(Stocks$profit)
plot(cumsum(Stocks$profit),type="l",col="red",lwd=2,main="開高10%以內買進,收盤賣出")

###總結4%跟10%沒甚麼太大差異，1%也只有些許差異







