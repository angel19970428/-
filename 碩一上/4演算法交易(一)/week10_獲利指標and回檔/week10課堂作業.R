###創3日新高買進，創3日新低賣出
rm(list=ls())#清除工作環境
library(quantmod)
STK=na.omit(get(
  getSymbols("AMZN",from = "2007-01-01",to = "2019-10-31" ,src = "yahoo")))
STK=as.data.frame(STK)
STK$PL<-0###PL--->Paper Loss 帳面損益
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
      STK$PL[m]=Sell-Buy
    }
  }
}
sum(STK$PL)
plot(cumsum(STK$PL),type="l",col="red",main="創3日新高買進，創3日新低賣出")
#####前面都是week8回家作業Case1
#####後面開始才是week10課堂筆記
par(mfrow=c(1,2))
#####################獲利指標
PL=STK$PL[STK$PL!=0]
plot(cumsum(PL),type="l",lwd=2,col="red")
###勝率
win=length(PL[PL>0])
total=length(PL)
WinRate=win/total*100
paste0("WinRate=",round(WinRate,2),"%")
###賺賠比
meanEarn=mean(PL[PL>0])
meanLoss=mean(PL[PL<0])
Odds=abs(meanEarn/meanLoss)
paste0("Odds=",round(Odds,2))
###獲利因子(profit factor)
#賺的和/賠的和
PF=sum(PL[PL>0])/abs(sum(PL[PL<0]))
paste0("Profit factor=",round(PF,2))
#DrawDown(回檔)--->這裡是絕對回檔
DD<-cumsum(PL)-cummax(cumsum(PL))
###獲利/MaxDrawDown
sum(PL)/abs(min(DD))
###最大的5個DDD
tail(sort(diff(which(DD==0))-1),5)
# ts.plot(cbind(cumsum(PL),DD),col=c("red","green"),lwd=1.5)
yrange=range(cumsum(PL),DD)
#回檔
plot(DD,type="h",lwd=2,col="darkgreen",ylim=yrange,ylab="PL")
par(new=T)
#損益
plot(cumsum(PL),type="l",lwd=2,col="red",ylim=yrange)
par(new=T)
#最高點
points(which(DD==0),cumsum(PL)[which(DD==0)],pch=4,col="purple")


