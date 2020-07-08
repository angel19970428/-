###創3日新高買進,創3日新低賣出 加碼與不加碼比較
rm(list=ls())#清除工作環境
library(quantmod)
library(ggplot2)
library(ggthemes)
STK=na.omit(get(
  getSymbols("AMZN",from = "2007-01-01",to = "2019-10-31" ,src = "yahoo")))
STK=as.data.frame(STK)
#還是data比較好，因為直接使用STK日期會無法變數字
#這樣不利於後面畫圖
data<-data.frame(Close=STK$AMZN.Close,
                 High=STK$AMZN.High,
                 Low=STK$AMZN.Low)
par(mfrow=c(2,1))
#####單張(不加碼)#####
m=3
data$PL1<-0#PL--->Paper Loss 帳面損益
while(m<=nrow(data)){
  ###可以用runMax()取代max()
  if(data$Close[m]>=max(data$High[(m-3):(m-1)])){
    Buy=data$Close[m]
    while(data$Close[m]>min(data$Low[(m-3):(m-1)]) && m<nrow(data)){
      m=m+1}
    Sell=data$Close[m]
    data$PL1[m]=Sell-Buy
  }
  m=m+1
}
sum(data$PL1)
plot(cumsum(data$PL1),type="l",col="red",main="單張")
#####多張(可加碼)#####
m=3
data$PL2<-0#PL--->Paper Loss 帳面損益
data$PZ<-0#PZ--->用來儲存購買的量
Buy=0
Sell=0
while(m<=nrow(data)){
  data$PZ[m]=data$PZ[m-1]
  if(data$Close[m]>=max(data$High[(m-3):(m-1)]) && m<nrow(data)){
    #設m<nrow(data)的原因不要讓最後一天買進
    #因為最後一天收盤一定會賣出，收盤還買進怪怪的
    Buy=Buy+data$Close[m]
    data$PZ[m]=data$PZ[m-1]+1
  }
  if(data$Close[m]<min(data$Low[(m-3):(m-1)]) || m==nrow(data)){
    Sell=data$Close[m]*data$PZ[m-1]
    data$PL2[m]=Sell-Buy
    data$PZ[m]=0
    Buy=0
  }
  m=m+1
}
sum(data$PL2)
plot(cumsum(data$PL2),type="l",col="red",main="加碼")
# yrange=range(na.omit(cumsum(PL1)),na.omit(cumsum(PL2)))
# plot(cumsum(PL),type="l",col="blue",main="單張",ylim=yrange)
# par(new=T)
# plot(cumsum(PL),type="l",col="red",main="加碼",ylim=yrange)

#畫圖用的變數
minPL=min(min(cumsum(data$PL1)),min(cumsum(data$PL2)))
maxPL=max(cumsum(data$PL2))
rate=maxPL/max(data$PZ)*0.75#調整右邊的Y軸座標用
#X軸為日期、左邊的Y軸為累積損益、右邊的Y軸為購買量
plot<-ggplot(data)+
  geom_bar(aes(x=as.numeric(rownames(data)),y=PZ*rate),stat="identity",color=8)+
  geom_line(aes(x=as.numeric(rownames(data)),y=cumsum(PL1)),size=1.3,color=2)+
  geom_line(aes(x=as.numeric(rownames(data)),y=cumsum(PL2)),size=1.3,color=3)+
  theme_economist()+
  ggtitle("創3日新高買進,創3日新低賣出 加碼與不加碼比較圖")+
  scale_x_continuous(name="Index")+
  scale_y_continuous(name="累積損益",
                     sec.axis=sec_axis(~./rate,name="Quantity"),
                     limits=c(minPL,maxPL))+
  theme(plot.title=element_text(size=10,hjust=0.5))
plot

