rm(list=ls())#清除工作環境
library(quantmod)
library(ggplot2)
library(ggthemes)
STK=na.omit(get(
  getSymbols("AMZN",from = "2007-01-01",to = "2019-10-31" ,src = "yahoo")))
STK=as.data.frame(STK)
#Close->收盤價、High->當日最高點、Low->當日最低點
#MA20->月線、MA60->季線
data<-data.frame(Close=STK$AMZN.Close,
                 High=STK$AMZN.High,
                 Low=STK$AMZN.Low,
                 MA20=SMA(STK$AMZN.Close,20),
                 MA60=SMA(STK$AMZN.Close,60))
###訊號--->0:月線在季線之下、1:月線在季線之上
data$Sign<-data$MA20>data$MA60
data$PL<-0#PL--->Paper Loss 帳面損益
data$PZ<-0#PZ--->用來儲存購買的量
Buy<-0
for(m in 61:nrow(data)){
  data$PZ[m]=data$PZ[m-1]
  #創3日新高買進(可加碼)
  if(data$Close[m]>=max(data$High[(m-3):(m-1)])){
    Buy=Buy+data$Close[m]
    data$PZ[m]=data$PZ[m-1]+1
  }#月線季線死亡交叉賣出
  if(data$Sign[m-1]==1 &&data$Sign[m]==0){
    Sell=data$Close[m]*data$PZ[m-1]
    data$PL[m]=Sell-Buy
    data$PZ[m]=0
    Buy=0
  }
}
#畫圖用的變數
minPL=min(data$PL)
maxPL=max(cumsum(data$PL))
rate=maxPL/max(data$PZ)*0.75#調整右邊的Y軸座標用
#X軸為日期、左邊的Y軸為累積損益、右邊的Y軸為購買量
ggplot(data)+
  geom_bar(aes(x=as.numeric(rownames(data)),y=PZ*rate),stat="identity",color=4)+
  geom_line(aes(x=as.numeric(rownames(data)),y=cumsum(PL)),size=1.3,color=2)+
  theme_economist()+
  ggtitle("創3日新高買進(可加碼),月線季線死亡交叉賣出")+
  scale_x_continuous(name="Index")+
  scale_y_continuous(name="累積損益",
                     sec.axis=sec_axis(~./rate,name="Quantity"),
                     limits=c(minPL,maxPL))+
  theme(plot.title=element_text(size=10,hjust=0.5))