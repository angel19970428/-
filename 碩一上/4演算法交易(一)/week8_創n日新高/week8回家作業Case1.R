rm(list=ls())#清除工作環境
library(quantmod)
library(ggplot2)
library(ggthemes)
STK=na.omit(get(
  getSymbols("AMZN",from = "2007-01-01",to = "2019-10-31" ,src = "yahoo")))
STK=as.data.frame(STK)
#Close-->收盤價、MA20-->月線、MA60-->季線
data<-data.frame(Close=STK$AMZN.Close,
                 MA20=SMA(STK$AMZN.Close,20),
                 MA60=SMA(STK$AMZN.Close,60))
###訊號--->0:月線在季線之下、1:月線在季線之上
data$Sign<-data$MA20>data$MA60
data$PL<-0#PL--->Paper Loss 帳面損益
hasBuy<-FALSE
for(m in 61:nrow(data)){
  if(!hasBuy){
    if(data$Sign[m-1]==0 && data$Sign[m]==1){
      Buy=data$Close[m]
      hasBuy=TRUE
    }
  }else{
    if(data$Sign[m-1]==0 && data$Sign[m]==0){#跌破後後一天
      Sell=data$Close[m]
      data$PL[m]=Sell-Buy
      hasBuy=FALSE
    }
  }
}
#X軸為日期、Y軸為累積損益
ggplot(data,aes(x=as.numeric(rownames(data)),y=cumsum(PL)))+
  geom_line(size=1.3,color=2)+
  theme_economist()+
  ggtitle("月線季線黃金交買進,月線季線死亡交叉賣出(不加碼)")+
  labs(x="Index",y="累積損益")+
  theme(plot.title=element_text(size=10,hjust=0.5))