#跌破布林帶下緣放空，突破布林帶下緣平倉
rm(list=ls())#清除工作環境
options(scipen = 999)#不要科學符號
library(quantmod)
STK=na.omit(get(
  getSymbols("AMZN",from = "2010-01-01",
             to = "2018-12-31" ,src = "yahoo")))
STK=as.data.frame(STK)
#BB--->Bollinger Bands布林通道
BB=as.data.frame(BBands(STK$AMZN.Close))
#Close-->收盤價、Down--->下軌、Avg--->中軌、Up--->上軌
data<-data.frame(Close=STK$AMZN.Close,
                 Down=BB$dn,
                 Avg=BB$mavg,
                 Up=BB$up)
#data欄位新增
data$Sign<-data$Close>data$Down
#收盤價是否大於下緣
#0:否、1:是
data$Cash<-0#現金累計、初始0
data$Capital<-0#總資產累計、初始0
#####總資產損益累計計算#####
hasSell=FALSE
STKValue=0#股票價值
for(m in 21:nrow(data)){
  data$Cash[m]=data$Cash[m-1]
  data$Capital[m]=data$Capital[m-1]
  if(!hasSell){
    #若收盤價跌破下緣放空
    if(data$Sign[m-1]==1 && data$Sign[m]==0){
      Sell=data$Close[m]
      data$Cash[m]=data$Cash[m-1]+Sell
      hasSell=TRUE
    }
  }else{
    STKValue=data$Close[m]
    #若收盤價跌破上緣賣出
    if(data$Sign[m-1]==0 && data$Sign[m]==1){
      Buy=data$Close[m]
      data$Cash[m]=data$Cash[m-1]-Buy
      STKValue=0
      hasSell=FALSE
    }
    data$Capital[m]=data$Cash[m]-STKValue
  }
}
###絕對回檔drawdown
data$DD1<-data$Capital-cummax(data$Capital)
###比例回檔drawdown(%)
data$DD2<-(data$Capital-cummax(data$Capital))/cummax(data$Capital)*100


###畫圖用參數
x=as.numeric(rownames(data))
xx=c(x,rev(x))
y1=data$Capital
y2=data$DD1
y2y2=c(rep(0,nrow(data)),rev(y2))
y3=data$DD2
y3y3=c(rep(0,nrow(data)),rev(y3))
###設定圖的區域讓多張圖可以被擠在一起
par(pty="m",plt=c(0.1, 1, 0, 1),omd=c(0.1,0.9,0.1,0.9))
#par():儲存圖形的各種參數
#pty:設置作圖類型，m=最大化作圖
#plt:設定繪圖區(plot region)占圖形區(figure region)的比例，plt=c(x1,x2,y1,y2)。
#omd:設定圖形外邊界占圖形的比例，omd = c(x1,x2,y1,y2)
###依row排序畫3張圖
par(mfrow=c(3, 1))
##################################畫1st的空圖######################################
plot(x,y1,xlim=range(x),type="n",xaxt="n",yaxt="n",main="",xlab="",ylab="")
#type:圖的類型，n=no plotting
#xaxt(x-axis type):x座標軸的樣式，n=不畫座標軸
#yaxt(y-axis type):y座標軸的樣式，n=不畫座標軸
mtext("跌破布林帶下緣放空，突破布林帶下緣平倉",3,line=1,cex=1.3)
#mtext():Write Text into the Margins of a Plot
#3:指在Top寫字
#line:與邊線的距離
#cex:字的大小
###儲存X軸的座標資料給三張圖一起用
pardat<-par()
xaxisdat<-seq(pardat$xaxp[1],pardat$xaxp[2],
              (pardat$xaxp[2]-pardat$xaxp[1])/pardat$xaxp[3])
#xaxp(x-axis positioning)範例:0,400,4==>指座標軸是0、100、200、300、400
###得到Y軸的座標資料+畫線和設定Y軸名稱
yaxisdat<-seq(pardat$yaxp[1],pardat$yaxp[2],
              (pardat$yaxp[2]-pardat$yaxp[1])/pardat$yaxp[3])
#yaxp(y-axis positioning)範例:0,500,5==>指座標軸是0、100、200、300、400、500
axis(2,at=yaxisdat,las=2,cex.axis=1)
#axis():Add an axis to a plot
#2=畫座標在left
#at:Y座標軸會顯示哪些Y座標(即Y座標刻度有哪些)
#las:設置座標標記顯示方向，2=垂直於座標軸
#cex.axis:座標字大小
abline(v=xaxisdat,col="lightgray")
abline(h=yaxisdat,col="lightgray")
#abline():Add Straight Lines to a Plot
mtext("總資產($)",2,line=3,cex=0.8)
#畫總資產損益累計的線
lines(x,y1,col="purple")
#畫最高點
points(which(y3==0),y1[which(y3==0)],pch=4,col="green")
##################################畫2nd的空圖######################################
plot(x,y2,xlim=range(x),type="n",xaxt="n",yaxt="n",main="",xlab="",ylab="")
###得到Y軸的座標資料+畫barchart和設定Y軸名稱
pardat<-par()
yaxisdat<-seq(pardat$yaxp[1],pardat$yaxp[2],
              (pardat$yaxp[2]-pardat$yaxp[1])/pardat$yaxp[3])
axis(2,at=yaxisdat,las=2,cex.axis=1)
abline(v=xaxisdat,col="lightgray")
abline(h=yaxisdat,col="lightgray")
mtext("絕對DD($)",2,line=3,cex=0.8)
lines(x,y2,col="red")
polygon(xx,y2y2,col="red")#從左上畫到右上到右下到左下，最後回到左上
##################################畫3rd的空圖######################################
plot(x,y3,xlim=range(x),type="n",xaxt="n",yaxt="n",main="",xlab="",ylab="")
###得到Y軸的座標資料+畫barchart和設定Y軸名稱
pardat<-par()
yaxisdat<-seq(pardat$yaxp[1],pardat$yaxp[2],
              (pardat$yaxp[2]-pardat$yaxp[1])/pardat$yaxp[3])
axis(2,at=yaxisdat,las=2,cex.axis=1)
abline(v=xaxisdat,col="lightgray")
abline(h=yaxisdat,col="lightgray")
mtext("比例DD(%)",2,line=3,cex=0.8)
lines(x,y3,col="red")
polygon(xx,y3y3,col="red")
######################################畫X軸#########################################
axis(1,at=xaxisdat,cex.axis=1)
mtext("交易天數",1,line=2)


###最大的5個DDD(drawdown duration)
DDD<-diff(which(data$DD2==0))-1
tail(sort(DDD),5)
###WinRate
win=0
lose=0
for(i in 2:nrow(data)){
  if(data$Capital[i]>data$Capital[i-1]){
    win=win+1
  }
  if(data$Capital[i]<data$Capital[i-1]){
    lose=lose+1
  }
}
total=win+lose
WinRate=win/total*100
###賺賠比(Odds)
Earn=c()
Loss=c()
for(i in 2:nrow(data)){
  if(data$Capital[i]>data$Capital[i-1]){
    money1=data$Capital[i]-data$Capital[i-1]
    Earn=c(Earn,money1)
  }
  if(data$Capital[i]<data$Capital[i-1]){
    money2=data$Capital[i]-data$Capital[i-1]
    Loss=c(Loss,money2)
  }
}
meanEarn=mean(Earn)
meanLoss=mean(Loss)
Odds=abs(meanEarn/meanLoss)
###獲利因子(profit factor)
#賺的和/賠的和
PF=sum(Earn)/abs(sum(Loss))
###獲利/MDD
result=data$Capital[nrow(data)]/abs(min(data$DD1))
###所有結果
paste0("最大的5個DDD:",tail(sort(DDD),5))
paste0("WinRate=",round(WinRate,2),"%")
paste0("Odds=",round(Odds,2))
paste0("Profit factor=",round(PF,2))
paste0("獲利/MDD=",round(result,2))
# 給回家作業Case4用
write.csv(data,file="C:/Users/Peter/Desktop/data2.csv",
          row.names = F,col.names = T,fileEncoding="UTF-8")