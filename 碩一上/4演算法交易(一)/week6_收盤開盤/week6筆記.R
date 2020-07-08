rm(list=ls())#清除工作環境
par(mfrow=c(2,2))#一頁多圖，在這邊是2row、2col
library(quantmod)
#get()--->取得特定環境空間中的變數
GOOG=get(getSymbols("GOOG",from = "2018-01-01",to = "2019-10-17" ,src = "yahoo"))
#GOOG=to.weekly(GOOG)
#可以選擇to.weekly、monthly、quarterly、yearly
###如果選monthly、quarterly要記得補參數indexAt = 'firstof'

#####第一張圖(最簡單的)#####
#收盤價(第4欄)-開盤價(第1欄)
PL=GOOG[,4]-GOOG[,1]#PL--->Paper Loss帳面損益
result=cbind(GOOG[,1],GOOG[,4],PL)
colnames(result)<-c("GOOG.Open","GOOG.Close","GOOG.Spread")
plot(cumsum(PL),type="l",col="red",lwd=2)
#cumsum()--->計算數值向量各分量的累積和
#type="l"畫線，lwd-->線的寬度
class(PL)##xts、zoo object

#####第二張圖(xts object)#####
Start1<-Sys.time()
PLts=xts(numeric(length(time(GOOG))),time(GOOG))
#xts()--->創建xts物件  xts=eXtensible Time Series 可擴展時間序列
#numeric()--->numeric(length=3)==>c(0,0,0)
for(m in as.character(time(GOOG))){
  PLts[m]=GOOG[m,4]-GOOG[m,1]
}
plot(cumsum(PLts),type="l",col="red",lwd=2)
class(PLts)##xts、zoo object
End1<-Sys.time()
time1<-End1-Start1
print(time1)##2秒左右

#####第三張圖(matrix)#####
Start2<-Sys.time()
#class(GOOG)  ##xts、zoo object
GOOG1=as.matrix(GOOG)#建議用成as.data.frame就會了
profit=setNames(numeric(nrow(GOOG1)),rownames(GOOG1))
#setNames()--->sets the names on an object and returns the object
#在這裡就是把profit數字向量的每一個元素命名
for(m in as.character(rownames(GOOG1))){
  profit[m]=Cl(GOOG1)[m]-Op(GOOG1)[m]
}
plot(cumsum(profit),type = "l",col="red",lwd=2)
#第三張圖會不一樣是因為profit跟前面兩個東西的型態不一樣
class(profit)##數字向量
End2<-Sys.time()
time2<-End2-Start2
print(time2)

#####第四張圖(自己試data.frame看看)#####
Start3<-Sys.time()
GOOG2=as.data.frame(GOOG)
GOOG2$profit<-GOOG2$GOOG.Close-GOOG2$GOOG.Open
plot(cumsum(GOOG2$profit),type="l",col="red",lwd=2)
class(GOOG2$profit)##數字向量
End3<-Sys.time()
time3<-End3-Start3
print(time3)

###比較計算時間2>3>4


