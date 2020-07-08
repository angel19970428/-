###計算2019年0050所有股票的股性(Beta、Beta.Bull、Beta.Bear)###
rm(list=ls())#清除工作環境
options(scipen = 999)#不要科學符號
library(quantmod)
library(PerformanceAnalytics)
#個別股
STK1<-get(getSymbols("2330.TW",from="2019-01-01",to="2019-12-29",src="yahoo"))
#台股加權指數
getSymbols("^TWII",from="2019-01-01",to="2019-12-29",src="yahoo")

#如果兩個dataframe都沒空值則不要執行，直接算Beta
#只要其中一個dataframe有空值，則兩個都刪掉那row
na1<-as.data.frame(is.na(STK1))
na.vector1<-grep(TRUE,na1[,1])
na2<-as.data.frame(is.na(TWII))
na.vector2<-grep(TRUE,na2[,1])
#利用union()儲存有空值的列index
na<-union(na.vector1,na.vector2)
#刪掉空值，並檢查兩個dataframe是否列數一樣
if(length(na)>0){
  STK1<-STK1[-na,]
  TWII<-TWII[-na,]
}
nrow(STK1)
nrow(TWII)

###計算各種Beta
price<-cbind(Cl(STK1),Cl(TWII))
returns<-Return.calculate(price)
returns<-returns[(-1),]
#單純的Beta
CAPM.beta(returns[,1],returns[,2],Rf=0)
#Beta.Bull
CAPM.beta.bull(returns[,1],returns[,2],Rf=0)
#Beta.Bear
CAPM.beta.bear(returns[,1],returns[,2],Rf=0)


###後面可以不用執行，補充寫法
###(如果不是用單一隻股票，而是投資組合的情況)

# price<-cbind(Cl(STK1),Cl(STK2),Cl(STK3),Cl(TWII))
# returns<-Return.calculate(price)
# returns<-returns[(-1),]
# CAPM.beta(returns[,1:3],returns[,4],Rf=0)
# CAPM.alpha(returns[,1:3],returns[,4],Rf=0)
