#quantmod教學
#install.packages("quantmod")
library(quantmod)
getSymbols("GOOG",from = "2018-01-01",to = "2019-09-25" ,src = "yahoo")
ma20<-runMean(GOOG[,4],n=20)
ma60<-runMean(GOOG[,4],n=60)
#to.weekly、monthly、quarterly、yearly
chartSeries(
  to.weekly(GOOG),theme='white',name='谷歌',
  up.col='red',dn.col='green')
#加上20日線和60日線
addTA(ma20,on=1,col="blue")
addTA(ma60,on=1,col="red")