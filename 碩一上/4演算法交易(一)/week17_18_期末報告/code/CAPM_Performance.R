###實現Alpha策略，並且回測績效&作圖###
rm(list=ls())#清除工作環境
options(scipen = 999)#不要科學符號
library(quantmod)
library(PerformanceAnalytics)
#個別股
STK1<-get(getSymbols("2330.TW",from="2017-12-20",to="2019-12-31",src="yahoo"))
#台股加權指數
getSymbols("^TWII",from="2017-12-20",to="2019-12-31",src="yahoo")
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


#存Alpha和Beta用
result<-data.frame(a=rep(0,nrow(STK1)),b=rep(0,nrow(STK1)))
result$b.bull<-0
result$b.bear<-0
#把result的rowname改成日期格式
rname<-index(STK1)
rownames(result)<-rname
#找出2019第一天的位置
#因為需要前一天的資料，所以從2018最後一天開始
m<-grep("2019-01-02",index(STK1))-1
for(i in m:nrow(STK1)){
  #股市一年平均有245交易日
  #個別股
  Stock1<-STK1[(i-245):i,]
  #市場指數(台股加權指數)
  Market<-TWII[(i-245):i,]
  price<-cbind(Cl(Stock1),Cl(Market))
  #計算每天的報酬率
  return<-Return.calculate(price)
  return<-return[(-1),]
  #計算各種Alpha與Beta，並存result裡
  result$b[i]<-CAPM.beta(return[,1],return[,2],Rf=0)
  result$b.bull[i]<-CAPM.beta.bull(return[,1],return[,2],Rf=0)
  result$b.bear[i]<-CAPM.beta.bear(return[,1],return[,2],Rf=0)
  result$a[i]<-CAPM.alpha(return[,1],return[,2],Rf=0)
}

#為了方便運算，所以把資料限縮於2019年&從新儲存於另外一個變數
result.plot<-result[(m+1):nrow(result),]
STK2019<-window(STK1,start="2019-01-01",end="2019-12-31")
TWII2019<-window(TWII,start="2019-01-01",end="2019-12-31")
#要多cbind一欄，是因為之後要放投資組合的報酬率
#因為不會其他語法，所以直接套用STK2019(省時間，不正統)
price<-cbind(Cl(STK2019),Cl(TWII2019),Cl(STK2019))
returns<-Return.calculate(price)
returns<-returns[(-1),]
#因為我們是賣空市場指數，所以要將第二欄的returns全改為負值
returns[,2]<- -(returns[,2])
for(i in 1:nrow(returns)){
  #若Alpha值>=0，則代表要計算報酬率
  if(result.plot$a[i]>=0){
    returns[i,3]<-returns[i,1]/(1+result.plot$b[i])+returns[i,2]*result.plot$b[i]/(1+result.plot$b[i])
  }
  #反之若Alpha值為負值，則沒有買進賣空，不須計算報酬率
  else
    returns[i,3]<-0
}


################自製圖###################
###參考來源: 
###https://stackoverflow.com/questions/14817006/ggplot-version-of-charts-performancesummary
gg.charts.PerformanceSummary<-function(rtn.obj,geometric=TRUE,main="",plot=TRUE)
{
  #load libraries
  suppressPackageStartupMessages(require(ggplot2))
  suppressPackageStartupMessages(require(scales))
  suppressPackageStartupMessages(require(reshape))
  suppressPackageStartupMessages(require(PerformanceAnalytics))
  
  #create function to clean returns if having NAs in data
  clean.rtn.xts<-function(univ.rtn.xts.obj,na.replace=0){
    univ.rtn.xts.obj[is.na(univ.rtn.xts.obj)]<- na.replace
    univ.rtn.xts.obj  
  }
  
  #Create cumulative return function
  cum.rtn<-function(clean.xts.obj,g=TRUE)
  {
    x<-clean.xts.obj
    if(g==TRUE){y<-cumprod(x+1)-1} else {y<-cumsum(x)}
    y
  }
  
  #Create function to calculate drawdowns
  dd.xts<-function(clean.xts.obj,g=TRUE)
  {
    x<-clean.xts.obj
    if(g==TRUE){y<-PerformanceAnalytics:::Drawdowns(x)} else {y<-PerformanceAnalytics:::Drawdowns(x,geometric=FALSE)}
    y
  }
  
  #create a function to create a dataframe to be usable in ggplot to replicate charts.PerformanceSummary
  cps.df<-function(xts.obj,geometric)
  {
    x<-clean.rtn.xts(xts.obj)
    series.name<-colnames(xts.obj)[1]
    tmp<-cum.rtn(x,geometric)
    tmp$rtn<-x
    tmp$dd<-dd.xts(x,geometric)
    colnames(tmp)<-c("Cumulative_Returns","Daily_Returns","Drawdown") # names with space
    tmp.df<-as.data.frame(coredata(tmp))
    tmp.df$Date<-as.POSIXct(index(tmp))
    tmp.df.long<-melt(tmp.df,id.var="Date")
    tmp.df.long$asset<-rep(series.name,nrow(tmp.df.long))
    tmp.df.long
  }
  
  #A conditional statement altering the plot according to the number of assets
  if(ncol(rtn.obj)==1)
  {
    #using the cps.df function
    df<-cps.df(rtn.obj,geometric)
    #adding in a title string if need be
    if(main==""){
      title.string<-paste("Asset Performance")
    } else {
      title.string<-main
    }
    
    gg.xts<-ggplot(df,aes_string(x="Date",y="value",group="variable"))+
      facet_grid(variable ~ ., scales="free_y",space="fixed")+
      geom_line(data=subset(df,variable=="Cumulative_Returns"),col="purple",size=1.2)+
      geom_bar(data=subset(df,variable=="Daily_Returns"),stat="identity",fill="#FF6666")+
      geom_line(data=subset(df,variable=="Drawdown"),col="darkgreen",size=1.2)+
      geom_hline(yintercept=0,size=0.5,colour="black")+
      ggtitle(title.string)+
      theme(axis.text.x=element_text(angle=0,hjust=1))+
      scale_x_datetime(breaks=date_breaks("6 months"),labels=date_format("%m/%Y"))+
      ylab("")+
      xlab("")
    
  } 
  else 
  {
    #a few extra bits to deal with the added rtn columns
    no.of.assets<-ncol(rtn.obj)
    asset.names<-colnames(rtn.obj)
    df<-do.call(rbind,lapply(1:no.of.assets,function(x){cps.df(rtn.obj[,x],geometric)}))
    df$asset <-ordered(df$asset, levels=asset.names)
    if(main==""){
      title.string <- paste("Asset",asset.names[1],asset.names[2],asset.names[3],"Performance")
    }else{
      title.string <- main
    }
    
    if(no.of.assets>5){legend.rows<-5} else {legend.rows<-no.of.assets}
    
    gg.xts<-ggplot(df,aes_string(x="Date",y="value"))+
      
      #panel layout
      facet_grid(variable~., scales="free_y",space="fixed",shrink=TRUE,drop=TRUE,margin= 
                   ,labeller=label_value)+ # label_value is default
      
      #display points for Index and Drawdown, but not for Return
      geom_point(data=subset(df,variable==c("Cumulative_Returns","Drawdown"))
                 ,aes(colour=factor(asset),shape=factor(asset)),size=1.2,show.legend=TRUE)+ 
      
      #manually select shape of geom_point
      scale_shape_manual(values=c(1,2,3))+ 
      
      #line colours for the Index
      geom_line(data=subset(df,variable=="Cumulative_Returns"),aes(colour=factor(asset)),show.legend=FALSE)+
      
      #bar colours for the Return
      geom_bar(data=subset(df,variable=="Daily_Returns"),stat="identity"
               ,aes(fill=factor(asset),colour=factor(asset)),position="dodge",show.legend=FALSE)+
      
      #line colours for the Drawdown
      geom_line(data=subset(df,variable=="Drawdown"),aes(colour=factor(asset)),show.legend=FALSE)+
      
      #horizontal line to indicate zero values
      geom_hline(yintercept=0,size=0.5,colour="black")+
      
      #horizontal ticks
      scale_x_datetime(breaks=date_breaks("6 months"),labels=date_format("%m/%Y"))+
      
      #main y-axis title
      ylab("")+
      
      #main x-axis title
      xlab("")+
      
      #main chart title
      ggtitle(title.string)
    
    #legend 
    
    gglegend<-guide_legend(override.aes=list(size=3))
    
    gg.xts<-gg.xts+guides(colour=gglegend,size="none")+
      
      # gglegend <- guide_legend(override.aes = list(size = 3), direction = "horizontal") # direction overwritten by legend.box?
      # gg.xts <- gg.xts + guides(colour = gglegend, size = "none", shape = gglegend) + # Warning: "Duplicated override.aes is ignored"
      
      theme( legend.title = element_blank()
             , legend.position = c(0,1)
             , legend.justification = c(0,1)
             , legend.background = element_rect(colour = 'grey')
             , legend.key = element_rect(fill = "white", colour = "white")
             , axis.text.x = element_text(angle = 0, hjust = 1)
             , strip.background = element_rect(fill = "white")
             , panel.background = element_rect(fill = "white", colour = "white")
             , panel.grid.major = element_line(colour = "grey", size = 0.5) 
             , panel.grid.minor = element_line(colour = NA, size = 0.0)
      )
    
  }
  assign("gg.xts",gg.xts,envir=.GlobalEnv)
  if(plot==TRUE){
    plot(gg.xts)
  } else {}
  
}
#print所有圖出來
gg.charts.PerformanceSummary(returns[,3], geometric=TRUE)
par(mfrow=c(2,2))
plot(result.plot$a,type="l",col="red",lwd=2,main="alpha值變動",ylab="alpha")
plot(result.plot$b,type="l",col="red",lwd=2,main="beta值變動",ylab='beta')
plot(result.plot$b.bull,type="l",col="red",lwd=2,main="beta.bull值變動",ylab="beta.bull")
plot(result.plot$b.bear,type="l",col="red",lwd=2,main="beta.bear值變動",ylab="beta.bear")
