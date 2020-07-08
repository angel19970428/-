###猜數字
a<-sample(1:9,9)
# f1<-((a[1]*10+a[2])*a[3]==a[4]*10+a[5])
# f2<-(a[4]*10+a[5]+a[6]*10+a[7]==a[8]*10+a[9])
while(((a[1]*10+a[2])*a[3]!=a[4]*10+a[5])||(a[4]*10+a[5]+a[6]*10+a[7]!=a[8]*10+a[9])){
  a<-sample(1:9,9)
  print(a)
}
###大樂透多久中獎
t1<-Sys.time()
k=0
lucky=sort(c(1,3,5,7,9,10))
sample(1:10,6)
while(sum(lucky==sort(sample(1:10,6)))<6){
  sample(1:10,6)
  k=k+1
  print(k)
}
t2<-Sys.time()
cat(t2-t1)


###pi
getpi <- function(){
  
  N <- 1000000
  cnt <- 0
  for (i in 1:N) {
    x <- runif(1,0,1)
    y <- runif(1,0,1)
    
    if((x*x + y*y) <=1)
      cnt= cnt + 1
    
  }
  
  return ( 4 * cnt / N)
}
getpi()

