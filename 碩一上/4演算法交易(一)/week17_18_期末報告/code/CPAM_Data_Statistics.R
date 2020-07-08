###璸衡0050い┦计###
library(readxl)
X0050 <- read_excel("C:/Users/Peter/Desktop/0050.xlsx", 
                    col_types = c("text","text","numeric","blank","numeric",
                                  "numeric","numeric","blank","blank","blank"))
count1<-0#ю阑┦布Beta>1
count2<-0#ň縨┦布Beta<1
for(i in 1:50){
  if(X0050$Beta[i]>1)
    count1<-count1+1
  else
    count2<-count2+1
}
paste0(count1,",",count2)
########################
count3<-0#布beta.bull > 1 && beta.bear < 1
count4<-0#布beta.bull < 1 && beta.bear > 1
for(i in 1:50){
  if(X0050$`Beta-Bull`[i]>1 && X0050$`Beta-Bear`[i]<1)
    count3<-count3+1
  if(X0050$`Beta-Bull`[i]<1 && X0050$`Beta-Bear`[i]>1)
    count4<-count4+1
}
paste0(count3,",",count4)