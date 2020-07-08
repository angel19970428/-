#Generate the data for the four graphs
x <- seq(1, 50, 1)
y1 <- 10*rnorm(50)
y2 <- 100*rnorm(50)
y3 <- 1000*rnorm(50)

#Set up the plot area so that multiple graphs can be crammed together
par(pty="m", plt=c(0.1, 1, 0, 1), omd=c(0.1,0.9,0.1,0.9))

#Set the area up for 4 plots
par(mfrow = c(3, 1))

#Plot the top graph with nothing in it =========================
plot(x, y1, xlim=range(x), type="n", xaxt="n", yaxt="n", main="", xlab="", ylab="")
mtext("Three Y Plots With the Same X", 3, line=1, cex=1.5)

#Store the x-axis data of the top plot so it can be used on the other graphs
pardat<-par()
xaxisdat<-seq(pardat$xaxp[1],pardat$xaxp[2],(pardat$xaxp[2]-pardat$xaxp[1])/pardat$xaxp[3])

#Get the y-axis data and add the lines and label
yaxisdat<-seq(pardat$yaxp[1],pardat$yaxp[2],(pardat$yaxp[2]-pardat$yaxp[1])/pardat$yaxp[3])
axis(2, at=yaxisdat, las=2, padj=0.5, cex.axis=0.8, hadj=0.5, tcl=-0.3)
abline(v=xaxisdat, col="lightgray")
abline(h=yaxisdat, col="lightgray")
mtext("y1", 2, line=2.3)
lines(x, y1, col="red")

#Plot the 2nd graph with nothing ================================
plot(x, y2, xlim=range(x), type="n", xaxt="n", yaxt="n", main="", xlab="", ylab="")

#Get the y-axis data and add the lines and label
pardat<-par()
yaxisdat<-seq(pardat$yaxp[1],pardat$yaxp[2],(pardat$yaxp[2]-pardat$yaxp[1])/pardat$yaxp[3])
axis(2, at=yaxisdat, las=2, padj=0.5, cex.axis=0.8, hadj=0.5, tcl=-0.3)
abline(v=xaxisdat, col="lightgray")
abline(h=yaxisdat, col="lightgray")
mtext("y2", 2, line=2.3)
lines(x, y2, col="blue")

#Plot the 3rd graph with nothing =================================
plot(x, y3, xlim=range(x), type="n", xaxt="n", yaxt="n", main="", xlab="", ylab="")

#Get the y-axis data and add the lines and label
pardat<-par()
yaxisdat<-seq(pardat$yaxp[1],pardat$yaxp[2],(pardat$yaxp[2]-pardat$yaxp[1])/pardat$yaxp[3])
axis(2, at=yaxisdat, las=2, padj=0.5, cex.axis=0.8, hadj=0.5, tcl=-0.3)
abline(v=xaxisdat, col="lightgray")
abline(h=yaxisdat, col="lightgray")
mtext("y3", 2, line=2.3)
lines(x, y3, col="green")


#Plot the X axis =================================================
axis(1, at=xaxisdat, padj=-1.4, cex.axis=0.9, hadj=0.5, tcl=-0.3)
mtext("X Variable", 1, line=1.5)
