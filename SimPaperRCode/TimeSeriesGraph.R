#creating time series graph with optimal and stable group size

source("Ruth/SplineFunction.R")

graphHeaders <- read.csv("Ruth/RFile/graphHeaders.csv", stringsAsFactors=FALSE)

fileNames<-read.csv("kinshipEvolution/DataAnalysis/fileNames.csv", quote="", col.names="filenames")

timeSes<-read.delim(as.character(fileNames[6,]))
#import the data

#par(mfrow=c(4,1), mar = c(2,4,0,2))
#defult par margins  5.1 4.1 4.1 2.1 (btm, left, top, right)
#X11()

pdf("C:/Users/Ruth/Desktop/KinPrefExampeTimeSeriesGraphSpline.pdf", width=11, height=9)

layout(matrix(c(1,2,3,4), 4, 1, byrow = FALSE), heights = c(1.1, 1, 1, 1.3) )

MarginsFun<-function(i){
	
	l<-5.5
	r<-3
	
	if (i==1){t<-2} else {t<-0.5}
	if (i==4){b<-4} else {b<-0.5}
	
	return (c(b,l,t,r)) #setting margins c(bottom, left, top, right)
	
}

#graphs need to be made of:
#aveCoop=8
#AvegroupSize=10
#rel=12
#kinPref=13
#optimum gp size =15
#stable gp size = 16

paramtrs<-c(13, 12, 8, 10)



for(i in 1:4) {
  
  pam<-paramtrs[i]# define which paramter you want to run
  
  genRem<-10000# number of generations to remove from the start
  
  run<-ts(timeSes[,pam][which(timeSes$tick>=genRem)], start=genRem)
  
  spl<-fnSpline(0.0001, time(run), run)
  #spl#print the spline
  
  print(graphHeaders[pam,4])#checking which parambetr chosen
  
 
  
  #plotting the spline smoothed times series
  if (i < 4) { 
    
    xLabel<-""   # defines the xlabel
    ticks<-"n"  #defines whether there are any ticks on the xaxis
    
  } else {               
    
    xLabel<-"Generation"
    ticks<-"s"
  }
  
  #par(mar=c(3,5,0.4,2))#setting the size of the margins
  #bottom, left, top, right

 par(mar=MarginsFun(i))
	
  plot(spl, pch=3, col="white", xlab=xLabel, ylab=graphHeaders$AxisHeadersTime[pam], cex.axis=1.7, cex.lab=1.7,xaxt=ticks, mgp=c(2,0.5,0), xaxs="i", xlim=c(10000, 50000))
  
  abline(v=c(1000*(11:49)), col="darkgrey", lty=3)# creating vertical grid lines
  
  lines(spl)
  
}

dev.off()
#run<-ts(timeSes[,15][which(timeSes$tick>=genRem)], start=genRem)

#spl<-fnSpline(0.0001, run)

#lines(spl, lty=2)

#print(row.names(run))