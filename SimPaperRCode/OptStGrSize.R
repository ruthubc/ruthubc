#creating time series graph with whatever you want on one graph

source("D:/EclipseR/SimPaper/SplineFunction.R")

graphHeaders <- read.csv("D:/Dropbox/Ruth/RFile/graphHeaders.csv", stringsAsFactors=FALSE)

timeSes <- read.delim("D:/Dropbox/kinshipEvolution/NewSimulations2010-2011/r0.10c0.06b0.2/r0.1c0.06b0.2_RPT.pf.csv.series")

#graphs need to be made of:
#aveCoop=8
#AvegroupSize=10
#rel=12
#kinPref=13
#optimum gp size =15
#stable gp size = 16

paramtrs<-c(10,15)

plot(timeSes$run, timeSes$avgGrSize, pch=3, ylim=c(8, 19), col="white", xlab="Generation", cex.axis=1.1, cex.lab=1.3,xaxt=ticks, mgp=c(2,0.5,0), xaxs="i", xlim=c(10000, 50000))


for(i in 1:length(paramtrs)) {
  
  pam<-paramtrs[i]# define which paramter you want to run
  print(pam)
  
  genRem<-10000# number of generations to remove from the start
  
  run<-ts(timeSes[,pam][which(timeSes$tick>=genRem)], start=genRem)
  
  spl<-fnSpline(0.0001, run)
  spl#print the spline
  
  print(graphHeaders[pam,4])#checking which parambetr chosen
  
 lines(spl)
  
}

