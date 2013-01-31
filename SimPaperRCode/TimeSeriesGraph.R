#creating time series graph

source("D:/EclipseR/SimPaper/SplineFunction.R")

graphHeaders <- read.csv("D:/Dropbox/Ruth/RFile/graphHeaders.csv", stringsAsFactors=FALSE)

#import the data
timeSes <- read.delim("D:/Dropbox/kinshipEvolution/NewSimulations2010-2011/r0.10c0.06b0.2/r0.1c0.06b0.2_RPT.pf.csv.series")

#par(mfrow=c(4,1), mar = c(2,4,0,2))
#defult par margins  5.1 4.1 4.1 2.1 (btm, left, top, right)

layout(matrix(c(1,2,3,4), 4, 1, byrow = FALSE))

#graphs need to be made of:
#aveCoop=8
#AvegroupSize=10
#rel=12
#kinPref=13

paramtrs<-c(8,10,12,13)

names<-c("Average\nCooperation", "Average\nGroup Size", "Relatedness","Kin\nPreference")


for(i in 1:4) {

pam<-paramtrs[i]# define which paramter you want to run

genRem<-10000# number of generations to remove from the start

run<-ts(timeSes[,pam][which(timeSes$tick>=genRem)], start=genRem)

spl<-fnSpline(0.0001, run)
spl#print the spline

print(graphHeaders[pam,4])#checking which parambetr chosen


#plotting the spline smoothed times series
if (i < 4) { 
    print("less than 4")

      xLabel<-""   # defines the xlabel
      ticks<-"n"  #defines whenter there are any ticks on the xaxis
    
} else {               
    print("equals 4")
    
      xLabel<-"Generation"
      ticks<-"s"
}

par(mar=c(3,5,0.4,2))#setting the size of the margins
#bottom, left, top, right
    
plot(spl, pch=3, col="white", xlab=xLabel, ylab=names[i], cex.axis=1.1, cex.lab=1.3,xaxt=ticks, mgp=c(2,0.5,0), xaxs="i", xlim=c(10000, 50000))

abline(v=c(1000*(11:49)), col="darkgrey")# creating vertical grid lines

lines(spl)

}