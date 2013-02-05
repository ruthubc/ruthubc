##graphs relatedness against group size or whatever

###not the real 4 by 4 graph code... just testing and seeing!

library(pspline)
source("G:/PhDWork/RCode/SimPaperCode/SplineFunction.R")

#pdf("C:/Users/Ruth/Desktop/pRelCoop.pdf", height=3)

#imports the data
averages <- read.csv("G:/Dropbox/kinshipEvolution/DataAnalysis/averages.csv")

#imports info for axis headers etc. can check with rows to use
graphHeaders <- read.csv("G:/Dropbox/Ruth/RFile/graphHeaders.csv", stringsAsFactors=FALSE)

layout(matrix(c(1:3), 1, 3, byrow = TRUE))# layout of graphs

##adding column to averages for relative group size in relation to C

averages$relGrSize<-averages$C * averages$avgGrSize

################ setting the parameters###################

#R=4  
#Beta=5
#8 =cooperation
#10=ave group size
#12=relatedness
#17= group size scaled by group carrying capacity

xPar<-4 # parameter on the x axis 10 is gp size
yPar<-17 # parameter on y axis 12 is relatednes
byPar<-5 #by parambeter 5=beta

names<-names(averages)

xLabel<-names[xPar]
yLabel<-names[yPar]


pars<-c(xPar, yPar)

CC<-as.numeric(levels(as.factor(averages$C))) #because we are making it by C


RR<-as.numeric(levels(as.factor(averages$R))) 

BB<-as.numeric(levels(as.factor(averages$Beta)))

lambda<-0.001 #lambda for spline smoothing

# colours for graph
cols<-c("navy", "tomato1","purple", "green4", "orange")
#points for graph
pnts<-c(15, 16, 2, 18, 17)

lines<-c(2,3,4,5,6,7,8,9) #line styles for graphs


#setting margins c(bottom, left, top, right)
#par(mar=c(2.8,2.76,0.6,0.1))
#par(mar=c(1,1,1,0.1))


########### making plot ###############################

for(j in 1:3){



# makes empty plot with all C and R as don't want axis to change for each graph
plot(averages[,xPar], averages[,yPar],col=0, xlab=xLabel, ylab=yLabel, main=paste("C=", sep="", CC[j]))#, xlab=xLabel, ylab=yLabel, xaxt=xticks, cex.axis=1.2, cex.lab=1.4, mgp=c(1.7,0.5,0), xaxs="i", yaxt=ticks, yaxs=ya)

###creating different lines and points for different beta

for(i in 1:5) {
  
  
  #subsetting the data so we have what we want
  ave.Sub<-subset(averages, C==CC[j] & Beta==BB[i], select=pars)
  

  
  #adds points to graph
  points(ave.Sub, pch=pnts[i], col=cols[i], cex=1.2)  
  #pch is dot type, cex point size
  
  #create spline line from my function
  my.spline<-fnSpline(lambda, ave.Sub)
  
  smooth<-spline(my.spline) # adds extra points to smooth the line so it is not angular  
  
  lines(smooth, lty=lines[i], lwd=1, col = cols[i])
  #lty is line type goes 1-6
}  


}


###########create legend #################################

#plot(subset(averages, select=pars),col="transparent", xlab=graphHeaders[xPar,3], ylab=graphHeaders[yPar,3], cex.axis=0.80, cex.lab=0.90)

beta<-c("Beta=0.0", "Beta=0.2", "Beta=0.4", "Beta=0.6", "Beta=0.8")
#creates legend

legend(x=.3, y=0.6, beta, cex=0.8, col=cols, pch=pnts, lty=lines, box.col="transparent", bg="blue", bty="n") #kin pref x=1.0, y=0.3; ave coop x=1.0, y=0.4; relatedness

dev.off()