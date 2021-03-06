##Kin Preference Averages Graphs against C, R and Beta

#exporting graph pdf location ######
pdf("C:/Users/Ruth/Desktop/KinPrefAveragesGraph.pdf", width=11, height=11)

#jpeg("C:/Users/Ruth/Desktop/AveragesGraph.jpeg", width = 790, height = 790)

#imports of data and functions#########################################################################

source("G:/PhDWork/RCode/SimPaperCode/SplineFunction.R")

averages <- read.csv("kinshipEvolution/DataAnalysis/averages.csv")

#adding relative group size to averages
averages$relGrSize<-averages$C * averages$avgGrSize

CC<-as.numeric(levels(as.factor(averages$C)))

CCD<-c(50, 16.7, 10)

graphHeaders <- read.csv("Ruth/RFile/graphHeaders.csv", stringsAsFactors=FALSE)

#things to input before runing code################################
#R=4  
#Beta=5

xPar<-4 # parameter on the x axis
byPar<-5 #by parambeter

yVals<-c(13,12,10, 8)
#aveCoop=8
#AvegroupSize=10
#rel=12
#kinPref=13
#rel group size = 17


lambda<-0.001 #lambda for spline smoothing
cols<-c("navy", "tomato1","purple", "green4", "orange") # colours for graph
pnts<-c(15, 16, 2, 18, 17) #point type
lines<-c(2,3,4,5,6,7,8,9) #line type for graphs

#graph and parameter settings#################################################################################################

by<-as.numeric(levels(as.factor(averages[,byPar])))#number of levels of Beta
lenBy<-as.numeric(length(by))

layout(matrix(c(1:12), 4, 3, byrow = TRUE), widths = c(8.4,7,8), heights = c(10, 8, 8, 9.75)) #graph layout

#par(mar=c(5,5,1,1)) #setting margins c(bottom, left, top, right)

#function to get graph settings for different parameters
gphSet<-function(yParm){
  
  if (yParm==8){ #aveCoop
    
    ylimt<-c(0, 1) #y axis limit
    ya<-"r"    #y axis type
    aline<-c(0.1*(1:9))    
    
  } else if (yParm==10){ #ave GroupSize
    
    ylimt<-NULL #y axis limit
    ya<-"r"    #y axis type
    aline<-c(10*(1:7))    
    
  } else if (yParm==12){ #relatedness
    
    ylimt<-c(0, 0.49) #y axis limit
    ya<-"i"    #y axis type
    aline<-c(0.05*(1:9)) 
        
  } else if (yParm==13) { #kin preference
    
    ylimt<-c(0,1) #y axis limit
    ya<-"i"    #y axis type
    aline<-c(0.1*(1:9))    
  
  } else if (yParm==17) { #relative group size
  
  ylimt<-c(0, 1.6) #y axis limit
  ya<-"i"    #y axis type
  aline<-c(0.25*(1:6))
  
  }
  
  return(list(ylimt=ylimt, ya=ya, aline=aline))
  
}

#setting the margins function
MarFun<-function(j, k){
   
  if (k==4){b<-5} else {b<-0.75}
  
  if (k==1){t<-6} else {t<-0.75}
  
  if (j==1){l<-5} else {l<-0.75}
  
  if (j==3){r<-4} else {r<-0.75}  
  
  
  return (c(b,l,t,r)) #setting margins c(bottom, left, top, right)
  
}

#making graph#############################

for (k in 1:4){ #looping by rows/ y parameters
  
  yPar<-yVals[k]
  
  #looping by C
    for(j in 1:3){
      
      CVal=CC[j]
      
      par(mar=MarFun(j, k))
      
    ##setting first graph of the three has a y label, others don't
    if (j==1) {
          
          yLabel<-graphHeaders$AxisHeaders[yVals[k]] #y axis label 
          ticks<-"s"
        
          } else {               
        
          yLabel<-""   # y label is blank
          ticks<-"n"  #defines whether there are any ticks on the xaxis
        }
    
    
     #setting bottom graphs to display xlabels 
     if(k==4){ 
       
       xLabel<-"Intrinsic rate of growth(R)" 
       xticks<-"s" #s means some ticks, n means no ticks
       
     } else {
       
       xLabel<-"" # change to "" if not last run
       xticks<-"n"#"s" #s means some ticks, n means no ticks
     }
   
  

  #making empty plot with all C's so axis don't change
  plot(subset(averages, select=c(xPar, yPar)),col=0, xlab=xLabel, 
       ylab=yLabel, xaxt=xticks, cex.axis=1.8, cex.lab=1.8, mgp=c(3, 1, 0), 
       xaxs="i", xlim=c(0, 2.2), yaxt=ticks, ylim=gphSet(yPar)$ylimt, 
       yaxs=gphSet(yPar)$ya)    
      
      abline(h=gphSet(yPar)$aline, col="darkgrey")  #horizental grid lines on graphs
      
      if(k==1){mtext(paste("1/C=", CCD[j], sep=""), side=3, adj=0.5, cex=1.6, font=2, line=1)}
      
      
      #plotting the points and lines  
      for(i in 1:lenBy) {
        
        
        ave.Sub<-subset(averages, C==CVal & averages[,byPar]==by[i], select= c(xPar, yPar))
        
        points(ave.Sub, pch=pnts[i], col=cols[i], cex=1.2)  
        
        my.spline<-fnSpline(lambda, ave.Sub[,1], ave.Sub[,2])
		
        
        smooth<-spline(my.spline) # adds extra points to smooth the line so it is not angular  
        
        lines(smooth, lty=lines[i], lwd=1, col = cols[i])
        
      } 
    
    }  
}


##creates legend ####################


beta<-c("Beta=0.0", "Beta=0.2", "Beta=0.4", "Beta=0.6", "Beta=0.8")
#creates legend

#rect (xleft, ybottom, xright, ytop)
# kin pref if exporing w 911 h 324
#rect(1.0,0.06, 2.0, 0.28, col="white", border=NA)   
# average cooperation if exporting w911 h324
rect(1.2,0.23, 2.0, 0.75, col="white", border="black") 
#relatedness w911 h324
#rect(1.0,0.20, 2.0, 0.27, col="white", border=NA)

legend(x=1.2, y=.75, beta, cex=1.3, col=cols, pch=pnts, 
       lty=lines, box.col="transparent", bg="blue", bty="n",
       y.intersp=1) #kin pref x=1.0, y=0.3; ave coop x=1.0, y=0.4; relatedness 


dev.off()

