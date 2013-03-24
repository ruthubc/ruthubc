# TODO: Add comment
# 
# Author: Ruth
###############################################################################


####September 2012; upadated 1st Oct to output to one PDF file.


# Expand right side of clipping rect to make room for the legend
#par(xpd=T, mar=par()$mar+c(2,0,0,2))


Census<-read.csv("DataEcuadorSummer2012/Censuses.csv", na.strings = "NA") #importing the real file

pdf("DataEcuadorSummer2012/GraphFiles/CensusGraphs.pdf", onefile=TRUE) # for one graph


Census<-Census[order(as.Date(Census$Date, format="%d/%m/%Y")),] #orders by date

#Census$Date<-as.Date(Census$Date)

noNests<-as.numeric(nlevels(Census$TripletID))

Triplet<-levels(Census$TripletID) #saves the different nests

##updating nest type
NestTypes<-read.csv("DataEcuadorSummer2012/GraphFiles/NestType.csv", stringsAsFactors=FALSE)

Census$NestType<-as.character(Census$NestType)

for(m in 1:nrow(Census) ){
	
	Census$NestType[m]<-NestTypes$Actual.Type[which(NestTypes$NestNumber==as.character(Census$NestNumber[m]) & as.character(NestTypes$TripletID)==as.character(Census$TripletID[m]))]
	
}


#############TRIPLETLOOP##########

for(i in 1:noNests){ #look to seperate nests and loops
	
	
	CurTriplet<-(Triplet[i])
	
	#jpeg(file=paste("DataEcuadorSummer2012/GraphFiles/CensusGraphs/", CurTriplet, ".jpeg", sep=""))
	
	par(mgp = c(3, 0.25, 0)) #change location of labels default is (3,1,0)
	
	
	par(mfrow=c(2,1))
	
	#par(xpd=T, mar=par()$mar+c(1,0,-1,2))
	
	CenTri<-subset(Census, TripletID==CurTriplet) #excluding all but current triplet
	
	rowSums(CenTri[,19:25])
	
	CnsMax<-max(rowSums(CenTri[,19:25]))+10 #sets upperlimit for axis
	
	EggMax<-max(CenTri$EggSacsAFTERSwitching)
	
	CenTri$NestNumber<-factor(CenTri$NestNumber)#resetting the number of factors
	
	Nest<-levels(CenTri$NestNumber)
	
	print (CurTriplet) 
	print (CnsMax)

	
	###########NEST LOOP ##########
	
	for(k in 1:2){
		
		
		CurNest<-(Nest[k])
		
		OneOnly<-subset(CenTri, NestNumber==CurNest)
		
		Type<-(as.character(OneOnly$NestType)[1])
		
		
		
		
#CntOnly<-data.frame(OneOnly$Adult,OneOnly$Sub2, OneOnly$Sub1, OneOnly$Juv34) #removes all but couunts
		
		CntOnly<-aggregate(OneOnly[,19:25], list(OneOnly$Date), mean, simplify=TRUE)#mean
		
		CntOnly<-CntOnly[order(as.Date(CntOnly$Group.1, format="%d/%m/%Y")),] #orders by date of values entered
		
		dates<-substring(as.character(CntOnly$Group.1), 1, 5)
		
		CntOnly$Group.1<-NULL
		CntOnly$Date<-NULL
		
		
		paste(CurTriplet, CurNest, "-", Type, sep="")
		
		
#info from http://www.harding.edu/fmccown/r/#barcharts
		barplot(t(CntOnly), col=palette(), names.arg=dates, ylim=c(0, CnsMax),  yaxs="i", main=paste(CurTriplet, CurNest, "-", Type, sep=""), cex.names=0.75, las=1, tck=0.05)
		
		mtext("Date of Census",side=1,line=1.5)
		
		mtext("No of Spiders",side=2,line=1.9)
		
		
		legend(0, -(CnsMax/2.3), names(CntOnly), cex=0.7, fill=palette(), ncol=8, title="Key (orange circles are number of egg sacs)") #legend(x, y)
		
		par(new=TRUE) #tells R to overwrite the first plot
		
		EggOnly<-aggregate(OneOnly$EggSacsAFTERSwitching, list(OneOnly$Date), mean, simplify=TRUE)
		
		EggOnly<-EggOnly[order(as.Date(EggOnly$Group.1, format="%d/%m/%Y")),]
		
		plot(EggOnly$x, type="o", pch=16, lwd=2, col="dark orange", lty=2,xaxt="n",yaxt="n",xlab="",ylab="", yaxs="i", ylim=c(0, EggMax))
		
		axis(4)
		mtext("No of Eggs",side=4,line=1.5)
		
	}
	
	
	
	#dev.off()
	
}

dev.off()## one pdf

#par(mgp = c(3, 0.25, 0)) #change location of labels default is (3,1,0)


#par(mfrow=c(2,1))

#par(xpd=T, mar=par()$mar+c(1,0,-1,2))

#str(Census)

#levels(Census$TripletID)

#levels(Census$NestNumber)

#table(Census$ColonyID)

#levels(Census$Censuser)
