# TODO: Add comment
# 
# Author: Ruth
###############################################################################

library(xlsx)

par(mar=c(10, 4, 4, 4))

prop<-read.xlsx("DataEcuadorSummer2012/Dispersal/Dispersal.xlsx", 1)

#already comes up as dateprop$Date<-as.Date(prop$Date) 

prop$noNewProps<-as.integer(prop$noNewProps)

maxArea<- max(prop$Area)

pdf("DataEcuadorSummer2012/Dispersal/NumPropGraph2.pdf", onefile = TRUE)
#plot(prop$Date, prop$noNewProps, col='white')

for (i in  1:nlevels(prop$Nest.ID)){
	
	levels(prop$Nest.ID)[i]

	propSub<-prop[which(prop$Nest.ID==levels(prop$Nest.ID)[i]),]
	plot(propSub$Date, propSub$noNewProps, main = levels(prop$Nest.ID)[i], pch=16, type="b" ,
			ylab= "",
			ylim=c(0, max(prop$noNewProps)+1),  xlim=c(min(prop$Date), max(prop$Date)))
	
	par(new=T) # tells R to overwrite the other plot
	plot (propSub$Date, propSub$Area, pch = 25, type = "b",  col = 'red', yaxs="i",
			ann=FALSE,  ylab = "", yaxt = "n", 
			ylim=c(0, max(prop$Area)+0.25), xlim=c(min(prop$Date), max(prop$Date)) )
	
	axis(4)
	mtext("area of nest",side=4,line=2)
	
	par(new=T)
	
	plot(propSub$Date, propSub$Age.Max, type = 'l', yaxs = "i",
			yaxt='n', ann=FALSE, lty = 3, ylab="",
			ylim=c(0, 4), xlim=c(min(prop$Date), max(prop$Date)))
	axis(2, line = 2, tcl=-1)
	lines(propSub$Date, propSub$Age.Min, lty = 3)
	
	

}

dev.off()

nests<- as.data.frame(table(prop$Nest.ID))