# TODO: Add comment
# 
# Author: Ruth
###############################################################################

library(xlsx)

prop<-read.xlsx("DataEcuadorSummer2012/Dispersal/Dispersal.xlsx", 1)

#aleady comes up as dateprop$Date<-as.Date(prop$Date) 

prop$noNewProps<-as.integer(prop$noNewProps)


pdf("DataEcuadorSummer2012/Dispersal/NumPropGraph.pdf", onefile = TRUE)
#plot(prop$Date, prop$noNewProps, col='white')

for (i in  1:nlevels(prop$Nest.ID)){
	
	levels(prop$Nest.ID)[i]

	propSub<-prop[which(prop$Nest.ID==levels(prop$Nest.ID)[i]),]
	plot(propSub$Date, propSub$noNewProps, main = levels(prop$Nest.ID)[i], pch=16, type="b" , 
			ylim=c(0, max(propSub$noNewProps)+1), xlim=c(min(prop$Date), max(prop$Date) ))

}

dev.off()

nests<- as.data.frame(table(prop$Nest.ID))