# TODO: Add comment
# 
# Author: Ruth
###############################################################################

library(xlsx)

prop<-read.xlsx("DataEcuadorSummer2012/Dispersal/Dispersal.xlsx", 1)

#aleady comes up as dateprop$Date<-as.Date(prop$Date) 



plot(prop$Date, prop$noNewProps, col='white')

for (i in  1:nlevels(prop$Nest.ID)){
	
	levels(prop$Nest.ID)[i]

	propSub<-prop[which(prop$Nest.ID==levels(prop$Nest.ID)[i]),]
	points(prop$Date, prop$noNewProps)

}