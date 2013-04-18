# TODO: Add comment
# 
# Author: Ruth
###############################################################################


JuvMes<-read.csv("DataEcuadorSummer2012/JuvMeasurements.csv", stringsAsFactors=FALSE)

 ##updating nest types

NestTypes<-read.csv("DataEcuadorSummer2012/GraphFiles/NestType.csv", stringsAsFactors=FALSE)

		
for(i in 1:nrow(JuvMes) ){
		
	JuvMes$Type[i]<-NestTypes$Actual.Type[which(NestTypes$NestNumber==JuvMes$NestNumber[i] & NestTypes$TripletID==JuvMes$TripletID[i])]

}


write.csv(JuvMes, "JuvMeasurements.csv")

######

Triplet<-levels(as.factor(JuvMes$TripletID))

JuvMes$individual.weight <- JuvMes$individual.weight*1000

JuvMes$Date<-as.Date(JuvMes$Date, "%d/%m/%Y")

JuvMes<-JuvMes[order(JuvMes$Date, JuvMes$Type),]

JuvMes$Date<-format(JuvMes$Date,"%d/%m/%y")

JuvMes$Date

#JuvMes$Date<-substr(JuvMes$Date, 1, 5)

noNests<- length(Triplet)

JuvMes$Nest<- paste(JuvMes$Date, JuvMes$Type, sep="-")

#X11()

pdf("DataEcuadorSummer2012/JuvMeasGraphs.pdf")
par(mfrow=c(2,3))
par(mar=c(7,5,3,1)) # c(bottom, left, top, right) 



for(i in 1:noNests){



temp<-subset(JuvMes, TripletID==Triplet[i])


boxplot(temp$individual.weight~temp$Nest,main=Triplet[i], cex=0.6, las=2, ylab = "weight, mg")



}

dev.off()
