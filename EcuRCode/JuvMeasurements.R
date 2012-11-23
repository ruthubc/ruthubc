# TODO: Add comment
# 
# Author: Ruth
###############################################################################


JuvMes<-read.csv("D:/Dropbox/DataEcuadorSummer2012/JuvMeasurements.csv", quote="", stringAsFactors=FALSE)

NestTypes<-read.csv("C:/Users/Ruth/Documents/NestType.csv", stringsAsFactors=FALSE)



		
for(i in 1:nrow(JuvMes) ){
		
	JuvMes$Type[i]<-NestTypes$Actual.Type[which(NestTypes$NestNumber==JuvMes$NestNumber[i] & NestTypes$TripletID==JuvMes$TripletID[i])]

}



Triplet<-levels(JuvMes$TripletID)

JuvMes$Date<-substr(JuvMes$Date, 1, 5)

noNests<- as.numeric(nlevels(JuvMes$TripletID))

JuvMes$Nest<- paste(JuvMes$Date, JuvMes$NestNumber, JuvMes$Type, sep=" ")

par(mfrow=c(2,2))


for(i in 1:noNests){



temp<-subset(JuvMes, TripletID==Triplet[i])


boxplot(temp$individual.weight~temp$Nest,main=Triplet[i], cex=0.6, las=2)



}