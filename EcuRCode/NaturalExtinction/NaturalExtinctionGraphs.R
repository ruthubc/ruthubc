# Natural extinction -looking at old data
# 
# Author: Ruth
###############################################################################

library(ggplot2)

NatExt <- read.csv("NaturalExtinction/NaturalExtinctionAll.csv", na.strings = NA)


levels(NatExt$NestID)

#Updates with the number of observations by nest
CountObsByNest<- ddply(NatExt, .(NestID), summarise, 
		N = length(Date)
)

#NatExtMer<- merge(NatExt, CountObsByNest, by = (c("NestID")))


#write.table(NatExtMer,"NaturalExtinction/NaturalExtinctionAll.csv", sep = ",", row.names = FALSE)

NatExt$Length <- as.numeric(paste(NatExt$Length))

NatExt$Width <- as.numeric(paste(NatExt$Width))

NatExt$Area <- pi * NatExt$Length * NatExt$Width

NatExt$AreaLog <- log(NatExt$Area)

NatExtSub <- subset(NatExt, NumObs > 1)

range <- max(NatExtSub$AreaLog, na.rm=T) - min(NatExtSub$AreaLog, na.rm=T)

ggplot(NatExtSub, aes(AreaLog)) + geom_histogram(binwidth = range/60)


#outputing a freq distribution table
factorx <- factor(cut(NatExtSub$AreaLog, breaks=60)) # IF NOT LOTS OF BINS THEN DUPLICATE TOP FREQ NESTSS
#Tabulate and turn into data.frame
xout <- as.data.frame(table(factorx))
#Add cumFreq and proportions
xout <- transform(xout, cumFreq = cumsum(Freq), relative = prop.table(Freq))

TopFreqNest <- subset(NatExtSub, AreaLog <9.99 & AreaLog >= 9.84)

TopFreqNest <- data.frame("NestID" = TopFreqNest$NestID, "OrgTime" = TopFreqNest$NumDays)

TopFreqNest$Dup <- duplicated(TopFreqNest$NestID)

DupNests <- subset(TopFreqNest, Dup == "TRUE")$NestID

TopFreqNest <- TopFreqNest[!TopFreqNest$NestID %in% DupNests,] # subsets the file for those nests that are duplicated

NatExtSub <- merge(NatExtSub, TopFreqNest, by = c("NestID"), all.x = TRUE)

NatExtSub$RelTime <- NatExtSub$NumDays - NatExtSub$OrgTime

NatExtFew <- subset(NatExtSub, !is.na(RelTime) )


ggplot(NatExtFew, aes(RelTime, AreaLog)) + geom_point() + geom_line() + facet_wrap(~NestID)






