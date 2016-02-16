# Natural extinction -looking at old data
# 
# Author: Ruth
###############################################################################

NatExt <- read.csv("NaturalExtinction/NaturalExtinctionAll.csv", na.strings = NA)


levels(NatExt$NestID)

#Updates with the number of observations by nest
CountObsByNest<- ddply(NatExt, .(NestID), summarise, 
		N = length(Date)
)

NatExtMer<- merge(NatExt, CountObsByNest, by = (c("NestID")))


write.table(NatExtMer,"NaturalExtinction/NaturalExtinctionAll.csv", sep = ",", row.names = FALSE)

