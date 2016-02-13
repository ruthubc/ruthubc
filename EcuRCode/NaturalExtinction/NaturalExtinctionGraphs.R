# Natural extinction -looking at old data
# 
# Author: Ruth
###############################################################################

NatExt <- read.csv("NaturalExtinction/NaturalExtinctionAll.csv", na.strings = NA)


levels(NatExt$NestID)

CountObsByNest<- ddply(NatExt, .(NestID), summarise, # need to discount trials where no feeding obs and eve
		N = length(Date)
)

NatExtMer<- merge(NatExt, CountObsByNest, by = (c("NestID")))


write.table(NatExtMer,"NaturalExtinction/NaturalExtinctionAll.csv", sep = ",", row.names = FALSE)
