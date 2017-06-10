# TODO: Add comment
# 
# Author: Ruth
###############################################################################


library(plyr)

setwd("C:/Users/Ruth/Dropbox/RuthEcuador2013/NestSize/")

bootSamples <- read.csv("bootSamplesDolph_9June.csv")

bootSummy <- ddply(bootSamples, .(SampleID, SampleSize), summarise,
		N    = sum(!is.na(Value)),
		mean = mean(Value, na.rm=TRUE),
		sum = sum(Value), 
		sd   = sd(Value, na.rm=TRUE)

)

write.csv(bootSummy,"Summary_bootSamplesDolph_9June.csv")