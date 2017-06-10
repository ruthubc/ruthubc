# TODO: Add comment
# 
# Author: Ruth
###############################################################################

# coefficient of variation

library(plyr)
library(ggplot2)

setwd("C:/Users/Ruth/Dropbox/RuthEcuador2013/NestSize/")

bootSamples <- read.csv("bootSamplesDolph_9June.csv")

bootSummy <- ddply(bootSamples, .(SampleID, SampleSize), summarise,
	N    = sum(!is.na(Value)),
	mean = mean(Value, na.rm=TRUE),
	sd   = sd(Value, na.rm=TRUE),
	sampVar = var(Value),
	cv = sd/mean,
	cv_coor = (1+ (1/4*N) * cv)
	
	)

	
# N vs cv
	
ggplot(bootSummy, aes(x = N, y = cv)) + geom_point() + geom_smooth(method='lm',formula=y~x)

# N vs cv_coor

ggplot(bootSummy, aes(x = N, y = cv_coor)) + geom_point() + geom_smooth(method='lm',formula=y~x)

# mean vs cv

ggplot(bootSummy, aes(x = mean, y = cv)) + geom_point() + geom_smooth(method='lm',formula=y~x)