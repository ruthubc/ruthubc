# TODO: Add comment
# 
# Author: user
###############################################################################


library(ggplot2)
library(plyr)
library(data.table)


ColonySize <- read.csv("RuthSync/EggManipulation/RuthDataFiles/ColonySize.csv", na.strings = NA)

hist(ColonySize$ColSize)
hist(ColonySize$Log10ColSize)

ColSizeAve<- ddply(ColonySize, .(CensusNo, Treatment), summarise,
		N = length(!is.na(IndColID)),
		Size.Mean = mean(Log10ColSize, na.rm = TRUE),
		Size.sd   = sd(Log10ColSize),
		Size.se   = Size.sd / sqrt(N)
)


ggplot(data = ColSizeAve, aes(x = CensusNo, y = Size.Mean, colour = Treatment )) + geom_point() + geom_line() +
		geom_errorbar(aes(ymin=Size.Mean - Size.se, ymax=Size.Mean + Size.se),width=.1) + 
		geom_text(aes(label=N),hjust=-0.3, vjust=-0.3)
