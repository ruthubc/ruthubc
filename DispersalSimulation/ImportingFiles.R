# TODO: Add comment
# 
# Author: Ruth
###############################################################################

library(plyr)
library(ggplot2)


File <- read.csv("RuthSync/DisperalFiles/OutputFile/slp0.6_Rsk0.1_K100_var0.1_rn142.py.csv", na.strings = NA)

pdf("RuthSync/DisperalFiles/OutputFile/rGraph.pdf", width =10, height =10)


ByPopAge<- ddply(File, .(pop_age), summarise, # need to discount trials where no feeding obs and eve
		N = length(!is.na(colony_ID)),
		TotNumInd = sum(num.ads)

)


ggplot(data = ByPopAge, aes(x = pop_age, y = TotNumInd)) + geom_line() + geom_point()

dev.off()
