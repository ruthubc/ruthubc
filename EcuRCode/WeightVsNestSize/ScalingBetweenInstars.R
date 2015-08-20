# TODO: Add comment
# 
# Author: Ruth
###############################################################################


## scaling to plot on one graph from stackoverflow.com/questions/15836452/data-table-aggregations-that-return-vectors-such-as-scale
spidersSub <- subset(spiders, select = c(Instar, logCond))
spiders <- as.data.frame(ddply(spiders,"Instar",transform, Cond.Scal=zScore(logCond)))
ggplot(spiders, aes(Cond.Scal)) + geom_histogram() + facet_wrap(~Instar) # checking that the condition worked
spiders <- as.data.frame(ddply(spiders,"Instar",transform, Leg.Scal=zScore(logLeg)))
ggplot(spiders, aes(Leg.Scal)) + geom_histogram() + facet_wrap(~Instar) # checking that the condition worked
