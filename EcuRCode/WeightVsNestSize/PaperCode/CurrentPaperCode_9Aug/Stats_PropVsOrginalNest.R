# TODO: Add comment
# 
# Author: Ruth
###############################################################################


nestsSigOnly<- subset(spiders, type == "multiple")
length(nestsSigOnly)


ggplot(Spis44, aes(x=type, y=logcond)) + geom_boxplot() + 
		ggtitle("Hunger 44.4 nests only ") + ylab("log Cond") + mytheme +theme(axis.title.x = element_blank()) +
		scale_x_discrete(breaks=c("multiple", "single"), labels=c("source: multiple", "propagules: single"))

ggplot(Spis44, aes(x=type, y=logLeg)) + geom_boxplot() +
		ggtitle("Leg Length 44.4 nests only ") + ylab("Log Leg Length") +
		mytheme +  theme(axis.title.x = element_blank()) +
		scale_x_discrete(breaks=c("multiple", "single"), labels=c("source: multiple", "propagules: single"))

