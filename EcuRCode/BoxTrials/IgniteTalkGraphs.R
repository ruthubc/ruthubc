# TODO: Add comment
# 
# Author: user
###############################################################################


## Box plot of box evenness

pdf("RuthEcuador2013/BoxFeedingTrials/Graphs/IgniteGraphs.pdf", width =10, height =10, unit)


mm <- ddply(AveByTrial, "Treatment", summarise, PJEven = mean(PJEven),
		len = length(Treatment), SE= (sd(AsinPJEven, na.rm = TRUE)/ sqrt(len)))

ggplot(mm, aes(x = factor(Treatment), y = PJEven)) + geom_bar(stat = "identity") + mytheme + 
		geom_bar(fill="dark green", colour="dark green") + ylim(0, 1) + 
		geom_errorbar(aes(ymin=PJEven-SE, ymax=PJEven+SE), width=.2, position=position_dodge(.9)) +
		xlab ("Prey Size") + ylab("")



ff <- ddply(BoxComboMorn, c("Treatment", "FeedIndPos"), summarise, Hunger = mean(Hunger, na.rm=TRUE),
		len = length(Treatment), SE= (sd(LogHunger, na.rm = TRUE)/ sqrt(len)))

ff <- subset(ff, FeedIndPos!="<NA>")

ggplot(ff, aes(x = factor(Treatment), y = Hunger, fill = FeedIndPos)) + geom_bar(position=position_dodge(), stat = "identity") + 
		mytheme + guides(fill=FALSE) + xlab("Prey Size") + 
		geom_errorbar(aes(ymin=Hunger-SE, ymax=Hunger+SE), width=.2, position=position_dodge(.9))


dev.off()