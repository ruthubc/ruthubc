# TODO: Add comment
# 
# Author: user
###############################################################################


## Box plot of box evenness

pdf("RuthEcuador2013/BoxFeedingTrials/Graphs/IgniteGraphs.pdf", width =8, height =6.5, unit)



mm <- ddply(AveByTrial, "Treatment", summarise, PJEven = mean(PJEven),
		len = length(Treatment), SE= (sd(AsinPJEven, na.rm = TRUE)/ sqrt(len)))

mm$Treatment <- factor(mm$Treatment, levels = c("small", "large"))

ggplot(mm, aes(x = factor(Treatment), y = PJEven)) + geom_bar(stat = "identity") + mytheme + 
		geom_bar(fill="dark green", colour="dark green") +
		geom_errorbar(aes(ymin=PJEven-SE, ymax=PJEven+SE), width=.2, position=position_dodge(.9)) +
		xlab ("") + ylab("Amount Food is Shared") + scale_y_continuous(expand = c(0,0), limits = c(0, 0.75))



ff <- ddply(BoxComboMorn, c("Treatment", "FeedIndPos"), summarise, Hunger = mean(Hunger, na.rm=TRUE),
		len = length(Treatment), SE= (sd(LogHunger, na.rm = TRUE)/ sqrt(len)))

ff <- subset(ff, FeedIndPos!="<NA>")

ff$Treatment <- factor(ff$Treatment, levels = c("small", "large"))

ggplot(ff, aes(x = factor(Treatment), y = Hunger, fill = FeedIndPos)) + geom_bar(position=position_dodge(), stat = "identity") + 
		mytheme + guides(fill=FALSE) + xlab("") + scale_y_continuous(expand = c(0,0), limits = c(0, 0.37)) + 
		geom_errorbar(aes(ymin=Hunger-SE, ymax=Hunger+SE), width=.2, position=position_dodge(.9)) + ylab("Average Hunger of Spiders")


dev.off()