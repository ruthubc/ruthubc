# TODO: Add comment
# 
# Author: user
###############################################################################


library(plyr)
library(ggplot2)
require(reshape2)
library(nlme)
library(gridExtra)
require(scales)

mytheme <-theme_bw(base_size=30)  + theme(plot.title = element_text(vjust=2), panel.margin= unit(0.75, "lines"), 
		axis.title.y = element_text(vjust=0), plot.margin=unit(c(1,1,1.5,1.2),"cm"), 
		panel.border = element_rect(fill = NA, colour = "grey", linetype=1, size = 1)) +  theme(strip.background = element_rect(fill = 'white'))



### Box evenness vs prey size box plot


pdf("RuthEcuador2013/BoxFeedingTrials/Graphs/PaperGraphs.pdf", width =7, height =7)

ggplot(AveByTrial, aes(x= Treatment, y =PJEven)) + stat_boxplot(geom ='errorbar') + geom_boxplot() + 
		mytheme + ylab("Intragroup Evenness") + xlab("Prey Size") + ylim(0,1)


## Logistic plot of feed vs condition

ggplot(subset(BoxComboMorn, IndFeed != "NA"), aes(x = Cond.Scal, y = IndFeedNum, linetype = Treatment)) + geom_point(aes(shape = Treatment)) + 
		stat_smooth(method="glm", family="binomial", se=FALSE, colour = "black") + mytheme + xlab("Log Condition Scaled") + ylab("Individual Fed?") +
		scale_shape_discrete(name = "Prey Size") + scale_linetype_discrete(name = "Prey Size")


ggplot(subset(BoxComboMorn, IndFeed != "NA"), aes(x = Cond.Scal, y = IndCapNum, linetype = Treatment)) + geom_point(aes(shape = Treatment)) + 
		stat_smooth(method="glm", family="binomial", se=FALSE, colour = "black") + mytheme + xlab("Log Condition Scaled") + ylab("Individual Captured?")  +
		scale_shape_discrete(name = "Prey Size") + scale_linetype_discrete(name = "Prey Size")

## Prey capture 

CapVsEat <-subset(BoxCombo, select = c("FeedIndPos", "CaptureIndPos", "Treatment", "Instar", "LogHunger") )
CapVsEat <-na.omit(CapVsEat)
CapVsEat$FeedIndPos <- factor(CapVsEat$FeedIndPos, levels =c("y", "n"))
CapVsEat$FeedAndCap <- paste("Cap", CapVsEat$CaptureIndPos, "Feed", CapVsEat$FeedIndPos)



ggplot(data=CapVsEat, aes(x=CaptureIndPos, fill = FeedIndPos)) +
		geom_bar(stat="bin", position="fill") + xlab("Participated in Prey Capture")+ ylab("Percentage of Individuals That Fed") + 
		 facet_wrap(~Treatment) + scale_x_discrete(breaks=c("y", "n"), labels=c("Yes", "No")) +
		theme(axis.text=element_text(colour="black"), axis.title = element_blank()) +
		scale_fill_discrete(name = "Fed?", breaks = c("y", "n"),
				labels = c("Yes", "No"))  + mytheme    + scale_fill_manual("FeedIndPos", values = c("darkblue", "white")) +
		theme(legend.position = "none") + scale_y_continuous(labels = percent, expand = c(0.001,0.001) )



dev.off()
