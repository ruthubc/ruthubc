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

Facet_label <- function(var, value){
	value <- as.character(value)
	if (var=="Treatment") { 
		value[value=="large"] <- "Large Prey"
		value[value=="small"]   <- "Small Prey"
	} else if (var=="Instar") {
		value[value=="Sub1"] <- "Subadult 1"
		value[value=="Sub2"]   <- "Subadult 2"
	}
	return(value)
}


### Box evenness vs prey size box plot


pdf("RuthEcuador2013/BoxFeedingTrials/Graphs/PaperGraphs.pdf", width = 10, height = 10)

ggplot(AveByTrial, aes(x= Treatment, y =PJEven)) + stat_boxplot(geom ='errorbar') + geom_boxplot() + 
		mytheme + ylab("Intragroup Evenness") + xlab("Prey Size") + ylim(0,1)


## Logistic plot of feed vs condition

ggplot(subset(BoxComboMorn, IndFeed != "NA"), aes(x = Cond.Scal, y = IndFeedNum, linetype = Treatment)) + geom_point(aes(shape = Treatment)) + 
		stat_smooth(method="glm", family="binomial", se=FALSE, colour = "black") + mytheme + xlab("Log Condition Scaled") + ylab("Fed")


ggplot(subset(BoxComboMorn, IndFeed != "NA"), aes(x = Cond.Scal, y = IndCapNum, linetype = Treatment)) + geom_point(aes(shape = Treatment)) + 
		stat_smooth(method="glm", family="binomial", se=FALSE, colour = "black") + mytheme + xlab("Log Condition Scaled") + ylab("Captured")

## Prey capture 

CapVsEat <-subset(BoxCombo, select = c("FeedIndPos", "CaptureIndPos", "Treatment", "Instar", "LogHunger") )
CapVsEat <-na.omit(CapVsEat)
CapVsEat$FeedIndPos <- factor(CapVsEat$FeedIndPos, levels =c("y", "n"))
CapVsEat$FeedAndCap <- paste("Cap", CapVsEat$CaptureIndPos, "Feed", CapVsEat$FeedIndPos)



ggplot(data=CapVsEat, aes(x=CaptureIndPos, fill = FeedIndPos)) +
		geom_bar(stat="bin", position="fill") + xlab("Participated in Prey Capture")+ ylab("Percentage of Individuals That Fed") + 
		scale_x_discrete(breaks=c("y", "n"), labels=c("Yes", "No")) + facet_wrap(~Treatment) 
		theme(axis.text=element_text(colour="black"), axis.title = element_blank()) +
		scale_fill_discrete(name = "Fed?", breaks = c("y", "n"),
				labels = c("Yes", "No"))  + mytheme    + scale_fill_manual("FeedIndPos", values = c("darkblue", "white")) +
		theme(legend.position = "none") + scale_y_continuous(labels = percent, expand = c(0.001,0.001) )

dev.off()
