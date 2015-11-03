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
require(grid)

mytheme <-theme_bw(base_size=30)  + theme(plot.title = element_text(vjust=2), panel.margin= unit(0.75, "lines"), 
		axis.title.y = element_text(vjust=0), plot.margin=unit(c(1,1,1.5,1.2),"cm"), 
		panel.border = element_rect(fill = NA, colour = "grey", linetype=1, size = 1), panel.grid.major = element_blank()) +  
		theme(strip.background = element_rect(fill = 'white'))



### Box evenness vs prey size box plot


pdf("RuthEcuador2013/BoxFeedingTrials/Graphs/PaperGraphsNov1.pdf", width =10, height =10, unit)


# boxplot of evenness 
ggplot(AveByTrial, aes(x= Treatment, y =PJEven)) + stat_boxplot(geom ='errorbar') + geom_boxplot() + 
		mytheme + ylab("Intragroup Evenness") + xlab("Prey Size") + ylim(0,1) + annotate("text", x = 1.5, y = 0.99, label = "***") + 
		annotate("segment", x = 1, y = 0.97, xend = 2, yend = 0.97) + annotate("segment", x = 1, y = 0.97, xend = 1, yend = 0.94) +
		annotate("segment", x = 2, y = 0.97, xend = 2, yend = 0.94)


## Logistic plot of feed vs condition



ggplot(subset(BoxComboMorn, IndFeed != "NA"), aes(x = Cond.Scal, y = IndFeedNum, linetype = Treatment)) + ylab("Individual Fed?") +
		geom_point(aes(shape = Treatment),  size = 3, position = position_jitter(width = 0.00, height = 0.03)) + 
		stat_smooth(method="glm", family="binomial", se=FALSE, colour = "black", size = 1) + mytheme + xlab("Log Condition Scaled") + 
		scale_shape_manual(values = c(1, 17)) +	scale_linetype_manual(values = c(1, 5)) + ggtitle("(b)") + 
		theme(legend.position = "none", plot.title = element_text(size = 20, hjust = 0))
		

ggplot(subset(BoxComboMorn, IndFeed != "NA"), aes(x = Cond.Scal, y = IndCapNum, linetype = Treatment)) + geom_point(aes(shape = Treatment)) + 
		xlab("Log Condition Scaled") + ylab("Individual Captured?")  + geom_point(aes(shape = Treatment),  size = 3, position = position_jitter(width = 0.00, height = 0.03)) + 
		stat_smooth(method="glm", family="binomial", se=FALSE, colour = "black", size = 1) + mytheme + xlab("Log Condition Scaled") + 
		scale_shape_manual(values = c(1, 17)) + scale_linetype_manual(values = c(1, 5)) + 	ggtitle("(a)")  
		theme(legend.position = "none", plot.title = element_text(size = 20, hjust = 0)) + 

## Prey capture 

CapVsEat <-subset(BoxCombo, select = c("FeedIndPos", "CaptureIndPos", "Treatment", "Instar", "LogHunger") )
CapVsEat <-na.omit(CapVsEat)
CapVsEat$FeedIndPos <- factor(CapVsEat$FeedIndPos, levels =c("y", "n"))
CapVsEat$FeedAndCap <- paste("Cap", CapVsEat$CaptureIndPos, "Feed", CapVsEat$FeedIndPos)

CapVsEat$Treatment <- factor(CapVsEat$Treatment, levels = c("small", "large"))
CapVsEat$CaptureIndPos  <- factor(CapVsEat$CaptureIndPos, levels = c("y", "n"))

prey_names <- list(
		"small" = "small prey",
		"large" = "large prey"
		)
		
prey_labeller <- function(variable,value){
		return(prey_names[value])
		}
		


ggplot(data=CapVsEat, aes(x=CaptureIndPos, fill = FeedIndPos)) +
		geom_bar(stat="bin", position="fill", colour = "black") + xlab("Participated in Prey Capture")+ ylab("Percentage of Individuals") + 
		 facet_grid(~Treatment, labeller = prey_labeller) + 
		 scale_x_discrete(breaks=c("y", "n"), labels=c("Yes", "No")) +
		theme(axis.text=element_text(colour="black"), axis.title = element_blank()) +
		scale_fill_manual(values=c("dimgrey", "lightgrey"), name = "Fed?", breaks = c("y", "n"), labels = c("Yes", "No"))+
		mytheme + theme(strip.text.x = element_text(size = 20, vjust = 1), strip.background = element_rect(colour="white")) + 
		scale_y_continuous(limits = c(0, 1.06)) + 
		annotate("text", x = 1.5, y = 1.06, label = "***") + annotate("segment", x = 1, y = 1.05, xend = 2, yend = 1.05) + 
		annotate("segment", x = 1, y = 1.05, xend = 1, yend = 1.02) +
		annotate("segment", x = 2, y = 1.05, xend = 2, yend = 1.02) 


dev.off()

pdf("RuthEcuador2013/BoxFeedingTrials/Graphs/PaperGraphs_FeedOrCap.pdf", width =17, height =9, unit)

####  Cap or Feed
ggplot(subset(BoxComboMorn, CapAndFeed == "NC+E" | CapAndFeed == "C+E"), aes(x=CapAndFeed, y = Cond.Scal))  + stat_boxplot(geom ='errorbar') + geom_boxplot() + 
		facet_wrap(~Treatment) + stat_summary(fun.y=mean, colour="red", geom="point") +
		ylab("Scaled Condition") + xlab("") + mytheme + theme(axis.text.y=element_text(angle=45))# + + coord_flip() +
		#scale_x_discrete(breaks = c("NC+E", "C+NE", "C+E"),labels = c("Eat, No Capture", "No Eat, Capture", "Eat, Capture"))


ggplot(subset(BoxComboMorn, IndFeed == "y"), aes(x = Cond.Scal, y = IndCapNum, linetype = Treatment)) + geom_point(aes(shape = Treatment)) + 
		stat_smooth(method="glm", family="binomial", se=FALSE, colour = "black") + mytheme + xlab("Log Condition Scaled")  +
		scale_shape_discrete(name = "Prey Size") + scale_linetype_discrete(name = "Prey Size")



dev.off()
