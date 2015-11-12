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

mytheme <-theme_bw(base_size=10)  + theme(plot.title = element_text(vjust=2), plot.margin=unit(c(0.08, 0.08, 0.0, 0.08),"cm"), 
		axis.title.y = element_text(vjust=0.50),
		axis.line = element_line(colour = "grey6", linetype=1, size = 0.3), panel.border = element_blank(), 
		panel.grid.major = element_blank()) +  
		theme(strip.background = element_rect(fill = 'white'))



### Box evenness vs prey size box plot


#pdf("RuthEcuador2013/BoxFeedingTrials/Graphs/PaperGraphsNov1.pdf", width =10, height =10, unit)

setEPS()


postscript("RuthEcuador2013/BoxFeedingTrials/Graphs/Sharpe_f1.eps",width = 2.76, height = 2.76)

# boxplot of evenness 
ggplot(AveByTrial, aes(x= Treatment, y =PJEven)) + stat_boxplot(geom ='errorbar') + geom_boxplot() + 
		mytheme + ylab("Intragroup evenness") + xlab("Prey size") + ylim(0,1) + annotate("text", x = 1.5, y = 0.99, label = "**") + 
		annotate("segment", x = 1, y = 0.97, xend = 2, yend = 0.97) + annotate("segment", x = 1, y = 0.97, xend = 1, yend = 0.94) +
		annotate("segment", x = 2, y = 0.97, xend = 2, yend = 0.94)


## Logistic plot of feed vs condition
dev.off()





postscript("RuthEcuador2013/BoxFeedingTrials/Graphs/Sharpe_f2b.eps",width = 2.76, height = 2.76)


ggplot(subset(BoxComboMorn, IndFeed != "NA"), aes(x = Cond.Scal, y = IndFeedNum, linetype = Treatment)) + ylab("Individual fed?") +
		geom_point(aes(shape = Treatment),  size = 1.2, position = position_jitter(width = 0.00, height = 0.03)) + 
		stat_smooth(method="glm", family="binomial", se=FALSE, colour = "black", size = 0.3) + mytheme + xlab("Log condition scaled") + 
		scale_shape_manual(values = c(1, 17)) +	scale_linetype_manual(values = c(1, 5))  + 
		theme(legend.position = "none", plot.title = element_text(size = 20, hjust = 0))

dev.off()
		


postscript("RuthEcuador2013/BoxFeedingTrials/Graphs/Sharpe_f2a.eps",width = 2.76, height = 2.76)


ggplot(subset(BoxComboMorn, IndFeed != "NA"), aes(x = Cond.Scal, y = IndCapNum, linetype = Treatment)) + ylab("Individual captured?") +
		geom_point(aes(shape = Treatment),  size = 1.2, position = position_jitter(width = 0.00, height = 0.03)) + 
		stat_smooth(method="glm", family="binomial", se=FALSE, colour = "black", size = 0.3) + mytheme + xlab("Log condition scaled") + 
		scale_shape_manual(values = c(1, 17)) +	scale_linetype_manual(values = c(1, 5))  + 
		theme(legend.position = "none", plot.title = element_text(size = 20, hjust = 0))


dev.off()

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
		

postscript("RuthEcuador2013/BoxFeedingTrials/Graphs/Sharpe_f3.eps",width = 2.76, height = 2.76)

ggplot(data=CapVsEat, aes(x=CaptureIndPos, fill = FeedIndPos)) +
		geom_bar(stat="bin", position="fill", colour = "black") + xlab("Participated in prey capture")+ 
		ylab("Percentage of individuals") + 
		 facet_grid(~Treatment, labeller = prey_labeller) + 
		 scale_x_discrete(breaks=c("y", "n"), labels=c("Yes", "No")) +
		theme(axis.text=element_text(colour="black"), axis.title = element_blank()) +
		scale_fill_manual(values=c("dimgrey", "lightgrey"), guide = FALSE) +
		mytheme + theme(strip.text.x = element_text(size = 8, vjust = 0.5), strip.background = element_rect(colour="white")) + 
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
