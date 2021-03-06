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
		axis.line = element_line(colour = "grey6", linetype=1, size = 0.3), 
		panel.border = element_blank(), 
		panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +  
		theme(strip.background = element_rect(fill = 'white'))



### Box evenness vs prey size box plot




setEPS()


postscript("RuthEcuador2013/BoxFeedingTrials/Graphs/Sharpe_f2.eps",width = 2.76, height = 2.76)

# boxplot of evenness 
ggplot(AveByTrial, aes(x= Treatment, y =PJEven)) + stat_boxplot(geom ='errorbar') + geom_boxplot() + 
		mytheme + ylab("Intragroup evenness") + xlab("Prey size") + ylim(0,1) + annotate("text", x = 1.5, y = 0.99, label = "**") + 
		annotate("segment", x = 1, y = 0.97, xend = 2, yend = 0.97) + annotate("segment", x = 1, y = 0.97, xend = 1, yend = 0.94) +
		annotate("segment", x = 2, y = 0.97, xend = 2, yend = 0.94)



dev.off()



## Logistic plot of feed vs condition

postscript("RuthEcuador2013/BoxFeedingTrials/Graphs/Sharpe_f3b.eps",width = 2.76, height = 2.76)


ggplot(subset(BoxComboMorn, IndFeed != "NA"), aes(x = residCond, y = IndFeedNum, linetype = Treatment)) + ylab("Individual fed?") +
		geom_point(aes(shape = Treatment),  size = 1.2, position = position_jitter(width = 0.00, height = 0.03)) + 
		stat_smooth(method="glm",  family = "binomial", se=FALSE, colour = "black", size = 0.3) + mytheme + xlab("Individual Condition") + 
		scale_shape_manual(values = c(1, 17)) +	scale_linetype_manual(values = c(1, 5))  + 
		theme(legend.position = "none", plot.title = element_text(size = 20, hjust = 0))

dev.off()
		


postscript("RuthEcuador2013/BoxFeedingTrials/Graphs/Sharpe_f3a.eps",width = 2.76, height = 2.76)


ggplot(subset(BoxComboMorn, IndFeed != "NA"), aes(x = residCond, y = IndCapNum, linetype = Treatment)) + ylab("Individual captured?") +
		geom_point(aes(shape = Treatment),  size = 1.2, position = position_jitter(width = 0.00, height = 0.03)) + 
		stat_smooth(method="glm",  family = "binomial", se=FALSE, colour = "black", size = 0.3) + mytheme + xlab("Individual Condition") + 
		scale_shape_manual(values = c(1, 17)) +	scale_linetype_manual(values = c(1, 5))  + 
		theme(legend.position = "none", plot.title = element_text(size = 20, hjust = 0))


dev.off()

## Prey capture 

CapVsEat <-subset(BoxComboMorn, select = c("FeedIndPos", "CaptureIndPos", "Treatment", "Instar") )
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
		

postscript("RuthEcuador2013/BoxFeedingTrials/Graphs/Sharpe_f4.eps",width = 2.76, height = 2.76)

ggplot(data=CapVsEat, aes(x=CaptureIndPos, fill = FeedIndPos)) +
		geom_bar(stat="bin", position="fill", colour = "black", size=0.1) + xlab("Participated in prey capture")+ 
		ylab("% individuals that fed") + 
		 facet_grid(~Treatment, labeller = prey_labeller) + 
		 scale_x_discrete(breaks=c("y", "n"), labels=c("Yes", "No")) +
		theme(axis.text=element_text(colour="black"), axis.title = element_blank()) +
		scale_fill_manual(values=c("dimgrey", "lightgrey"), guide = FALSE) +
		mytheme + theme(strip.text.x = element_text(size = 8, vjust = 0.5), strip.background = element_rect(colour="white")) + 
		scale_y_continuous(limits = c(0, 1.06)) + 
		annotate("text", x = 1.5, y = 1.06, label = "***", size = 3) + annotate("segment", x = 1, y = 1.05, xend = 2, yend = 1.05, size = 0.2) + 
		annotate("segment", x = 1, y = 1.051, xend = 1, yend = 1.02, size = 0.2) +
		annotate("segment", x = 2, y = 1.051, xend = 2, yend = 1.02, size = 0.2) 


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


############# Histogram of individual residual conditions 
# Run Residual Condition code


hist_theme <-theme_bw(base_size=17)  + theme(axis.title.y = element_text(vjust=0.50),
				axis.line = element_line(colour = "grey6", linetype=1, size = 0.3), panel.border = element_blank(), 
				panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +  
				theme(strip.background = element_rect(fill = 'white'))

LegendFormat <- scale_fill_grey(start = 0, end = .4, breaks=c("Sub1", "Sub2"), labels=c("Subadult1", "Subadult2"))

axis_x_Gap <- scale_x_continuous(expand = c(0.01,0.01))
axis_y_Gap <- scale_y_continuous(expand = c(0,0)) 

png("RuthEcuador2013/BoxFeedingTrials/Graphs/Appendix_RawWeights_Histogram.png", width = 600, height = 400, units = "px")



ggplot(Weights, aes(Weight.1, fill = Instar)) + geom_histogram(lty = 0) + hist_theme + scale_fill_grey(start = 0, end = .4) + 
		xlab("Individual Weights (mg)") + ylab("Number of Individuals") + LegendFormat + axis_x_Gap + axis_y_Gap + geom_histogram(colour = "white", show_guide = FALSE)

dev.off()



png("RuthEcuador2013/BoxFeedingTrials/Graphs/Appendix_ResidualCond_Histogram.png", width = 600, height = 400, units = "px")



ggplot(Weights, aes(condResiduals, fill = Instar)) + geom_histogram(lty = 0) + hist_theme + scale_fill_grey(start = 0, end = .4) + 
		xlab("Residual Condition Index") + ylab("Number of Individuals") + LegendFormat + axis_x_Gap + axis_y_Gap + geom_histogram(colour = "white", show_guide = FALSE)

dev.off()

png("RuthEcuador2013/BoxFeedingTrials/Graphs/Appendix_LogCond_Histogram.png", width = 600, height = 400, units = "px")


ggplot(Weights, aes(LogRatioCond, fill = Instar)) + geom_histogram(lty = 0) + hist_theme + 
		xlab("Log of Ratio Index") + ylab("Number of Individuals") + axis_x_Gap + axis_y_Gap + geom_histogram(colour = "white", show_guide = FALSE)


dev.off()
