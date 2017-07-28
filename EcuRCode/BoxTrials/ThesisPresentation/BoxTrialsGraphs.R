# TODO: Add comment
# 
# Author: Ruth
###############################################################################

mytheme <-theme_bw(base_size=10)  + theme(plot.title = element_text(vjust=2), plot.margin=unit(c(0.08, 0.08, 0.0, 0.08),"cm"), 
				axis.title.y = element_text(vjust=0.50),
				axis.line = element_line(colour = "grey6", linetype=1, size = 0.3), 
				panel.border = element_blank(), 
				panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +  
		theme(strip.background = element_rect(fill = 'white'))




colorList <- c("orangered2", "dodgerblue1")

png("RuthSync/Thesis/Presentation/1_FeedingVsCond.png", width = 1000, height = 800, units = "px", res = 200)

ggplot(subset(BoxComboMorn, IndFeed != "NA"), aes(x = residCond, y = IndFeedNum, color = Treatment, linecolor = Treatment)) + 
		ylab("Individual fed?") +
		geom_point(aes(shape = Treatment),  size = 1, position = position_jitter(width = 0.00, height = 0.03)) + 
		stat_smooth(aes(color = Treatment), method="glm",   method.args = list(family = "binomial"), se=FALSE, size = 1) + 
		mytheme + xlab("Individual Condition") + 
		scale_shape_manual(values = c(16, 18), name = "Prey Size") +
		scale_color_manual(values = colorList, name = "Prey Size")

dev.off()