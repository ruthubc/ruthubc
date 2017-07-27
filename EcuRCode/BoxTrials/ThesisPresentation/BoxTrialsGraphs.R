# TODO: Add comment
# 
# Author: Ruth
###############################################################################


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