# TODO: Add comment
# 
# Author: Ruth
###############################################################################


# constant variables
env1<- 0.0
no_off <- 6 

subDispFile <- subset(importDispFile, Comp_meas != 0.5)


aveFile <- ddply(subDispFile, .(ad_dsp_fd, Comp_meas), summarize, 
		metPopAgeMax = mean(metPopAgeMax))

png("RuthSync/Thesis/Presentation/2_compdispNoEnv.png", width = 1000, height = 800, units = "px", res = 200)




ggplot(aveFile, aes(x = as.factor(Comp_meas), y = as.factor(ad_dsp_fd), fill = metPopAgeMax))  + 
		geom_tile(colour = "darkgrey", size=0.1) +
		mytheme + ylab("Competition") +  xlab("adult dispersl") + 
		scale_fill_gradient(low="lightcyan", high="navyblue", name = "Metapopulation\nSurvival")

dev.off()