# TODO: Add comment
# 
# Author: Ruth
###############################################################################


setwd("C:/Users/Ruth/Dropbox/")


mytheme <- theme_bw(base_size=10)  + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
		panel.border = element_rect(fill = NA, colour = "black", linetype=1, size = 0.5), 
		legend.background = element_rect(fill="white", size=0.5, linetype="solid", colour ="black"), legend.text=element_text(size=8), 
		strip.background = element_rect(fill="white", size=0.7, linetype="solid", colour ="black"))

Nests<- ddply(spidersMul, .(NestID), summarise, # need to discount trials where no feeding obs and eve
		NumAds = mean(CountFemales),
		logCtFm = mean(logCtFm))


png("RuthSync/Thesis/Presentation/2_NestSizeHistogram.png", width = 800, height = 600, units = "px", res = 200)


ggplot(Nests, aes(x= NumAds)) + geom_histogram(colour = "white") + theme_bw() + mytheme + xlab("Colony Size (# Adult Females)") +
		ylab("Number measured")

dev.off()