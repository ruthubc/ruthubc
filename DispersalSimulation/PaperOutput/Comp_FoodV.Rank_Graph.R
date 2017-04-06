# TODO: Add comment
# 
# Author: Ruth
###############################################################################


library("ggplot2")

mytheme <- theme_bw(base_size=7)  + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
		panel.border = element_rect(fill = NA, colour = "black", linetype=1, size = 0.5), 
		legend.background = element_rect(fill="white", size=0.7, linetype="solid", colour ="black"), legend.text=element_text(size=6), 
		strip.background = element_rect(fill="white", size=0.7, linetype="solid", colour ="black"))


compFun <- function(comp, rnk){
	slp <- comp/numJuvs
	food <- (1 + (slp*medRnk/xbr^2) * (xbr - ((xbr/medRnk)*rnk))) * xbr
	
	food <- ifelse(food < 0, 0, food)
	food <- ifelse(food > 1, 1, food)
	food <- ifelse(comp == 0, xbr, food)
	food <- ifelse(comp == 10 & rnk <= medRnk, 1, food)
	food <- ifelse(comp == 10 & rnk > medRnk, 0, food)
	

	
	return(food)
	
}

numJuvs <- 100
medRnk <- 50
xbr <- 0.5
rankVect <- c(0:(numJuvs))
compVect <- c(0, 0.4, 0.8, 1.25, 2.5, 10)
# add full contest and scramble

grid <- expand.grid(rankVect, compVect)

df <- data.frame(rank = grid$Var1, comp = grid$Var2)

df$food <- compFun(df$comp, df$rank)

CompLookUp <- data.frame (comp = c(0, 0.2, 0.4, 0.6, 0.8, 1, 1.25, 1.33, 2.5, 5, 10), 
		Comp_meas = c(0.0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1.0))

df <- merge(df, CompLookUp, by = "comp")

setwd("~")
getwd()

png(filename = "CompGraph.png", width = 900, height = 550, units = "px", res = 200, pointsize = 10)

ggplot(df, aes(x = rank, y = food, linetype = as.factor(Comp_meas))) +
		geom_line() + mytheme + ylab("Individual food allocation") + xlab("Rank of individual") +
		scale_linetype_discrete(name = "Competition\nmeasure")

dev.off()
