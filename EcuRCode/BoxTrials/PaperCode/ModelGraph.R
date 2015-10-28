# TODO: Add comment
# 
# Author: user
###############################################################################

# good website with dets how to draw functions with ggplot
#https://kohske.wordpress.com/2010/12/25/draw-function-without-data-in-ggplot2/

library(ggplot2)
library(grid)

mytheme <-theme_bw(base_size=30)  + theme(legend.position="none", plot.title = element_text(vjust=2), panel.margin= unit(0.75, "lines"), 
		axis.title.y = element_text(vjust=0), plot.margin=unit(c(1,1,1.5,1.2),"cm"),  axis.ticks = element_blank(), 
		panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.text.y = element_blank(), axis.text.x = element_blank(),
		panel.border = element_rect(fill = NA, colour = "grey", linetype=1, size = 1)) +  theme(strip.background = element_rect(fill = 'white'))


## good constants chosen to illistrate my point!

A = 0.75
CostZero = 0.21
CostFun = function(x)(x * A) + CostZero

BenConst = 1.25
FdLmt = 0.75
LmInput = (FdLmt/BenConst) ^ (3/2)

BenFun = function(x)ifelse(x > LmInput, ((LmInput ^ (2/3)) * BenConst), ((x ^ (2/3)) * BenConst))
	
pdf("RuthEcuador2013/BoxFeedingTrials/Graphs/ModelGraph.pdf", width =10, height =10, unit)

ggplot(data.frame(x=c(0, 1)), aes(x)) + stat_function(fun=CostFun, aes(linetype = "Cost")) + stat_function(fun = BenFun, aes(linetype = "Benefit")) +
		xlab("Prey Surface Area") + ylab("Fitness") + geom_vline(xintercept = 0.72, linetype = 5, colour = "grey") + 
		geom_vline(xintercept = 0.115, linetype = 5, colour = "grey") + mytheme  + annotate("rect", xmin = 0.115, xmax= 0.72, ymin = 0, ymax = 1, alpha = 0.1) +
		scale_x_continuous(expand = c(0, 0)) + scale_y_continuous(expand = c(0, 0)) + annotate("text", x = 0.5, y = 0.53, label = "Costs", fontface = "bold", size = 6) + 
		annotate("text", x = 0.05, y = 0.35, label = "Very\nSmall\nPrey", size = 5, fontface= 'italic') + 
		annotate("text", x = 0.4, y = 0.3, label = "Small Prey", size = 5, fontface= 'italic') +
		annotate("text", x = 0.85, y = 0.3, label = "Large Prey", size = 5, fontface= 'italic') +
		annotate("text", x = 0.55, y = 0.78, label = "Benefits", fontface = "bold", size = 6)
		
#+ annotate("text", x = 0.115, y = 0.03, label = "PS min", size = 3.5 ) + annotate("text", x = 0.72, y = 0.03, label = "PS max", size = 3.5)
dev.off()