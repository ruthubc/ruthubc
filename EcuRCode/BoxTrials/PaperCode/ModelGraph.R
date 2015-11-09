# TODO: Add comment
# 
# Author: user
###############################################################################

# good website with dets how to draw functions with ggplot
#https://kohske.wordpress.com/2010/12/25/draw-function-without-data-in-ggplot2/

library(ggplot2)
library(grid)
library(Cairo) # not sure if i need this

mytheme <-theme_bw(base_size=30)  + theme(legend.position="none", plot.title = element_text(vjust=2), panel.margin= unit(0.75, "lines"), 
		axis.title.y = element_text(vjust=0), plot.margin=unit(c(1,1,1.5,1.2),"cm"),  axis.ticks = element_blank(), 
		panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.text.y = element_blank(), axis.text.x = element_blank(),
		panel.border = element_rect(fill = NA, colour = "grey", linetype=1, size = 1)) +  theme(strip.background = element_rect(fill = 'white'))


## good constants chosen to illistrate my point!

A = 0.85
CostZero = 0.10
CostFun = function(x)(A * x ^(3/2)) + CostZero

BenConst = 1.1
LmInput = 0.7

BenFun = function(x)ifelse(x > LmInput, (LmInput * BenConst), (x * BenConst))
pMin = 0.125
pMax = 0.851

setEPS()
	
#pdf("RuthEcuador2013/BoxFeedingTrials/Graphs/ModelGraph.pdf", width =10, height =10, unit)

postscript("RuthEcuador2013/BoxFeedingTrials/Graphs/ModelGraph.eps",width =10, height =10)

#postscript is the other option but can't find way to increase resolution
# CairoPS but makes PS files not eps files
#http://thepoliticalmethodologist.com/2013/11/25/making-high-resolution-graphics-for-academic-publishing/

ggplot(data.frame(x=c(0, 1.1)), aes(x)) +  annotate("rect", xmin = pMin, xmax= pMax, ymin = 0, ymax = 1.1, fill = "light grey") +
		stat_function(fun=CostFun, aes(linetype = "Cost")) + stat_function(fun = BenFun, aes(linetype = "Benefit")) +
		xlab("Prey Volume") + ylab("Fitness") + geom_vline(xintercept = pMax, linetype = 5, colour = "grey") + 
		geom_vline(xintercept = pMin, linetype = 5, colour = "grey") + mytheme   +
		scale_x_continuous(expand = c(0, 0)) + scale_y_continuous(expand = c(0, 0)) + annotate("text", x = 0.7, y = 0.54, label = "Costs", fontface = "bold", size = 6) + 
		annotate("text", x = 0.06, y = 0.25, label = "Very\nSmall\nPrey", size = 6, fontface= 'italic') + 
		annotate("text", x = 0.5, y = 0.25, label = "Small Prey", size = 5.5, fontface= 'italic') +
		annotate("text", x = 0.97, y = 0.25, label = "Large Prey", size = 5.5, fontface= 'italic') +
		annotate("text", x = 0.5, y = 0.65, label = "Benefits", fontface = "bold", size = 5.5)
		
#+ annotate("text", x = 0.115, y = 0.03, label = "PS min", size = 3.5 ) + annotate("text", x = 0.72, y = 0.03, label = "PS max", size = 3.5)
dev.off()