# TODO: Add comment
# 
# Author: user
###############################################################################

# good website with dets how to draw functions with ggplot
#https://kohske.wordpress.com/2010/12/25/draw-function-without-data-in-ggplot2/

library(ggplot2)
library(grid)

mytheme <-theme_bw(base_size=10)  + theme(legend.position="none", plot.title = element_text(vjust=2), 
		axis.title.y = element_text(vjust=0.80, size = 10), axis.title.x = element_text(vjust = 0.95, size = 10), plot.margin=unit(c(0.08, 0.08, -0.05, -0.15),"cm"),  axis.ticks = element_blank(),
		panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.border = element_blank(),  axis.text.y = element_blank(), axis.text.x = element_blank(),
		axis.line = element_line(colour = "grey4", linetype=1, size = 0.3)) +  theme(strip.background = element_rect(fill = 'white'))


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

postscript("RuthEcuador2013/BoxFeedingTrials/Graphs/Sharpe_f4.eps",width = 2.76, height = 2.76)

#postscript is the other option but can't find way to increase resolution
# CairoPS but makes PS files not eps files
#http://thepoliticalmethodologist.com/2013/11/25/making-high-resolution-graphics-for-academic-publishing/

anTxSz = 2.5

ggplot(data.frame(x=c(0, 1.1)), aes(x)) +  annotate("rect", xmin = pMin, xmax= pMax, ymin = 0, ymax = 1.1, fill = "light grey") +
		stat_function(fun=CostFun, aes(linetype = "Cost")) + stat_function(fun = BenFun, aes(linetype = "Benefit")) +
		xlab("Prey Volume") + ylab("Fitness") + mytheme   +
		scale_x_continuous(expand = c(0, 0)) + scale_y_continuous(expand = c(0, 0)) + annotate("text", x = 0.7, y = 0.48, label = "Costs", fontface = "bold", size = anTxSz +0.5) + 
		annotate("text", x = 0.06, y = 0.25, label = "very\nsmall\nprey", size = anTxSz, fontface= 'italic') + 
		annotate("text", x = 0.5, y = 0.25, label = "small prey", size = anTxSz, fontface= 'italic') +
		annotate("text", x = 0.97, y = 0.25, label = "large prey", size = anTxSz, fontface= 'italic') +
		annotate("text", x = 0.5, y = 0.7, label = "Benefits", fontface = "bold", size = anTxSz + 0.5)

# geom_vline(xintercept = pMin, linetype = 3, colour = "grey31", size = 0.5)
#geom_vline(xintercept = pMax, linetype = 3, colour = "grey31", size = 0.5) + 
#+ annotate("text", x = 0.115, y = 0.03, label = "PS min", size = 3.5 ) + annotate("text", x = 0.72, y = 0.03, label = "PS max", size = 3.5)
dev.off()