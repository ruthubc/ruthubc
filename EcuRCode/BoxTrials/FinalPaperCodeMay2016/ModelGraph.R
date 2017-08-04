# TODO: Add comment
# 
# Author: user
###############################################################################

# good website with dets how to draw functions with ggplot
#https://kohske.wordpress.com/2010/12/25/draw-function-without-data-in-ggplot2/

setwd("C:/Users/Ruth/Dropbox")

library(ggplot2)
library(grid)

ModGraphTheme <-theme_bw(base_size=15)  + theme(legend.position="none", plot.title = element_text(vjust=2), 
		axis.title.y = element_text(vjust=0.1, size = 20), axis.title.x = element_text(vjust = 0.95, size = 20),  axis.ticks = element_blank(),
		panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.border = element_blank(),  axis.text.y = element_blank(), 
		axis.text.x = element_blank(),
		axis.line = element_line(colour = "grey4", linetype=1, size = 0.3)) +  theme(strip.background = element_rect(fill = 'white'))


## good constants chosen to illistrate my point!

A = 0.85
CostZero = 0.2
CostFun = function(x)(A * x ^(2/3)) + CostZero

BenConst = 0.35 # initial intake rate
FdMax = 1.5

BenFun = function(x)(FdMax * x)/ (BenConst  + x)
pMin = 0.1413059# min x of grey box
pMax = 1.199797 # mas x of grey box
anTxSz = 6
smp = (pMax-pMin)/2 + pMin

#setEPS()
	
pdf("RuthEcuador2013/BoxFeedingTrials/Graphs/ModelGraph.pdf", width =10, height =10)

#postscript("RuthEcuador2013/BoxFeedingTrials/Graphs/Sharpe_f4_newModel.eps",width = 2.76, height = 2.76)

#postscript is the other option but can't find way to increase resolution
# CairoPS but makes PS files not eps files
#http://thepoliticalmethodologist.com/2013/11/25/making-high-resolution-graphics-for-academic-publishing/



ggplot(data.frame(x=c(0, 1.55)), aes(x)) +  annotate("rect", xmin = pMin, xmax= pMax, ymin = 0, ymax = 1.32, fill = "light grey") +
		stat_function(fun=CostFun, aes(linetype = "Cost"), size = 0.4) + 
		stat_function(fun = BenFun, aes(linetype = "Benefit"), size = 0.4) +
		xlab("Prey volume") + ylab("Fitness (benefits - costs)") + ModGraphTheme  +
		scale_x_continuous(expand = c(0, 0)) + scale_y_continuous(expand = c(0, 0)) +
		annotate("text", x = 0.60, y = 0.70, label = "Costs", fontface = "bold", size = anTxSz +0.2) + 
		annotate("text", x = 0.064, y = 0.52, label = "tiny\nprey", size = anTxSz, fontface= 'italic') + 
		annotate("text", x = smp, y = 0.47, label = "small prey", size = anTxSz, fontface= 'italic') +
		annotate("text", x = 1.35, y = 0.47, label = "large prey", size = anTxSz, fontface= 'italic') +
		annotate("text", x = 0.50, y = 1, label = "Benefits", fontface = "bold", size = anTxSz + 0.2)

# geom_vline(xintercept = pMin, linetype = 3, colour = "grey31", size = 0.5)
#geom_vline(xintercept = pMax, linetype = 3, colour = "grey31", size = 0.5) + 
#+ annotate("text", x = 0.115, y = 0.03, label = "PS min", size = 3.5 ) + annotate("text", x = 0.72, y = 0.03, label = "PS max", size = 3.5)
dev.off()