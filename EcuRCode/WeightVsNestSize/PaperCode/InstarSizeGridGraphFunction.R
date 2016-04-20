# Author: Ruth
###############################################################################
library (plyr)
library(ggplot2)
library(gridExtra)
library(RGraphics) 

# Function to make instar grid graphs

#gridGraphTheme <-  theme_bw(base_size=15)  + 
#		theme(plot.title = element_text(vjust=2), panel.margin= unit(0.75, "lines"), axis.title.y = element_text(vjust=0),
#		plot.margin=unit(c(1,1,1.5,1.2),"cm"), panel.border = element_rect(fill = NA, colour = "black", linetype=1, size = 1), panel.grid.major = element_blank(), panel.grid.minor = element_blank())
#


InstarGridGraph <- function(spiderData, variable, yaxisLabel, export,  fileName = "" ) {	
	
	if (export == "y") {
		outputpath <- paste("RuthEcuador2013/NestSize/Graphs/", fileName, ".pdf", sep = "")
		print(outputpath)
		pdf(outputpath, height=8, width=13)
		
	}
	
	column_index <- which(names(spiderData) == variable)
	
	if(length(column_index) == 0){ stop('variable not found - check spelling')}
	
	no_rows <- nrow(spiderData)

	
	MyPlots <- function(data, var, num_rows) { # function to make the graphs
		current.Instar <- as.character(unique(data$Instar)) # get the Name of the current subset
		
		data$variable <- data[,column_index]
		
		if (num_rows > 300) {
			dataSummary<- ddply(data, .(NestID, CountFemales), summarise,
					mean = mean(variable, na.rm = TRUE))
			p <- ggplot(dataSummary, aes(x = CountFemales , y = mean))	
			
		} else {
			#dataSummary <- data
			p <- ggplot(data, aes(x = CountFemales , y = relativeVar))		
		}
		
		
		p <- p + geom_point() + ggtitle(current.Instar) + 
				stat_smooth(method = "lm", formula = y~x, se = FALSE) + 
				scale_x_log10(limits = c(1, 6000 )) + theme_classic(base_size=15) + 
				theme(axis.title.x=element_blank(), axis.title.y=element_blank()) +
				theme(panel.border = element_rect(fill = NA, colour = "black", linetype=1, size = 1)) +
				theme(panel.margin= unit(0.0, "lines")) + theme(plot.title = element_text(size=15))
				
		
		return(p)  
	}
	
	# this creates a list of plots
	my.plots <- dlply(spiderData, .(Instar),        
			function(x) MyPlots(x, variable, no_rows))
	
	define_region <- function(row, col){
		viewport(layout.pos.row = row, layout.pos.col = col)
	} 
	
	
	grid.newpage()
	
	# Create layout (nrows, ncols), small bottow row for the axis title
	top.vp <-viewport(layout=grid.layout(5, 5,
					widths=unit(c(2,5,5,5,5),  "null"), # null is the unit. Not s
					heights=unit(c(5, 5, 5, 5,2), "null")))
	
	pushViewport(top.vp)
	
	
	# Arrange the plots  #(row, column)
	print(my.plots$Juv4, vp=define_region(2:3, 2))  
	print(my.plots$Sub1, vp = define_region(1:2, 3))
	print(my.plots$Sub2, vp = define_region(1:2, 4))
	print(my.plots$Adult, vp=define_region(1:2, 5))
	print(my.plots$SubMale, vp = define_region(3:4, 3))
	print(my.plots$AdMale, vp = define_region(3:4, 4))
	print((arrow(angle = 30, length = unit(0.25, "inches"),ends = "last", type = "open")), vp=define_region(1,2))
	
	
	# adding text
	grid.text('Females', vp=define_region(1,2), gp=gpar(fontsize=18))#x=1.1, y=0.1)# adds text to the plot
	grid.text('Males', vp=define_region(4,2), gp=gpar(fontsize=18))
	grid.text('Nest Size (Number of Adult Females)', vp=define_region(5,3:4), x=0.5, y=1, gp=gpar(fontsize=15))
	grid.text(yaxisLabel, vp=define_region(2:3,1), rot = 90, just = "top", x=0.8, y=0.5, gp=gpar(fontsize=15))
	#grid.lines(x = unit(c(0, 1), "npc"), y = unit(c(0, 1), "npc"), default.units = "npc")vp=define_region(1,2)
#	grid.lines(x = (c(0, 0)), y = (c(0.5, 0.5)), arrow = arrow(angle = 30, length = unit(0.5, "inches"),
#					ends = "last", type = "open"), vp=define_region(1,2))

	
	if (export == "y") {
		dev.off()
		print ("graph exported")
	}
	
}




InstarGridGraph(spidersMul, "logLeg", "yaxis", "n", "testGraph2")
