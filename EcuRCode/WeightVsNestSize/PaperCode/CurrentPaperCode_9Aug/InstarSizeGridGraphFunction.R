

# Function to make instar grid graphs

#gridGraphTheme <-  theme_bw(base_size=15)  + 
#		theme(plot.title = element_text(vjust=2), panel.margin= unit(0.75, "lines"), axis.title.y = element_text(vjust=0),
#		plot.margin=unit(c(1,1,1.5,1.2),"cm"), panel.border = element_rect(fill = NA, colour = "black", linetype=1, size = 1), panel.grid.major = element_blank(), panel.grid.minor = element_blank())
#

MyPlots <- function(data, num_rows, model, minNstSz, same_y_axis, minVarValue, maxVarValue, predictDF) { # function to make the graphs
	current.Instar <- as.character(unique(data$Instar)) # get the Name of the current subset	
	
	
	if (num_rows > 300) {  # summarise data instead of plotting individual points
		#24th August changed from dataSummary
		dataSummary<- ddply(data, .(NestID, CountFemales), summarise, mean = mean(variable, na.rm = TRUE))

		p <- ggplot(dataSummary, aes(x = CountFemales , y = mean))	
	}else{
		p <- ggplot(data, aes(x = CountFemales , y = variable)) 
	}
	

	if (same_y_axis != "n") {

		p <- p + scale_y_continuous(limits = c(minVarValue, maxVarValue))
		
	} else {
		p
		
	}
		
	
	p <- p + geom_point() + ggtitle(current.Instar) + 				
			scale_x_log10(limits = c(minNstSz, 6000 )) + theme_classic(base_size=20) + 
			theme(axis.title.x=element_blank(), axis.title.y=element_blank()) +
			theme(panel.border = element_rect(fill = NA, colour = "black", linetype=1, size = 1)) +
			theme(panel.margin= unit(0.0, "lines")) + theme(plot.title = element_text(size=20))
	
	
	if (typeof(model) == "character") { # no model specified
		
		p <- p + stat_smooth(method = "lm", formula = y~x, se = FALSE)
		
	} else {
		
		
		p <- tryCatch(
				{
					#Vis_fit <- (visreg(model, "logCtFm", by = "Instar", plot = FALSE))$fit						
					#Vis_fit <- subset(Vis_fit, Instar == current.Instar)
					fit <- subset(predictDF, Instar == current.Instar)
					p <- p +  geom_line(data = fit, aes(x = (10^logCtFm), y= lmrPrd))
				},
				error=function(cond) {
					return(p + stat_smooth(method = "lm", formula = y~x, se = FALSE))
				})
		
		
	}
	
	
	return(p)  
}
	


InstarGridGraph <- function(spiderData, variable, yaxisLabel, export ="n",  fileName = "", model = "noModel", same_y_axis = "n" ) {	
	cat("Note: If line on graph is blue R could not plot the lmer, plotting a simple lm instead")
	# whether or not to export the data
	if (export == "y") {
		setwd("C:/Work/Dropbox/")
		outputpath <- paste("RuthEcuador2013/NestSize/Graphs/", fileName, ".pdf", sep = "")
		print(outputpath)
		pdf(outputpath, height=8, width=13)
		
	}
	
	column_index <- which(names(spiderData) == variable)
	
	### error checking for input variable
	if(length(column_index) == 0){ stop('variable not found - check spelling')}
	
	no_rows <- nrow(spiderData)
	
	minNstSz <- min(spiderData$CountFemales)
	
	spiderData$variable <- spiderData[,column_index]
	
	### Making table to plot the results of the model
	
	
	
	p <- tryCatch(
			{
				if (typeof(model) == "character") {
					predictDF <- ""
					print("no model selected")
					
				} else if(class(model)[1] == "glmmPQL") {
					
					print("glmmpql")
					
					predictDF <- spiderData
					
					predictDF$lmrPrd <- predict(model, predictDF, type = "response")
					
				}else{
					print("lmer")
					predictDF <- expand.grid(logCtFm = seq(min(spiderData$logCtFm), max(spiderData$logCtFm), by = 0.1), 
							InstarNumber = c(4, 5, 6, 7), InstarSex = c("M", "F"), NestID = c("44.4EX12"))
					
					predictDF <- merge(predictDF, InstarLookUp, by = c("InstarNumber", "InstarSex"))
					
					predictDF$lmrPrd <- predict(model, predictDF)
					
				}			
				
				
				
			},
			error=function(cond) {
				predictDF <- ""
			})
	

	
	
	
	if (no_rows > 300) {
	
		dataSum<- ddply(spiderData, .(NestID, CountFemales, Instar), summarise, mean = mean(variable, na.rm = TRUE))
		
		minVarValue <- min(dataSum$mean, na.rm = TRUE)
		maxVarValue <- max(dataSum$mean, na.rm = TRUE)
	}else{
		
		minVarValue <- min(spiderData$variable, na.rm = TRUE)
		maxVarValue <- max(spiderData$variable, na.rm = TRUE)
	}
	
	

	

	
	# this creates a list of plots
	my.plots <- dlply(spiderData, .(Instar),        
			function(x) MyPlots(x, no_rows, model, minNstSz, same_y_axis, minVarValue, maxVarValue, predictDF))
	
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
	
	
	# adding text
	grid.text('Females', vp=define_region(1,2), gp=gpar(fontsize=18))#x=1.1, y=0.1)# adds text to the plot
	grid.text('  Males', vp=define_region(4,2), gp=gpar(fontsize=18))
	grid.text('Colony Size (Number of Adult Females)', vp=define_region(5,3:4), x=0.5, y=1, gp=gpar(fontsize=18))
	grid.text(yaxisLabel, vp=define_region(2:3,1), rot = 90, just = "top", x=0.8, y=0.5, gp=gpar(fontsize=18))

	## Arrows on graph
	arrow_gp <- gpar(fill = TRUE, lwd = 3)
	arrow_vars <- arrow(angle = 30, length = unit(0.1, "inches"), type = 'closed')
	x_coors = (c(0.75, 0.95))
	grid.lines(x = x_coors, y = (c(0.5, 0.4)), gp = arrow_gp, 
		arrow = arrow_vars, vp=define_region(1,2))
	grid.lines(x = x_coors, y = (c(0.5, 0.6)), gp = arrow_gp, 
		arrow = arrow_vars, vp=define_region(4,2))



				


	if (export == "y") {
		dev.off()
		print ("graph exported")
	}
}


#Testing

#InstarGridGraph(spidersMul, "logLeg", "testing wiht visreg", "n", "_testGraph2",  "NoModel", same_y_axis = "n")


