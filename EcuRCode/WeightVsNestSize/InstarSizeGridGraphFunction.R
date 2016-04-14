# Author: Ruth
###############################################################################


# Function to make instar grid graphs


InstarGridGraph <- function(spiderData, variable) {
	
	#pdf("RuthEcuador2013/NestSize/Graphs/Test.pdf", height=8, width=13)
	
	column_index <- which(names(spiderData) == var)
	
	if(column_index == 0) stop('variable not found - check spelling')



	
	MyPlots <- function(data, var) { # function to make the graphs
		current.Instar <- as.character(unique(data$Instar)) # get the Name of the current subset
		
		p <- ggplot(data, aes(x = CountFemales , y = data[[var]])) + geom_point() + ggtitle(current.Instar) + 
				theme(axis.title.x=element_blank(), axis.title.y=element_blank())
		
		return(p)  
	}
	
	# this creates a list of plots
	my.plots <- dlply(spidersMul, .(Instar),        
			function(x) MyPlots(x, variable))
	
	define_region <- function(row, col){
		viewport(layout.pos.row = row, layout.pos.col = col)
	} 
	
	
	grid.newpage()
	
	# Create layout (nrows, ncols), small bottow row for the axis title
	top.vp <-viewport(layout=grid.layout(5, 4,
					widths=unit(c(1,1,1,1),  "null"), # null is the unit. Not s
					heights=unit(c(5, 5, 5, 5,1), "null")))
	
	pushViewport(top.vp)
	
	
	# Arrange the plots  #(row, column)
	print(my.plots$Juv4, vp=define_region(2:3, 1))  
	print(my.plots$Sub1, vp = define_region(1:2, 2))
	print(my.plots$Sub2, vp = define_region(1:2, 3))
	print(my.plots$Adult, vp=define_region(1:2, 4))
	print(my.plots$SubMale, vp = define_region(3:4, 2))
	print(my.plots$AdMale, vp = define_region(3:4, 3))
	
	# adding text
	grid.text('Females', vp=define_region(1,1))#x=1.1, y=0.1)# adds text to the plot
	grid.text('Nest Size', vp=define_region(5,2:3), just = "top")
	grid.text('LogLeg', vp=define_region(4,4), just = "top", rot = 90)
	
	
	
	
}


graph <- InstarGridGraph(spidersMul, "logLegsdfd")

var <- "logLdeg"

column_index <- grep(var, colnames(spidersMul))

column_index <- which(names(spidersMul) == var)

if (column_index == 0) {
	print ("zero")
	
} else {
	print("something is wrong")
	
}