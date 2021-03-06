# TODO: Add comment
# 
# Author: Ruth
###############################################################################
# feb 22, replacing ave.Sub with x, y so that I don't have to make
#new table to run the spline function

# spline function. Set lambda and it will produce the spline for you

fnSpline<-function(my.lambda, xx, yy) {     
	
	my.spar<-0.05 

		
#calculating s0
	
	for(i in 1:4){ # improves on my.spar with each iteration
		s0 <- with(smooth.spline(xx, y=yy, spar=my.spar), spar-0.0601*log(lambda)) 
		
		my.spar<-s0 + 0.0601*log(my.lambda) #getting the correct spar from my lambda  
		
		
		myspline<-smooth.spline(xx, y=yy, spar=my.spar, keep.data = TRUE)
	
		print(my.spar)
	}
		
	spline.df <- data.frame(myspline$x, myspline$y)
		
	return (spline.df)
	#return (my.spline)
}



splineOutput <- fnSpline(0.05, subDispFile$Comp_meas, log10(subDispFile$metPopAgeMax))


ggplot(splineOutput, aes(x = myspline.x, y =  myspline.y))  + 
		geom_point(size = pointSize, position = position_jitter(w = 0, h = 0.05)) + 
		geom_line() + 
		mytheme + ylab("Log max metapopulation age") + 
		scale_colour_discrete("Size needed to disperse") + xlab("Competition measure") +
		scale_y_continuous(breaks=seq(0, 3.0, by=0.5), limits = c(0, 3.0))



ggplot() + geom_point(data = subDispFile, aes(x = Comp_meas, y = log10(metPopAgeMax), colour = as.factor(ad_dsp_fd))) + 
		geom_line(data = splineOutput, aes(x = myspline.x, y =  myspline.y,  colour = as.factor(ad_dsp_fd))) + 
		mytheme + ylab("Log max metapopulation age") + 
		scale_colour_discrete("Size needed to disperse") + xlab("Competition measure") +
		scale_y_continuous(breaks=seq(0, 3.0, by=0.5), limits = c(0, 3.0))


splineOutput <- ddply(subDispFile, "ad_dsp_fd", function(x) {
    		spline <- fnSpline(0.0005, x$Comp_meas, log10(x$metPopAgeMax))

		})


ggplot(splineOutput, aes(x = myspline.x, y =  myspline.y))  + 
		geom_point(size = pointSize, position = position_jitter(w = 0, h = 0.05)) + 
		geom_line() + 
		mytheme + ylab("Log max metapopulation age") + 
		scale_colour_discrete("Size needed to disperse") + xlab("Competition measure") +
		scale_y_continuous(breaks=seq(0, 3.0, by=0.5), limits = c(0, 3.0))
