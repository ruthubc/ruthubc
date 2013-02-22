# TODO: Add comment
# 
# Author: Ruth
###############################################################################
# feb 22, replacing ave.Sub with x, y so that I don't have to make
#new table to run the spline function

# spline function. Set lambda and it will produce the spline for you

fnSpline<-function(my.lambda, xx, yy) {     
	
	my.spar<-0.05 
	
	for (i in 1:4){
		
#calculating s0
		s0 <- with(smooth.spline(xx, y=yy, spar=my.spar), spar-0.0601*log(lambda)) 
		
		my.spar<-s0 + 0.0601*log(my.lambda) #getting the correct spar from my lambda  
		
		my.spline<-smooth.spline(xx, y=yy, spar=my.spar)
		
		
	}      
	return(my.spline)
}
