# TODO: Add comment
# 
# Author: Ruth
###############################################################################


# spline function. Set lambda and it will produce the spline for you

fnSpline<-function(my.lambda, ave.Sub) {     
	
	my.spar<-0.05 
	
	for (i in 1:4){
		
#calculating s0
		s0 <- with(smooth.spline(ave.Sub, spar=my.spar), spar-0.0601*log(lambda)) 
		
		my.spar<-s0 + 0.0601*log(my.lambda) #getting the correct spar from my lambda  
		
		my.spline<-smooth.spline(ave.Sub, spar=my.spar)
		
		
	}      
	return(my.spline)
}
