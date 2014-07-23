# TODO: Add comment
# 
# Author: Ruth
###############################################################################

 x<-timeSer

find.freq <- function(x)
{
	n <- length(x)
	spec <- spec.ar(c(x),plot=FALSE)
	if(max(spec$spec)>0) # Arbitrary threshold chosen by trial and error.
	{
		period <- round(1/spec$freq[which.max(spec$spec)])
		if(period==Inf) # Find next local maximum
		{
			j <- which(diff(spec$spec)>0)
			if(length(j)>0)
			{
				nextmax <- j[1] + which.max(spec$spec[j[1]:500])
				period <- round(1/spec$freq[nextmax])
			}
			else
				period <- 1
		}
	}
	else
		period <- 1
	return(period)
}

## my playing

(spec.ar(c(x),plot=TRUE))

print (max(spec$spec))