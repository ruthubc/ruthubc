# TODO: Add comment
# 
# Author: Ruth
###############################################################################

plotfun <- function(x) {
	plot( c(-0.5,-0.5,0.5,0.5), c(0,1,1,0), col='blue', xlim=c(-2,2),
			type='l', xlab='', ylab='' )
	if( x > -1 && x < 0 ) {
		polygon( c(-0.5, -0.5, x+0.5, x+0.5), c(0,1,1,0), col='yellow', border=NA )
		lines( c(-0.5, -0.5, 0.5, 0.5), c(0,1,1,0), col='blue' )
		lines( c(-1,x), c(0,x+1) )
	} else if( x >= 0 && x < 1 ) {
		polygon( c(x-0.5, x-0.5, 0.5, 0.5), c(0,1,1,0), col='yellow', border=NA )
		lines( c(-0.5, -0.5, 0.5, 0.5), c(0,1,1,0), col='blue' )
		lines( c(-1,0,x), c(0,1,1-x) )
	} else if (x >= 1) {
		lines( c(-1,0,1), c(0,1,0) )
	}
	abline(v=x, lty=3)
	lines( c(x-0.5,x-0.5,x+0.5,x+0.5), c(0,1,1,0), col='red' )
}

dev.new(height=3, width=6)

for(i in seq(-2.5, 2.5, 0.05) ) {
	plotfun(i)
	Sys.sleep(0.1)
}

