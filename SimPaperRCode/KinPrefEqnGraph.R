# TODO: Add comment
# 
# Author: Ruth
###############################################################################


##plotting the kin pref function


x<-seq(0, (0.5), length.out=2)
y<-1-(2*x)

plot(x, y, xlim=c(0, 0.5), ylim=c(0, 1), xlab="Relatedness", ylab = "Probability of admittance to the group",
		type="l", lty=3, xaxs="i", yaxs="i", bty="n" ) ## produces an empty plot

tVec <- c(0.05, 0.2, 0.4, 0.6, 0.8, 0.95) #varies between 0 and 1 kin preference - each line is a different t


for (i in 1:length(tVec)){
	
t<-	tVec[i]

#r = relatedness, plot on x axis
r1 <- seq(0, (0.5*t) , length.out=6) # 0-0.5t
r2 <- seq((0.5*t) , 1,  length.out=6) # 0.5t to 1

k1<- (r1 * ((2/t)-2)) # prob of getting in for r=0-0.5t--plot on y axis
k2<- 1- (t*(((2*r2)-1)/(t-1)))

k<- c(k1, k2)
r<-c(r1, r2)

lines(r, k)
	

}

