# Author: Ruth
###############################################################################

instarInput <- "Adult"


spidersBoot <- subset(spidersMul, Instar == instarInput, select = c(NestID, logLeg))

spidersBoot <- subset(spidersMul, Instar == instarInput & NestID == "28.8EXa03", select = c(NestID, logLeg))



spidersBoot <- spidersBoot[complete.cases(spidersBoot), ]  # removing any NA's

colnames(spidersBoot)[2] <- "variable"  # changing name of variable of interest for function

spidersBootAve <- ddply(spidersBoot, .(NestID), summarise,
		N = length(variable),
		mean = mean(variable),
		sd_data = ad(variable))


maxVariable <- max(spidersBoot$variable)
minVariable <- min(spidersBoot$variable)
mean <- spidersBootAve$mean[nest]	
sampSize <- spidersBootAve$N[nest]

calMaxVarFun <- function(mean, sampSize, min, max){

	data_tot <- mean * sampSize
	list <- c(rep(min, sampSize))  # making a list with the min value for all spiders
	calTot<- sum(list)
	totDiff <- data_tot - calTot	
	n<- sampSize + 1
	
	while(totDiff > 0){
		
		n <- n -1

	if (totDiff < max){
		list[n] <- totDiff
		
	}else{
		list[n] <- max		
		
	}
	calTot<- sum(list)
	print("calTot")
	print(calTot)
	totDiff = data_tot - calTot
	print("n")
	print(n)
	print("totdiff")
	print (totDiff)
	print ("list")
	print(list);
	
	
}
return(list)
	
}


out <- calMaxVarFun(8.7, 5, 1.2, 10.1)
out
mean(out)
sd(out)


list <- c(rep(1.2, 5))
list


i <- 1

while (i < 6) {
	print(i)
	i <- i+1
}
