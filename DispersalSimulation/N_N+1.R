# TODO: Add comment
# 
# Author: Ruth
###############################################################################


library (plyr)
library(reshape2)
library(broom)
library(ggplot2)
#library(doParallel)

#cl <- makeCluster(2, outfile = "")# outfile = paste(folder, "log.txt", sep = ""))
# Register cluster
#registerDoParallel(cl)


num_gens <- 500

File <- read.csv(paste("DisperalSimulationOutput/SurvivalTest.csv", sep = ""))

File <- subset(File, pop_age > 100)


## Updating dispersal information
firstDisp <- ddply(subset(File, dispersers > 0), .(colony_ID), summarise,
		firstDisp = min(pop_age))

File <- merge(File, firstDisp, by = "colony_ID", all.x = TRUE)
 
File$prevDisp <-  ifelse(File$firstDisp > File$pop_age | is.na(File$firstDisp), "n", "y" )

File$prevDisp <- ifelse(File$dispersers > 0, "now", File$prevDisp)

# Testing
#File<-subset(File, select = c(colony_ID, dispersers, pop_age, firstDisp, prevDisp))

#write.csv(test, file = "DisperalSimulationOutput/nnTest.csv" )




ColInfo <- data.frame(pop_age = File$pop_age, col_age = File$colony_age, col_id = File$colony_ID, 
		numAdsB4dis = File$num_adsB4_dispersal, dispersers = File$dispersers, prevDisp = File$prevDisp)

write.csv(ColInfo, file = "DisperalSimulationOutput/ColInfoTest.csv" )

## Copied from other file -- need to make into function


ColInfo$ColAgePlus1 <- ColInfo$col_age

ColInfoPlus1 <- subset(ColInfo, select = c("col_id", "numAdsB4dis", "col_age"))

colnames(ColInfoPlus1)[2] <- "NPlus1"

ColInfoPlus1$ColAgePlus1 <- ColInfoPlus1$col_age - 1

nnplus1 <- merge(ColInfo, ColInfoPlus1,  by =c("ColAgePlus1", "col_id"))

colnames(nnplus1)[5] <- "N"

write.csv(nnplus1, file = "DisperalSimulationOutput/nnTest.csv" )

nnplus1 <- subset(nnplus1, prevDisp != "now")  # removing colonies that have just dispersed


nnplus1 <- subset(nnplus1, pop_age < num_gens - 1 ) # removing nests at the end of generations that might not have died

nnplus1$AveGrowth <- (nnplus1$NPlus1 - nnplus1$N) / nnplus1$N


curve(I((x^(1+0.4)) * exp(1.5) * (exp(-0.02 * x))), 0, 400) # plots the function


ggplot(data.frame(x=c(0, 400)), aes(x)) + stat_function(fun=function(x)(x^(1+0.4)) * exp(1.5) * (exp(-0.02 * x)))
				#I((x^(1+0.4)) * exp(1.5) * (exp(-0.02 * x))))

######## calculating logistic equation

logisticFn = function(nnplus1){
	
	logistic <- nls(NPlus1 ~ I((N^(1+a)) * exp(b) * (exp(-c * N))) , data = nnplus1, start = list(a=0.4, b=1.5, c=0.02), 
			lower=list(a=0, b=0,c=0), algorithm= "port", trace = T)
	
	#logistic <- nls(NPlus1 ~ (N * exp(b) * (exp(-c * N))) , data = nnplus1, start = list(b=1.5, c=0.02), 
			#algorithm= "port", trace = T)
	return (logistic) #list(a=0.4, b=1.5, c=0.02)
}	

tryCatchLogistic= function(x) {
	tryCatch(logisticFn(x), warning = function(w) {print("warning")},
			error = function(e) {print("error")}) 
}

logistic <- tryCatchLogistic(nnplus1)

summary(logistic)

nnplus1$logisticPredict <- predict(logistic)

ggplot(data = nnplus1, aes(x= N, y = NPlus1, colour = prevDisp)) + geom_point() + 
		geom_abline(intercept = 0, slope = 1 ) + scale_y_continuous(limits = c(0, NA)) + scale_x_continuous(limits = c(0, NA)) +
		geom_line(aes(x = N, y = logisticPredict), colour = 'blue') + ggtitle("logistic eqn")


ggplot(data = nnplus1, aes(x= N, y = NPlus1, colour = prevDisp)) + geom_point() + 
		geom_abline(intercept = 0, slope = 1 ) + scale_y_continuous(limits = c(0, NA)) + scale_x_continuous(limits = c(0, NA)) +
		geom_line(aes(x = N, y = logisticPredict), colour = 'blue') + ggtitle("logistic eqn") + 
		stat_function(fun=function(x)(x^(1+0.79)) * exp(-1) * (exp(-0.0105 * x)))


 options(warn = -1) #suppress warnings globally

if (logistic =="error"){
	print ("error in logistic calculation") 
	
	p14 <- ggplot(data = nnplus1, aes(x= N, y = NPlus1)) + geom_point() + 
			geom_abline(intercept = 0, slope = 1 ) + scale_y_continuous(limits = c(0, NA)) + scale_x_continuous(limits = c(0, NA))+
			ggtitle("logistic eqn did not work :-(")
	
	
} else {
	print ("logistic equation did work!")
	
	nnplus1$logisticPredict <- predict(logistic)
	
	p14 <- ggplot(data = nnplus1, aes(x= N, y = NPlus1)) + geom_point() + 
			geom_abline(intercept = 0, slope = 1 ) + scale_y_continuous(limits = c(0, NA)) + scale_x_continuous(limits = c(0, NA)) +
			geom_line(aes(x = N, y = logisticPredict), colour = 'blue') + ggtitle("logistic eqn")
	
	logisticTable <- tidy(logistic)
	logisticTable$type <- "logistic"		
	
}



######## Calculating ricker equation

rickerFn = function(nnplus1){
	#ricker <- nls(NPlus1 ~ I((N^(1+a)) * b * (1-(N/K))) , data = nnplus1, start = list(a=0.4, b=1.5, K=100), 
			#algorithm= "port", trace = T)
	
	ricker <- nls(NPlus1 ~ I((N * lam) * ((1- capA * exp((-a * N )/oth))/((1 + a *N )^b))),
			data = nnplus1, start = list(capA = 70, a = 0.01, b=1, lam = 1, oth = 1.0), 
			algorithm= "port", trace = T)
	
	return(ricker) # start = list(a=0.4, b=1.5, K=100)
}



ricker <- nls(NPlus1 ~ I((N * lam) * ((1 - (capA * exp((-a * N )/oth)))/((1 + a *N )^b))),
		data = nnplus1, start = list(capA = 40, a = 0.01, b=6, lam = 1, oth = 1.0), 
		algorithm= "port", trace = T)

tryCatchRicker= function(x) {
	tryCatch(rickerFn(x), warning = function(w) {print("warning")},
			error = function(e) {print("error")}) 
}



ricker <- tryCatchRicker(nnplus1)

nnplus1$rickerPredict <- predict(ricker)	

ggplot(data = nnplus1, aes(x= N, y = NPlus1, colour = prevDisp)) + geom_point() + 
		geom_abline(intercept = 0, slope = 1 ) + scale_y_continuous(limits = c(0, NA)) + scale_x_continuous(limits = c(0, NA)) +
		geom_line(aes(x = N, y = rickerPredict), colour = 'blue') + ggtitle("ricker eqn")



if (ricker =="error"){
	print ("error in ricker calculation") 
	
	p15 <- 	ggplot(data = nnplus1, aes(x= N, y = NPlus1, colour = prevDisp)) + geom_point() + 
			geom_abline(intercept = 0, slope = 1 ) + scale_y_continuous(limits = c(0, NA)) + scale_x_continuous(limits = c(0, NA)) +
			ggtitle("ricker eqn did not work :-(")
	
	
	
	
} else {
	print ("ricker equation did work!")
	nnplus1$rickerPredict <- predict(ricker)		
	
	p15 <- 	ggplot(data = nnplus1, aes(x= N, y = NPlus1, colour = prevDisp)) + geom_point() + 
			geom_abline(intercept = 0, slope = 1 ) + scale_y_continuous(limits = c(0, NA)) + scale_x_continuous(limits = c(0, NA)) +
			geom_line(aes(x = N, y = rickerPredict), colour = 'blue') + ggtitle("ricker eqn")
	
	rickerTable <- tidy(ricker)
	rickerTable$type <- "ricker"
	
	
}


options(warn = 0) # turns warnings back on



if (exists("rickerTable") == TRUE & exists("logisticTable") == TRUE){
	print ("both tables exist")
	nnplusoneCom <- rbind(rickerTable, logisticTable)	
	
	
} else {
	if(exists("rickerTable") == TRUE & exists("logisticTable") == FALSE){
		print("only ricker")	
		nnplusoneCom <- rickerTable
		
		
		
	} else {
		if(exists("rickerTable") == FALSE & exists("logisticTable") == TRUE){
			print("only the logistic table exists")
			nnplusoneCom <- logisticTable
			
		}else{	
			print("neither ricker or logistic worked")
			nnplusoneCom <- data.frame(term = NA, estimate = NA, std.error = NA,  p.value = NA,
					type = "both")
			
			
		}
	}
}


nnplusoneCom$Comp <- DF[1,1]
nnplusoneCom$disp <- DF[1,2]
nnplusoneCom$var <- DF[1,3]
nnplusoneCom$meanK <- DF[1,4]
nnplusoneCom$FileName<-fileName


rm(nnplus1)

