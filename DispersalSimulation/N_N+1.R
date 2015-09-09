# TODO: Add comment
# 
# Author: Ruth
###############################################################################


library (plyr)
library(reshape2)
library(broom)
library(doParallel)

cl <- makeCluster(2, outfile = "")# outfile = paste(folder, "log.txt", sep = ""))
# Register cluster
registerDoParallel(cl)


File <- read.csv(paste("DisperalSimulationOutput/SurvivalTest.csv", sep = ""))

File <- subset(file, pop_age > 100)

File$prevDisp <- "n"

cols<- as.numeric(levels(as.factor(File$colony_ID)))

for (i in 1:length(cols)){
	
	thisCol <- cols[i]
	#print(thisCol)
	
	age_FstDisp <- File$colony_age[which(File$dispersers > 0 & File$colony_ID == thisCol)]
	if (length(age_FstDisp) > 0){
		min_ageFstDisp <- min(age_FstDisp)
		#print (min_ageFstDisp)
		File$prevDisp[which(File$colony_age >= min_ageFstDisp & File$colony_ID == thisCol)] <- "y"
		
	}else{
		#print("no dispersers")
	}
}

File$prevDisp[File$dispersers >0] <- "now"



ColInfo <- data.frame(pop_age = File$pop_age, col_age = File$colony_age, col_id = File$colony_ID, 
		numAdsB4dis = File$num_adsB4_dispersal, dispersers = File$dispersers, prevDisp = File$prevDisp)


## Copied from other file -- need to make into function

nnplus1 <- data.frame(col_id=numeric(), pop_age = numeric(),  N=numeric(), NPlus1=numeric(), disp = numeric(), prevDisp = character(),
		stringsAsFactors=FALSE) # creating empty data frame

counter <- 0

cols <- as.numeric(levels(as.factor(ColInfo$col_id)))

#print(length(cols))

for (i in 1:length(cols)){
	
	colony <- cols[i]
	#print ("colony")
	#print(colony)
	
	col_subset <- subset(ColInfo, col_id == colony)
	
	
	maxcol_age <- max(col_subset$col_age)
	mincol_age <- min(col_subset$col_age)
	
	for (age in mincol_age:maxcol_age){
		#print("age")
		#print(age)
		counter <- counter + 1
		
		if (col_subset$pop_age[which(col_subset$col_age == age)]){
			
			nnplus1[counter,1] <- colony # [row number, col num]
			nnplus1[counter,2] <- col_subset$pop_age[which(col_subset$col_age == age)]
			nnplus1[counter,3] <- col_subset$numAdsB4dis[which(col_subset$col_age == age)]
			nnplus1[counter,5] <- col_subset$dispersers[which(col_subset$col_age == age)]	
			nnplus1[counter,6] <- as.character(col_subset$prevDisp[which(col_subset$col_age == age)])
			
			
			if (age == maxcol_age){ # setting 
				nnplus1[counter,4] <- 0 
			}else{
				nnplus1[counter,4] <- col_subset$numAdsB4dis[which(col_subset$col_age == (age +1))]	
			}
			
			
			
			
		}
	}
}




nnplus1 <- subset(nnplus1, pop_age < num_gens ) # removing nests at the end of generations that might not have died

nnplus1$AveGrowth <- (nnplus1$NPlus1 - nnplus1$N) / nnplus1$N

#ave growth rate per ind
p13a <- ggplot(data = nnplus1, aes(x  = N, y = AveGrowth)) + geom_point(aes(colour = prevDisp)) + stat_smooth() + mytheme + ggtitle("ave growth rate per ind nnplus1-n/n") + stat_smooth(se = FALSE)

nnplus1 <- subset(nnplus1, disp == 0)  # removes colonies that had dispersed as the numbers will be wrong

######## calculating logistic equation

logisticFn = function(nnplus1){
	
	logistic <- nls(NPlus1 ~ I((N^(1+a)) * exp(b) * (exp(-c * N))) , data = nnplus1, start = list(a=0.4, b=1.5, c=0.02), 
			algorithm= "port", trace = T)
	return (logistic) #list(a=0.4, b=1.5, c=0.02)
}	

tryCatchLogistic= function(x) {
	tryCatch(logisticFn(x), warning = function(w) {print("warning")},
			error = function(e) {print("error")}) 
}

logistic <- tryCatchLogistic(nnplus1)

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
	ricker <- nls(NPlus1 ~ I((N^(1+a)) * b * (1-(N/K))) , data = nnplus1, start = list(a=0.4, b=1.5, K=100), 
			algorithm= "port", trace = T)
	return(ricker) # start = list(a=0.4, b=1.5, K=100)
}



tryCatchRicker= function(x) {
	tryCatch(rickerFn(x), warning = function(w) {print("warning")},
			error = function(e) {print("error")}) 
}

ricker <- tryCatchRicker(nnplus1)


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

