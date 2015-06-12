# TODO: Add comment
# test
# Author: Ruth
###############################################################################
library (plyr)
library(ggplot2)
library(gridExtra)
library(reshape2)

#folder <- "R_Graphs/"
folder <- "DisperalSimulationOutput/"

fileNames<-read.csv(paste(folder, "FilesCreated.csv", sep = ""), quote="")# import file names csv file

fileNames[] <- lapply(fileNames, as.character) # making factors into strings

DF <- data.frame(Comp = numeric(0), disp = numeric(0), var = numeric(0), meanK = integer(0), pop_age = integer(0))#, fileName = character(0))
num_gens <- 2000

## TO test the function
fileName <- fileNames[14,1]

#graph making function
graphFunction <- function(folder, fileName){

	#filetoImport <- paste(fileName, ".py.csv", sep = "")
	#indFileToImport <- paste(fileName, ".py_inds.csv", sep = "")
	filetoImport <- paste(folder, fileName, ".py.csv", sep = "")
	indFileToImport <- paste(folder, fileName, ".py_inds.csv", sep = "")
	
	
	pngTitle <- paste(folder, fileName, "_graph", ".png", sep = "")
	print ("overall file exists?")
	print(file.exists(filetoImport))
	print ("ind file exists?")
	print(file.exists(indFileToImport))

	File <- read.csv(filetoImport, quote = "")
	
	ColInfo <- data.frame(pop_age = File$pop_age, col_age = File$colony_age, col_id = File$colony_ID, numAdsB4dis = File$num_adsB4_dispersal)
	ColInfo <- unique(ColInfo)
	
	# graph variables
	mytitle = textGrob(label = fileName)
	
	pngHeight = 400 * 14 # 400 * number of graphs
	
	png(pngTitle,  width = 1300, height = pngHeight, units = "px", pointsize = 16) # height = 400* num graphs
	
	print("png title")
	print (pngTitle)
	mytheme = theme(text = element_text(size=16))

	
	
	DF_list <- c(as.numeric(File$Comp_slope[1]), File$disp_rsk[1], File$input_var[1], File$meanK[1], max(File$pop_age))#, filetoImport)

	
	
	ByPopAge<- ddply(File, .(pop_age), summarise,
			NCols = length(!is.na(colony_ID)),
			TotNumInd = sum(num_ads)
		)
	
	ByPopAgeAndCol<- ddply(File, .(pop_age, colony_ID, colony_age), summarise, 
			TotNumAds = sum(num_ads)
		)
	
	ByPopAgeAndCol$factorAge <- as.factor(ByPopAgeAndCol$pop_age)
	
	File$pcntDisperse <- File$dispersers / File$num_adsB4_dispersal
	File$pcntMoult <- File$num_juvs_moulting/File$numjuvs
	
	
	#pop age by total number of individuals
	p1 <- ggplot(data = ByPopAge, aes(x = pop_age, y = TotNumInd)) + geom_point() +  mytheme
	
	#pop age by number of colonies
	p2 <- ggplot(data = ByPopAge, aes(x = pop_age, y = NCols)) + geom_point() +  mytheme
	
	#number of adults per nest
	p3 <- ggplot(data = ByPopAgeAndCol, aes(x= factorAge, y = TotNumAds)) + geom_point() +  mytheme # maybe change to max/min?
	
	#average age
	p4 <- ggplot(data = ByPopAgeAndCol, aes(x= factorAge, y = colony_age)) + geom_point() +  mytheme
	
	# next size vs dispersers
	p5 <- ggplot(data = File, aes(x= num_adsB4_dispersal, y = dispersers)) + geom_point() +  mytheme + stat_smooth(se=FALSE) + scale_y_continuous(limits = c(0, NA))
	
	# Nest size before dispersal vs food per adult
	p6 <- ggplot(data = File, aes(x=num_adsB4_dispersal, y = colony_food/num_ads )) + geom_point() + stat_smooth(se = FALSE) +  
			mytheme + scale_y_continuous(limits = c(0, NA))
	
	#Nest size after disperal with food per adult
	p7 <- ggplot(data = File, aes(x=num_ads, y = colony_food/num_ads )) + geom_point() + stat_smooth(se = FALSE) +  
			mytheme + scale_y_continuous(limits = c(0, NA))
	
	# number juvs moulting with size after dispersal
	p8 <- ggplot(data = File, aes(x=num_ads, y = pcntMoult)) + stat_smooth(se = FALSE)  + geom_point() +  mytheme +
			scale_y_continuous(limits = c(0, 1))
	
	# percentage of adults dispersing by nest size
	p9 <- ggplot(data = File, aes(x= num_adsB4_dispersal, y = pcntDisperse)) + geom_point() + stat_smooth(se = FALSE) + mytheme +
			scale_y_continuous(limits = c(0, 1))
	
	p10 <- ggplot(data = File, aes(x=num_adsB4_dispersal, y = ave_food)) + geom_point() +
			mytheme + scale_y_continuous(limits = c(0, 1)) + stat_smooth(se = FALSE)
	
	rm(ByPopAge)
	rm(ByPopAgeAndCol)
	
	
	deadcols <- subset(File, colAlive== 'dead')
	
	if (nrow(deadcols) == 0){
		df <- data.frame()
		p11<-ggplot(df) + geom_point() + xlim(0, 10) + ylim(0, 100) + ggtitle("NO COLONIES DIED!!")
		print("no colonies died")
	}else{
	# histogram of size of dead colonies
		p11 <- ggplot(data = subset(File, colAlive== 'dead'), aes(x= num_ads)) + geom_histogram() + mytheme
	}
	rm(deadcols)
	rm(File)
	
	## Making n, n+1 graph
	nnplus1 <- data.frame(col_id=numeric(), pop_age = numeric(),  N=numeric(), NPlus1=numeric()) # creating empty data frame
	maxcol_id <- max(ColInfo$col_id)
	
	counter <- 0
	
	for (colony in 1:maxcol_id){
		print(colony)
		
		col_subset <- subset(ColInfo, col_id == colony) # test 3 colony 11 incorrect numbering of colonies somehow
		maxcol_age <- max(col_subset$col_age)
		mincol_age <- min(col_subset$col_age)
	
		for (age in mincol_age:maxcol_age){
			counter <- counter + 1		
		
			nnplus1[counter,1] <- colony # [row number, col num]
			nnplus1[counter,2] <- col_subset$pop_age[which(col_subset$col_age == age)]
			nnplus1[counter,3] <- col_subset$numAdsB4dis[which(col_subset$col_age == age)]
			
			if (age == maxcol_age){
				nnplus1[counter,4] <- 0
			}else{
				nnplus1[counter,4] <- col_subset$numAdsB4dis[which(col_subset$col_age == (age +1))]	
			}

		
		}
	}
	
	
	nnplus1 <- subset(nnplus1, pop_age != num_gens)

	p12 <- ggplot(data = nnplus1, aes(x= N, y = NPlus1)) + geom_point() + stat_smooth(se = FALSE) + mytheme
	
	# testing
	ggplot(data = nnplus1, aes(x= N, y = NPlus1)) + geom_point() + stat_smooth(method = "gam", formula = y ~ s(x),  se = FALSE) + 
			geom_abline(intercept = 0, slope = 1 ) + scale_y_continuous(limits = c(0, NA)) + scale_x_continuous(limits = c(0, NA))
			
	gam_nnplus1 <- gam(NPlus1~s(N), data = nnplus1)
	plot <- plot.gam(gam_nnplus1)
	
	m <- nls(NPlus1 ~ I(N^(1-a) * exp(b) * exp(-c * N)) , data = nnplus1, start = list(a=0.4, b=1.5, c=0.02), trace = T)
	
	plot(m)
	
	plot(nnplus1$N, nnplus1$NPlus1)
	
	
	lines(nnplus1$N,predict(m))
	
	summary(m)
	
	predictm <- predict(m)
	
	#####################################
	## try nls http://robinlovelace.net/2013/10/23/nls-demonstation.html
	rm(nnplus1)
	rm(col_subset)
	
	model <-gam_nnplus1$model
	gamDF <- data.frame(gam_nnplus1$y, gam_nnplus1$linear.predictors)
	
	plot(gamDF$gam_nnplus1.y, test)
	
	plot(model$N, model$NPlus1)
	test <- fitted.values(gam_nnplus1)
	
	#### Ind Files Graphs
	
	indFile <- read.csv(indFileToImport, quote = "")
	
	
	
	AdsByPopAgeAndCol<- ddply(subset(indFile, type =="Ad"), .(col_age, col_id, type), summarise, 
			AdSzeMax = max(food),
			AdSzeMin = min(food),
			AdSzeMean = mean(food)
	)
	
	AdsByPopAgeAndCol <- melt(AdsByPopAgeAndCol, id.vars = c("col_id", "col_age"), 
			measure.vars = c("AdSzeMax", "AdSzeMin", "AdSzeMean"), variable.name="MinMax", value.name="AdSize")
	
	AdsByPopAgeAndCol <-merge(ColInfo, AdsByPopAgeAndCol, by = c("col_id", "col_age"))
	
	p13 <- ggplot(data = AdsByPopAgeAndCol, aes(x= numAdsB4dis, y = AdSize, colour = MinMax)) + geom_point() +
			stat_smooth(se = FALSE) + mytheme + scale_y_continuous(limits = c(0, 1))
	
	rm(AdsByPopAgeAndCol)
	
	JuvsByPopAgeAndCol<- ddply(subset(indFile, type =="juv"), .(col_age, col_id, type), summarise, 
			JuvSzeMax = max(food),
			JuvSzeMin = min(food),
			JuvSzeMean = mean(food)
	)
	
	JuvsByPopAgeAndCol <- melt(JuvsByPopAgeAndCol, id.vars = c("col_id", "col_age"), 
			measure.vars = c("JuvSzeMax", "JuvSzeMin", "JuvSzeMean"), variable.name="MinMax", value.name="JuvSize")
		
	JuvsByPopAgeAndCol <-merge(ColInfo, JuvsByPopAgeAndCol, by = c("col_id", "col_age"))
	
	
	p14 <- ggplot(data = JuvsByPopAgeAndCol, aes(x= numAdsB4dis, y = JuvSize, colour = MinMax)) + geom_point() +
			stat_smooth(se = FALSE) + mytheme + scale_y_continuous(limits = c(0, 1))
	

	rm(JuvsByPopAgeAndCol)	

	rm(indFile)

	print(grid.arrange(p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14,  ncol = 1, main = mytitle))
	
	dev.off()
	
	return(DF_list)

}


#for (i in 1:nrow(fileNames)){
for (i in 12:12){
	print(i)	
	theFileName <-fileNames[i,1]
		
	#fileToImport <- paste(theFileName, ".py.csv", sep = "")
	fileToImport <- paste(folder, theFileName, ".py.csv", sep = "")
	print(fileToImport)	
	
	if(file.exists(fileToImport) == "TRUE"){
		print ("the file does exist which is good!")
		list <- graphFunction(folder, theFileName)
		DF[i,] <- list
		} else {
	print ("file does not exist")
}}


write.table(DF, paste(folder, "PopAge.csv", sep = ""), sep=",", row.names = FALSE)


