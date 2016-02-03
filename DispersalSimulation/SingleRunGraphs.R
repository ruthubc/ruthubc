# TODO: Add comment
# 
# Author: Ruth
###############################################################################

library (plyr)
library(ggplot2)
library(gridExtra)
library(reshape2)
library(broom)

min_popAge <-50 # the number of generations to discount from the start of the calculations

folder <- "DisperalSimulationOutput/"

theFileName <- "3559_slp0_Rsk0.1_K300_var0.1_dslm0.2_maxOff4"

numGens <- 500   ###################################### DON"T FORGET TO CHANGE for each file ############


filetoImport <- paste(folder, theFileName, ".py.csv", sep = "")
File <- read.csv(filetoImport, quote = "")


maxPopAge <- max(File$pop_age)
print ("maxPop age")
print (maxPopAge)

if(maxPopAge < min_popAge + 50){
	fn_min_popAge <- 0
	print("pop did not survive to sufficant generations")
}else{
	fn_min_popAge <- min_popAge
	File <- subset(File, pop_age >= min_popAge) # removing the first x number of gens before do cals
}



File$AveOffAd <- File$numjuvs/File$num_ads
File$AveOffAd[which(File$AveOffAd == Inf)] <- NA



## Updating dispersal information


firstDisp <- ddply(subset(File, dispersers > 0), .(colony_ID), summarise, firstDisp = min(pop_age))

if(nrow(firstDisp) > 0){
	
	File <- merge(File, firstDisp, by = "colony_ID", all.x = TRUE)
	
	File$prevDisp <-  ifelse(File$firstDisp > File$pop_age | is.na(File$firstDisp), "n", "y" )
	
	File$prevDisp <- ifelse(File$dispersers > 0, "now", File$prevDisp)
}else{
	File$prevDisp <- "n"
}


mytheme = theme(text = element_text(size=16))
pngTitle <- paste(folder, theFileName, "_graph", ".png", sep = "")

mytitle <- theFileName

pngHeight = 480 * (20 +1 )# 400 * number of graphs)

png(pngTitle,  width = 1600, height = pngHeight, units = "px", pointsize = 16) # height = 400* num graphs


ByPopAge<- ddply(subset(File, num_ads!=0), .(pop_age), summarise,
		NCols = length(!is.na(colony_ID)),
		TotNumInd = sum(num_ads),
		maxNumAds = max(num_ads),
		minNumAds = min(num_ads),
		meanNumAds = mean(num_ads)
)



File$pcntDisperse <- File$dispersers / File$num_adsB4_dispersal
File$pcntDisperse[which(File$pcntDisperse == Inf)] <- NA

File$pcntMoult <- File$num_juvs_moulting/File$numjuvs
File$pcntMoult[which(File$pcntMoult == Inf)] <- NA

interval <- ceiling(max(File$num_adsB4_dispersal)/200)

#Histogram of colony sizes

p0 <- ggplot(data = File, aes(x= num_adsB4_dispersal, fill = prevDisp)) + geom_histogram(binwidth = interval) +  mytheme + ggtitle("histogram of colony sizes")

p00 <- ggplot(data = subset(File, num_ads > 1) , aes(x= num_adsB4_dispersal, fill = prevDisp)) + geom_histogram(binwidth = interval) +  
		mytheme + ggtitle("histogram of colony sizes with single nests removed")

#pop age by total number of individuals
p1 <- ggplot(data = ByPopAge, aes(x = pop_age, y = TotNumInd)) + geom_line() +  mytheme + ggtitle("total number of individuals in the population") 

#pop age by number of colonies
p2 <- ggplot(data = ByPopAge, aes(x = pop_age, y = NCols)) + geom_line() +  mytheme + ggtitle("total number of colonies in the population")

#number of adults per nest
p3 <- ggplot(data = ByPopAge, aes(x= pop_age, y = meanNumAds)) + geom_line() +  geom_line(aes(x=pop_age, y = maxNumAds), colour = "blue") +
		geom_line(aes(x=pop_age, y = minNumAds), colour = "red")+ mytheme + ggtitle("Mean, max and min num ads in nest")


p4 <- ggplot(data = File, aes(x= colony_age, y = num_ads, colour = prevDisp )) + geom_point() + mytheme + ggtitle("colony size by colony age")

# next size vs dispersers
p5 <- ggplot(data = File, aes(x= num_adsB4_dispersal, y = dispersers)) + geom_point() +  mytheme + stat_smooth(se=FALSE) + 
		scale_y_continuous(limits = c(0, NA)) + ggtitle("nest size before dispersal vs number of dispersers")

p6 <- ggplot(data = File, aes(x= num_adsB4_dispersal, y = pcntDisperse)) + geom_point() + stat_smooth(se = FALSE) + mytheme +
		scale_y_continuous(limits = c(0, 1)) + ggtitle("percentage of ads dispersing")

maxcolsize <- max(File$num_adsB4_dispersal)

p6a <-ggplot(data = subset(File, prevDisp == "now"), aes(x = num_adsB4_dispersal, y = num_ads, colour = prevDisp)) + geom_point() + mytheme +
		ylim(0, maxcolsize) + xlim(0, maxcolsize) + ggtitle("num ads before disperal against num ads after dispersal")


File$foodPerAd <- File$colony_food / File$num_ads
File$foodPerAd[which(File$foodPerAd == Inf)] <- NA

File$foodPerJuv <- File$colony_food / File$numjuvs
File$foodPerJuv[which(File$foodPerJuv == Inf)] <- NA

# Nest size before dispersal vs food per adult 

p7 <- ggplot(data = File, aes(x=num_adsB4_dispersal, y = foodPerAd, colour = prevDisp)) + geom_point() + stat_smooth(se = FALSE) +  
		mytheme + scale_y_continuous(limits = c(0, NA)) + ggtitle("colony food per adult before dispersal x=num_adsB4_dispersal, y = colony_food/num_ads")

#Nest size after dispersal with food per adult
p8 <- ggplot(data = File, aes(x=num_ads, y = foodPerAd, colour = prevDisp)) + geom_point() + stat_smooth(se = FALSE) +  
		mytheme + scale_y_continuous(limits = c(0, NA)) + ggtitle("num ads after dispersal vs per adult food")

p8a <- ggplot(data = File, aes(x=num_ads, y = foodPerJuv, colour = prevDisp)) + geom_point() + stat_smooth(se = FALSE) +  
		mytheme + scale_y_continuous(limits = c(0, NA)) + ggtitle("Number Juvs per food")


p9 <- ggplot(data = File, aes(x=num_ads, y = AveOffAd, colour = prevDisp )) + geom_point() + stat_smooth(se = FALSE) +  
		mytheme + scale_y_continuous(limits = c(0, NA)) + ggtitle("ave offspring per adults vs col size after dispersal")

# number juvs moulting with size after dispersal
p11 <- ggplot(data = File, aes(x=num_ads, y = pcntMoult, colour = prevDisp)) + stat_smooth(se = FALSE)  + geom_point() +  mytheme +
		scale_y_continuous(limits = c(0, 1)) + ggtitle("percentage juvs moults vs. num ads after dispersal")





deadcols <- subset(File, colAlive== 'dead')

if (nrow(deadcols) == 0){
	df <- data.frame()
	p13<-ggplot(df) + geom_point() + xlim(0, 10) + ylim(0, 100) + ggtitle("NO COLONIES DIED!!")
	print("no colonies died")
}else{
	# histogram of size of dead colonies
	p13 <- ggplot(data = subset(File, colAlive== 'dead'), aes(x= num_ads, fill = prevDisp )) + geom_histogram() + mytheme + ggtitle("histogram of size of colonies when died (num ads after dispersal)")
}
rm(deadcols)




#### Ind Files Graphs



AdsByPopAgeAndCol <- melt(File, id.vars = c("colony_ID", "colony_age", "num_adsB4_dispersal"), 
		measure.vars = c("adSz_B4_min", "adSz_B4_max", "adSz_B4_mean"), variable.name="MinMax", value.name="AdSize")

# colony size by adult size
p16 <- ggplot(data = AdsByPopAgeAndCol, aes(x= num_adsB4_dispersal, y = AdSize, colour = MinMax)) + geom_point() +
		stat_smooth(se = FALSE) + mytheme + scale_y_continuous(limits = c(0, 1)) + ggtitle("ad size vs col size b4 dispersal")



JuvsByPopAgeAndCol <- melt(File, id.vars = c("colony_ID", "colony_age", "num_adsB4_dispersal"), 
		measure.vars = c("jvSz_B4_min", "jvSz_B4_max", "jvSz_B4_mean"), variable.name="MinMax", value.name="JuvSize")

# Colony size by number of juvs
p17 <- ggplot(data = JuvsByPopAgeAndCol, aes(x= num_adsB4_dispersal, y = JuvSize, colour = MinMax)) + geom_point() +
		stat_smooth(se = FALSE) + mytheme + scale_y_continuous(limits = c(0, 1)) + ggtitle("juv size vs ads before dispersal")



grid.arrange(p0, p00, p1, p2, p3,  p4, p5, p6, p6a, p7, p8, p8a, p9, p11, p13, p16, p17, ncol = 1)

dev.off()