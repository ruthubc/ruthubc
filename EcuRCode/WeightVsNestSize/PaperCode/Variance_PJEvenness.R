# TODO: Add comment
# 
# Author: user
###############################################################################



PJEvenVar <- function(data){
	# do input var from FunctionCalculateVariance_overMax
	
	spidersBoot <- select(data, one_of(c("NestID", "Instar", "CountFemales", "logCtFm", "InstarNumber", "InstarSex", "type", "LegLen.mm")))
	spidersBoot <- spidersBoot[complete.cases(spidersBoot), ]
	
	spidersBoot<-ddply(spidersBoot, .(NestID, Instar), transform, legSum=sum(LegLen.mm))
	spidersBoot<-ddply(spidersBoot, .(NestID, Instar), transform, count=length(LegLen.mm))
	
	
	spidersBoot$PJEvenFrac <-(spidersBoot$LegLen.mm/ spidersBoot$legSum) * log((spidersBoot$LegLen.mm/ spidersBoot$legSum))
	
	spidersBoot<-ddply(spidersBoot, .(NestID, Instar), transform, PJFracSum=sum(PJEvenFrac))
	
	spidersBoot$PJEven <- -spidersBoot$PJFracSum/log(spidersBoot$count)
	ggplot(spidersBoot, aes(x = PJEven)) + geom_histogram()
	
	
	
	
	
	
}