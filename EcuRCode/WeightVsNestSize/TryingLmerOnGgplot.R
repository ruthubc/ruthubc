# TODO: Add comment
# 
# Author: Ruth
###############################################################################

library("multcomp")
library("lme4")



condDiffLm <- lmer(sqCVCond~  logCtFm + I(logCtFm ^2) +  
				(1|NestID), data = subset(SpiNestAveMul, N > 0), REML = FALSE)

tmp <- as.data.frame(confint(glht(condDiffLm))$confint)
tmp$Comparison <- rownames(tmp)
ggplot(tmp, aes(x = Comparison, y = Estimate, ymin = lwr, ymax = upr)) + geom_errorbar() + geom_point()


# http://www.u.arizona.edu/~ljchang/NewSite/papers/AdvPlot_HO.pdf

fixParam<-fixef(condDiffLm)
ranParam<-ranef(condDiffLm)
params<-cbind(ranParam[1]+fixParam[1],ranParam[2]+fixParam[2])
plot(RT~Offer,data=data,col=rgb(0,0,0,.1),pch=16,cex=4,
		ylab="Reaction Time (Seconds)",xlab="Offer Amount ($)")
subNum<-unique(data$Subject)
for(i in 1:length(subNum)){
	abline(a=params[i,1], b=params[i,2],col="grey",lty=2,lwd=2)
}
abline(fixParam,lwd=6,col="red")

