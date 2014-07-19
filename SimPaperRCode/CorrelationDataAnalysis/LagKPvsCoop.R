# TODO: Add comment
# Testing max lag with 
# Author: Ruth
###############################################################################
library (lme4)
library(lmerTest)

Corrls<-read.csv("kinshipEvolution/Correlations/LagMeansTransSplineSamples2014.csv")

##(1) Full model
lm1<-lmer(SOMETHINGHERE~(R+ Beta+C)^3 + I(R^2) +I(R^3)  + I(Beta^2) +   I(Beta^3)+ I(C^2) + C*I(Beta^2) +
				C*I(Beta^3) + C*I(R^2) +C*I(R^3) + Beta*I(R^2) +Beta*I(R^3)+ R*I(C^2)+ Beta*I(C^2) + 
				R*I(Beta^2) +R*I(Beta^3) + (1|NUMBER), Corrls, REML = FALSE)
#see notes in R document Anova....  I am including an intercepts so no -1

anova(lm1)

anv1<-as.data.frame(anova(lm1))
rownames(anv1)[which(anv1$"Sum Sq"==min(anv1$"Sum Sq"))] ##gives minimum value

write.csv(as.matrix(anova(lm1)), file = "kinshipEvolution/ANOVAandStatsTests/LagsAnova.csv", na = "")
