
# Author: Ruth
# Paper:Evolution of kinship 
# Carrying out the ANOVA on average Cooperation 26th March 2013
###############################################################################


averages <- read.csv("kinshipEvolution/DataAnalysis/averages10000.csv")

subAves<-averages
subAves$avCoopTrans<-asin(sqrt(subAves$avgCoop))

detach(subAves)

attach(subAves) #attaching averages table

##(1) Full model
lm1<-lm(avCoopTrans~(R+ Beta+C)^3 + I(R^2) +I(R^3)  + I(Beta^2) +   I(Beta^3)+ I(C^2) + C*I(Beta^2) +C*I(Beta^3) + C*I(R^2) +C*I(R^3) + Beta*I(R^2) +Beta*I(R^3)+ R*I(C^2)+ Beta*I(C^2)  +R*I(Beta^2) +R*I(Beta^3) )
#see notes in R document Anova....  I am including an intercepts so no -1

anova(lm1)

anv1<-as.data.frame(anova(lm1))
rownames(anv1)[which(anv1$"Sum Sq"==min(anv1$"Sum Sq"))] ##gives minimum value

write.csv(as.matrix(anova(lm1)), file = "kinshipEvolution/ANOVAandStatsTests/NewAnova.csv", na = "")


##(2) removing R^3 and all R^3 intercept terms
lm1<-lm(avCoopTrans~(R+ Beta+C)^3 + I(R^2) + I(Beta^2) +   I(Beta^3)+ I(C^2) + C*I(Beta^2) +C*I(Beta^3) + C*I(R^2) + Beta*I(R^2)+ R*I(C^2)+ Beta*I(C^2)  +R*I(Beta^2) )


anova(lm1)

anv1<-as.data.frame(anova(lm1))

rownames(anv1)[which(anv1$"Sum Sq"==min(anv1$"Sum Sq"))]


##(3) removing R:I(C^2)
lm1<-lm(avCoopTrans~(R+ Beta+C)^3 + I(R^2) + I(Beta^2) +   I(Beta^3)+ I(C^2) + C*I(Beta^2) +C*I(Beta^3) + C*I(R^2) + Beta*I(R^2)+ Beta*I(C^2)  +R*I(Beta^2) )


anova(lm1)

anv1<-as.data.frame(anova(lm1))

rownames(anv1)[which(anv1$"Sum Sq"==min(anv1$"Sum Sq"))]

##(4) removing C:I(R^2)
lm1<-lm(avCoopTrans~(R+ Beta+C)^3 + I(R^2) + I(Beta^2) +   I(Beta^3)+ I(C^2) + C*I(Beta^2) +C*I(Beta^3) + Beta*I(R^2)+ Beta*I(C^2)  +R*I(Beta^2) )


anova(lm1)

anv1<-as.data.frame(anova(lm1))

rownames(anv1)[which(anv1$"Sum Sq"==min(anv1$"Sum Sq"))]

##(5) removing C:I(Beta^3)
lm1<-lm(avCoopTrans~(R+ Beta+C)^3 + I(R^2) + I(Beta^2) +   I(Beta^3)+ I(C^2) + C*I(Beta^2) + Beta*I(R^2)+ Beta*I(C^2)  +R*I(Beta^2) )


anova(lm1)

anv1<-as.data.frame(anova(lm1))

rownames(anv1)[which(anv1$"Sum Sq"==min(anv1$"Sum Sq"))]

###########new averages this is the final ONE.. all terms significant
##(6) removing C^2 and all terms with c^2 
lm1<-lm(avCoopTrans~(R+ Beta+C)^3 + I(R^2) + I(Beta^2) +   I(Beta^3) + C*I(Beta^2) + Beta*I(R^2) +R*I(Beta^2) )


anova(lm1)

anv1<-as.data.frame(anova(lm1))

rownames(anv1)[which(anv1$"Sum Sq"==min(anv1$"Sum Sq"))]


write.csv(as.matrix(anova(lm1)), file = "kinshipEvolution/ANOVAandStatsTests/NewAnova.csv", na = "")
