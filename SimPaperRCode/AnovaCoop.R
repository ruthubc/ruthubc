# TODO: Add comment
# 
# Author: Ruth
# Carrying out the ANOVA for COOP
###############################################################################




##anova for group size

averages <- read.csv("D:/Dropbox/kinshipEvolution/DataAnalysis/averages.csv")

subAves<-averages
subAves$avCoopTrans<-asin(subAves$avgCoop)

detach(subAves)

attach(subAves) #attaching averages table

##(1) Full model
lm1<-lm(avCoopTrans~(R+ Beta+C)^3 + I(R^2) +I(R^3)  + I(Beta^2) +   I(Beta^3)+ I(C^2) + C*I(Beta^2) +C*I(Beta^3) + C*I(R^2) +C*I(R^3) + Beta*I(R^2) +Beta*I(R^3)+ R*I(C^2)+ Beta*I(C^2)  +R*I(Beta^2) +R*I(Beta^3) )
#see notes in R document Anova....  I am including an intercepts so no -1

anova(lm1)

anv1<-as.data.frame(anova(lm1))
rownames(anv1)[which(anv1$"Sum Sq"==min(anv1$"Sum Sq"))] ##gives minimum value

write.csv(as.matrix(anova(lm1)), file = "D:/Dropbox/kinshipEvolution/ANOVAandStatsTests/AveGpTransFullANOVA.csv", na = "")


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


##(6) removing C^2 and all terms with c^2 
lm1<-lm(avCoopTrans~(R+ Beta+C)^3 + I(R^2) + I(Beta^2) +   I(Beta^3) + C*I(Beta^2) + Beta*I(R^2) +R*I(Beta^2) )


anova(lm1)

anv1<-as.data.frame(anova(lm1))

rownames(anv1)[which(anv1$"Sum Sq"==min(anv1$"Sum Sq"))]


write.csv(as.matrix(anova(lm1)), file = "D:/Dropbox/kinshipEvolution/ANOVAandStatsTests/MyANOVA.csv", na = "")
