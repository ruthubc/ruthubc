# TODO: Add comment
# 
# Author: Ruth
###############################################################################



##Sept 27th 2012 Anova analysis kin preference

averages <- read.csv("D:/Dropbox/kinshipEvolution/DataAnalysis/averages.csv")

subAves<-averages

subAves$KinPrefTrans<-asin(subAves$kinPref)

detach(subAves)
attach(subAves) #attaching averages table

##(1) Full model
lm1<-lm(KinPrefTrans~(R+ Beta+C)^3 + I(R^2) +I(R^3)  + I(Beta^2) +   I(Beta^3)+ I(C^2)+ C*I(Beta^2) +C*I(Beta^3) + C*I(R^2) +C*I(R^3) + Beta*I(R^2) +Beta*I(R^3)+ R*I(C^2)+ Beta*I(C^2)  +R*I(Beta^2) +R*I(Beta^3) )
#see notes in R document Anova....  I am including an intercepts so no -1

anova(lm1)

anv1<-as.data.frame(anova(lm1))
rownames(anv1)[which(anv1$"Sum Sq"==min(anv1$"Sum Sq"))] ##gives minimum value

write.csv(as.matrix(anova(lm1)), file = "D:/Dropbox/kinshipEvolution/ANOVAandStatsTests/MyANOVA.csv", na = "") #outputs results


#(2)removing C:I(R^3)

lm1<-lm(KinPrefTrans~(R+ Beta+C)^3 + I(R^2) +I(R^3)  + I(Beta^2) +   I(Beta^3)+ I(C^2)+ C*I(Beta^2) +C*I(Beta^3) + C*I(R^2) + Beta*I(R^2) +Beta*I(R^3)+ R*I(C^2)+ Beta*I(C^2) +R*I(Beta^2) +R*I(Beta^3) )

anova(lm1)

anv1<-as.data.frame(anova(lm1))
rownames(anv1)[which(anv1$"Sum Sq"==min(anv1$"Sum Sq"))] ##gives minimum value


##(3) removing I(C^2) and therefore all other interactions with C^2

lm1<-lm(KinPrefTrans~(R+ Beta+C)^3 + I(R^2) +I(R^3)  + I(Beta^2) +   I(Beta^3)+ C*I(Beta^2) +C*I(Beta^3) + C*I(R^2) + Beta*I(R^2) +Beta*I(R^3) +R*I(Beta^2) +R*I(Beta^3) )

anova(lm1)

anv1<-as.data.frame(anova(lm1))
rownames(anv1)[which(anv1$"Sum Sq"==min(anv1$"Sum Sq"))] ##gives minimum value

###(4) removing Beta:I(R^2) and therefore Beta:R^3

lm1<-lm(KinPrefTrans~(R+ Beta+C)^3 + I(R^2) +I(R^3)  + I(Beta^2) +   I(Beta^3)+ C*I(Beta^2) +C*I(Beta^3) + C*I(R^2)  +R*I(Beta^2) +R*I(Beta^3) )

anova(lm1)

anv1<-as.data.frame(anova(lm1))
rownames(anv1)[which(anv1$"Sum Sq"==min(anv1$"Sum Sq"))] ##gives minimum value


##(5) Removing R:I(Beta^3)

lm1<-lm(KinPrefTrans~(R+ Beta+C)^3 + I(R^2) +I(R^3)  + I(Beta^2) +   I(Beta^3)+ C*I(Beta^2) +C*I(Beta^3) + C*I(R^2)  +R*I(Beta^2) )

anova(lm1)

anv1<-as.data.frame(anova(lm1))
rownames(anv1)[which(anv1$"Sum Sq"==min(anv1$"Sum Sq"))] ##gives minimum value

##(6) removing C:I(R^2)

lm1<-lm(KinPrefTrans~(R+ Beta+C)^3 + I(R^2) +I(R^3)  + I(Beta^2) +   I(Beta^3)+ C*I(Beta^2) +C*I(Beta^3)  +R*I(Beta^2) )

anova(lm1)

anv1<-as.data.frame(anova(lm1))
rownames(anv1)[which(anv1$"Sum Sq"==min(anv1$"Sum Sq"))] ##gives minimum value

## (7) removing I(R^3)

lm1<-lm(KinPrefTrans~(R+ Beta+C)^3 + I(R^2) + I(Beta^2) +   I(Beta^3)+ C*I(Beta^2) +C*I(Beta^3)  +R*I(Beta^2) )

anova(lm1)

anv1<-as.data.frame(anova(lm1))
rownames(anv1)[which(anv1$"Sum Sq"==min(anv1$"Sum Sq"))] ##gives minimum value


## (8) removing C:I(Beta^3)

lm1<-lm(KinPrefTrans~(R+ Beta+C)^3 + I(R^2) + I(Beta^2) +   I(Beta^3)+ C*I(Beta^2) +R*I(Beta^2) )

anova(lm1)

anv1<-as.data.frame(anova(lm1))
rownames(anv1)[which(anv1$"Sum Sq"==min(anv1$"Sum Sq"))] ##gives minimum value

##(9) removing I(Beta^3)


lm1<-lm(KinPrefTrans~(R+ Beta+C)^3 + I(R^2) + I(Beta^2) + C*I(Beta^2) +R*I(Beta^2) )

anova(lm1)

anv1<-as.data.frame(anova(lm1))

rownames(anv1)[which(anv1$"Sum Sq"==min(anv1$"Sum Sq"))] ##gives minimum value

### (10) C has the smallest sum of squares but can't remove that so removing R:C instead

lm1<-lm(KinPrefTrans~(R+ Beta)^2 + (C+Beta)^2 + I(R^2) + I(Beta^2) + C*I(Beta^2) +R*I(Beta^2) )

anova(lm1)

anv1<-as.data.frame(anova(lm1))

rownames(anv1)[which(anv1$"Sum Sq"==min(anv1$"Sum Sq"))] ##gives minimum value


### (10) C has the smallest sum of squares but can't remove that so removing C:I(Beta^2) instead

lm1<-lm(KinPrefTrans~(R+ Beta)^2 + (C+Beta)^2 + I(R^2) + I(Beta^2) +R*I(Beta^2) )

anova(lm1)

anv1<-as.data.frame(anova(lm1))

rownames(anv1)[which(anv1$"Sum Sq"==min(anv1$"Sum Sq"))] ##gives minimum value

write.csv(as.matrix(anova(lm1)), file = "D:/Dropbox/kinshipEvolution/ANOVAandStatsTests/MyANOVA.csv", na = "")