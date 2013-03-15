# TODO: Add comment
# 
# Author: Ruth
###############################################################################
 #test

##Anova relatedness sept 30th 2012

averages <- read.csv("D:/Dropbox/kinshipEvolution/DataAnalysis/averages.csv")

subAves<-averages
subAves$relTrans<-asin(subAves$rel)

detach(subAves)

attach(subAves) #attaching averages table#

##(1) Full model
lm1<-lm(relTrans~(R+ Beta+C)^3 + I(R^2) +I(R^3)  + I(Beta^2) +   I(Beta^3)+ I(C^2) + C*I(Beta^2) +C*I(Beta^3) + C*I(R^2) +C*I(R^3) + Beta*I(R^2) +Beta*I(R^3)+ R*I(C^2)+ Beta*I(C^2)  +R*I(Beta^2) +R*I(Beta^3) )
#see notes in R document Anova....  I am including an intercepts so no -1

anova(lm1)

anv1<-as.data.frame(anova(lm1))
rownames(anv1)[which(anv1$"Sum Sq"==min(anv1$"Sum Sq"))] ##gives minimum value

write.csv(as.matrix(anova(lm1)), file = "D:/Dropbox/kinshipEvolution/ANOVAandStatsTests/RelTrasFullANOVA.csv", na = "")


### (2) removing I(Beta^3) and all its interactions

lm1<-lm(relTrans~(R+ Beta+C)^3 + I(R^2) +I(R^3)  + I(Beta^2) + I(C^2) + C*I(Beta^2)  + C*I(R^2) +C*I(R^3) + Beta*I(R^2) +Beta*I(R^3)+ R*I(C^2)+ Beta*I(C^2)  +R*I(Beta^2) )
#see notes in R document Anova....  I am including an intercepts so no -1

anova(lm1)

anv1<-as.data.frame(anova(lm1))
rownames(anv1)[which(anv1$"Sum Sq"==min(anv1$"Sum Sq"))] ##gives minimum value



### (3) removing R:C and therefore R:C:Beta and all interactions R:C

lm1<-lm(relTrans~(Beta+C)^2 +(R+Beta)^2 + I(R^2) +I(R^3)  + I(Beta^2) + I(C^2) + C*I(Beta^2) + Beta*I(R^2) +Beta*I(R^3) + Beta*I(C^2)  +R*I(Beta^2) )


anova(lm1)

anv1<-as.data.frame(anova(lm1))
rownames(anv1)[which(anv1$"Sum Sq"==min(anv1$"Sum Sq"))] ##gives minimum value


### (4) removing C:Beta^2

lm1<-lm(relTrans~(Beta+C)^2 +(R+Beta)^2 + I(R^2) +I(R^3)  + I(Beta^2) + I(C^2) + Beta*I(R^2) +Beta*I(R^3) + Beta*I(C^2)  +R*I(Beta^2) )


anova(lm1)

anv1<-as.data.frame(anova(lm1))
rownames(anv1)[which(anv1$"Sum Sq"==min(anv1$"Sum Sq"))] ##gives minimum value



### (5) removing Beta:I(R^2) and also Beta:I(R^3)

lm1<-lm(relTrans~(Beta+C)^2 +(R+Beta)^2 + I(R^2) +I(R^3)  + I(Beta^2) + I(C^2) + Beta*I(C^2)  +R*I(Beta^2) )


anova(lm1)

anv1<-as.data.frame(anova(lm1))
rownames(anv1)[which(anv1$"Sum Sq"==min(anv1$"Sum Sq"))] ##gives minimum value



### (6) removing R:I(Beta^2)

lm1<-lm(relTrans~(Beta+C)^2 +(R+Beta)^2 + I(R^2) +I(R^3)  + I(Beta^2) + I(C^2) + Beta*I(C^2) )


anova(lm1)

anv1<-as.data.frame(anova(lm1))
rownames(anv1)[which(anv1$"Sum Sq"==min(anv1$"Sum Sq"))] ##gives minimum value


### (7) removing I(C^2) and all interactions involving C^2

lm1<-lm(relTrans~(Beta+C)^2 +(R+Beta)^2 + I(R^2) +I(R^3)  + I(Beta^2) )


anova(lm1)

anv1<-as.data.frame(anova(lm1))
rownames(anv1)[which(anv1$"Sum Sq"==min(anv1$"Sum Sq"))] ##gives minimum value



### (8) removing I(R^3)

lm1<-lm(relTrans~(Beta+C)^2 +(R+Beta)^2 + I(R^2) + I(Beta^2) )


anova(lm1)

anv1<-as.data.frame(anova(lm1))
rownames(anv1)[which(anv1$"Sum Sq"==min(anv1$"Sum Sq"))] ##gives minimum value

### (9) removing I(Beta^2)

lm1<-lm(relTrans~(Beta+C)^2 +(R+Beta)^2 + I(R^2) )


anova(lm1)

anv1<-as.data.frame(anova(lm1))
rownames(anv1)[which(anv1$"Sum Sq"==min(anv1$"Sum Sq"))] ##gives minimum value

### (10) removing Beta:C

lm1<-lm(relTrans~ Beta+C +(R+Beta)^2 + I(R^2) )


anova(lm1)

anv1<-as.data.frame(anova(lm1))
rownames(anv1)[which(anv1$"Sum Sq"==min(anv1$"Sum Sq"))] ##gives minimum value


### (11) removing Beta:R

lm1<-lm(relTrans~ Beta+C +R+ + I(R^2) )


anova(lm1)

anv1<-as.data.frame(anova(lm1))
rownames(anv1)[which(anv1$"Sum Sq"==min(anv1$"Sum Sq"))] ##gives minimum value

### (12) removing Beta removed as I got a bit carried away and continued when I should have stopped

#lm1<-lm(relTrans~ C +R+ I(R^2) )


#anova(lm1)

#anv1<-as.data.frame(anova(lm1))
#rownames(anv1)[which(anv1$"Sum Sq"==min(anv1$"Sum Sq"))] ##gives minimum value

write.csv(as.matrix(anova(lm1)), file = "D:/Dropbox/kinshipEvolution/ANOVAandStatsTests/MyANOVA.csv", na = "") #output final model

