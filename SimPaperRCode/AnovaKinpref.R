# TODO: Add comment
# 
# Author: Ruth
###############################################################################



##End March2012 Anova analysis kin preference

averages <- read.csv("kinshipEvolution/DataAnalysis/averages10000.csv")

subAves<-averages

subAves$KinPrefTrans<-asin(sqrt(subAves$kinPref))

detach(subAves)
attach(subAves) #attaching averages table

##(1) Full model
lm1<-lm(KinPrefTrans~(R+ Beta+C)^3 + I(R^2) +I(R^3)  + I(Beta^2) +   I(Beta^3)+ I(C^2)+ C*I(Beta^2) +C*I(Beta^3) + C*I(R^2) +C*I(R^3) + Beta*I(R^2) +Beta*I(R^3)+ R*I(C^2)+ Beta*I(C^2)  +R*I(Beta^2) +R*I(Beta^3) )
#see notes in R document Anova....  I am including an intercepts so no -1

anova(lm1)

anv1<-as.data.frame(anova(lm1))

write.csv(as.matrix(anova(lm1)), file = "kinshipEvolution/ANOVAandStatsTests/NewAnova.csv", na = "") #outputs results

rownames(anv1)[which(anv1$"Sum Sq"==min(anv1$"Sum Sq"))] ##gives minimum value


##(2) Remove R:Beta^3
lm1<-lm(KinPrefTrans~(R+ Beta+C)^3 + I(R^2) +I(R^3)  + I(Beta^2) +   I(Beta^3)+ I(C^2)+ C*I(Beta^2) +C*I(Beta^3) + C*I(R^2) +C*I(R^3) + Beta*I(R^2) +Beta*I(R^3)+ R*I(C^2)+ Beta*I(C^2)  +R*I(Beta^2) )
#see notes in R document Anova....  I am including an intercepts so no -1

anova(lm1)

anv1<-as.data.frame(anova(lm1))


rownames(anv1)[which(anv1$"Sum Sq"==min(anv1$"Sum Sq"))] ##gives minimum value

##(3) Remove C:R^3
lm1<-lm(KinPrefTrans~(R+ Beta+C)^3 + I(R^2) +I(R^3)  + I(Beta^2) +   I(Beta^3)+ I(C^2)+ C*I(Beta^2) +C*I(Beta^3) + C*I(R^2) + Beta*I(R^2) +Beta*I(R^3)+ R*I(C^2)+ Beta*I(C^2)  +R*I(Beta^2) )
#see notes in R document Anova....  I am including an intercepts so no -1

anova(lm1)
anv1<-as.data.frame(anova(lm1))
rownames(anv1)[which(anv1$"Sum Sq"==min(anv1$"Sum Sq"))] ##gives minimum value


##(4) Remove R: C^2
lm1<-lm(KinPrefTrans~(R+ Beta+C)^3 + I(R^2) +I(R^3)  + I(Beta^2) +   I(Beta^3)+ I(C^2)+ C*I(Beta^2) +C*I(Beta^3) + C*I(R^2) + Beta*I(R^2) +Beta*I(R^3)+  Beta*I(C^2)  +R*I(Beta^2) )
#see notes in R document Anova....  I am including an intercepts so no -1

anova(lm1)
anv1<-as.data.frame(anova(lm1))
rownames(anv1)[which(anv1$"Sum Sq"==min(anv1$"Sum Sq"))] ##gives minimum value

##(5) Remove R^3 and all relevent interactoins
lm1<-lm(KinPrefTrans~(R+ Beta+C)^3 + I(R^2)  + I(Beta^2) +   I(Beta^3)+ I(C^2)+ C*I(Beta^2) +C*I(Beta^3) + C*I(R^2) + Beta*I(R^2) +  Beta*I(C^2)  +R*I(Beta^2) )
#see notes in R document Anova....  I am including an intercepts so no -1

anova(lm1)
anv1<-as.data.frame(anova(lm1))
rownames(anv1)[which(anv1$"Sum Sq"==min(anv1$"Sum Sq"))] ##gives minimum value



##(6) Remove C^2 and all relevent interactoins
lm1<-lm(KinPrefTrans~(R+ Beta+C)^3 + I(R^2)  + I(Beta^2) +   I(Beta^3)+  C*I(Beta^2) +C*I(Beta^3) + C*I(R^2) + Beta*I(R^2)   +R*I(Beta^2) )
#see notes in R document Anova....  I am including an intercepts so no -1

anova(lm1)
anv1<-as.data.frame(anova(lm1))
rownames(anv1)[which(anv1$"Sum Sq"==min(anv1$"Sum Sq"))] ##gives minimum value

##(7) Remove C:I(Beta^3)
lm1<-lm(KinPrefTrans~(R+ Beta+C)^3 + I(R^2)  + I(Beta^2) +   I(Beta^3)+  C*I(Beta^2) + C*I(R^2) + Beta*I(R^2)   +R*I(Beta^2) )
#see notes in R document Anova....  I am including an intercepts so no -1

anova(lm1)
anv1<-as.data.frame(anova(lm1))
rownames(anv1)[which(anv1$"Sum Sq"==min(anv1$"Sum Sq"))] ##gives minimum value

##(8) Remove I(Beta^3) 
lm1<-lm(KinPrefTrans~(R+ Beta+C)^3 + I(R^2)  + I(Beta^2) +  C*I(Beta^2) + C*I(R^2) + Beta*I(R^2)   +R*I(Beta^2) )
#see notes in R document Anova....  I am including an intercepts so no -1

anova(lm1)
anv1<-as.data.frame(anova(lm1))
rownames(anv1)[which(anv1$"Sum Sq"==min(anv1$"Sum Sq"))] ##gives minimum value

##(9) Remove C:I(R^2)  
lm1<-lm(KinPrefTrans~(R+ Beta+C)^3 + I(R^2)  + I(Beta^2) +  C*I(Beta^2) + Beta*I(R^2)   +R*I(Beta^2) )
#see notes in R document Anova....  I am including an intercepts so no -1

anova(lm1)
anv1<-as.data.frame(anova(lm1))
rownames(anv1)[which(anv1$"Sum Sq"==min(anv1$"Sum Sq"))] ##gives minimum value

##(10) Remove Beta:I(R^2) 
lm1<-lm(KinPrefTrans~(R+ Beta+C)^3 + I(R^2)  + I(Beta^2) +  C*I(Beta^2) +R*I(Beta^2) )
#see notes in R document Anova....  I am including an intercepts so no -1

anova(lm1)
anv1<-as.data.frame(anova(lm1))
rownames(anv1)[which(anv1$"Sum Sq"==min(anv1$"Sum Sq"))] ##gives minimum value


##(11) Remove R:C
lm1<-lm(KinPrefTrans~R+ Beta+C + R*Beta + Beta*C + I(R^2)  + I(Beta^2) +  C*I(Beta^2) +R*I(Beta^2) )
#see notes in R document Anova....  I am including an intercepts so no -1

anova(lm1)
anv1<-as.data.frame(anova(lm1))
rownames(anv1)[which(anv1$"Sum Sq"==min(anv1$"Sum Sq"))] ##gives minimum value

##########################
write.csv(as.matrix(anova(lm1)), file = "kinshipEvolution/ANOVAandStatsTests/NewAnova.csv", na = "")