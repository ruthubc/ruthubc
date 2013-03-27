# TODO: Add comment
# 
# Author: Ruth
###############################################################################


##anova for group size

averages <- read.csv("kinshipEvolution/DataAnalysis/averages10000.csv")

subAves<-averages
subAves$avgGrSize<-log(subAves$avgGrSize)

detach(subAves)

attach(subAves) #attaching averages table

##(1) Full model
lm1<-lm(avgGrSize~(R+ Beta+C)^3 + I(R^2) +I(R^3)  + I(Beta^2) +   I(Beta^3)+ I(C^2) + C*I(Beta^2) +C*I(Beta^3) + C*I(R^2) +C*I(R^3) + Beta*I(R^2) +Beta*I(R^3)+ R*I(C^2)+ Beta*I(C^2)  +R*I(Beta^2) +R*I(Beta^3) )
#see notes in R document Anova....  I am including an intercepts so no -1

anova(lm1)

anv1<-as.data.frame(anova(lm1))


write.csv(as.matrix(anova(lm1)), file = "kinshipEvolution/ANOVAandStatsTests/NewAnova.csv", na = "")

rownames(anv1)[which(anv1$"Sum Sq"==min(anv1$"Sum Sq"))] ##gives minimum value

##(2) removing C:R^3

lm1<-lm(avgGrSize~(R+ Beta+C)^3 + I(R^2) +I(R^3)  + I(Beta^2) +   I(Beta^3)+ I(C^2) + C*I(Beta^2) +C*I(Beta^3) + C*I(R^2) + Beta*I(R^2) +Beta*I(R^3)+ R*I(C^2)+ Beta*I(C^2)  +R*I(Beta^2) +R*I(Beta^3) )

anova(lm1)

anv1<-as.data.frame(anova(lm1))

rownames(anv1)[which(anv1$"Sum Sq"==min(anv1$"Sum Sq"))]

##(3) removing Beta:R^3

lm1<-lm(avgGrSize~(R+ Beta+C)^3 + I(R^2) +I(R^3)  + I(Beta^2) +   I(Beta^3)+ I(C^2) + C*I(Beta^2) +C*I(Beta^3) + C*I(R^2) + Beta*I(R^2) + R*I(C^2)+ Beta*I(C^2)  +R*I(Beta^2) +R*I(Beta^3) )
anova(lm1)

anv1<-as.data.frame(anova(lm1))

rownames(anv1)[which(anv1$"Sum Sq"==min(anv1$"Sum Sq"))]

##(3) removing R^2 and all above intercepts

lm1<-lm(avgGrSize~(R+ Beta+C)^3 + I(Beta^2) +   I(Beta^3)+ I(C^2) + C*I(Beta^2) +C*I(Beta^3)  + R*I(C^2)+ Beta*I(C^2)  +R*I(Beta^2) +R*I(Beta^3) )
anova(lm1)

anv1<-as.data.frame(anova(lm1))

rownames(anv1)[which(anv1$"Sum Sq"==min(anv1$"Sum Sq"))]

##(4) removing R:C^2

lm1<-lm(avgGrSize~(R+ Beta+C)^3 + I(Beta^2) +   I(Beta^3)+ I(C^2) + C*I(Beta^2) +C*I(Beta^3) + Beta*I(C^2)  +R*I(Beta^2) +R*I(Beta^3) )
anova(lm1)

anv1<-as.data.frame(anova(lm1))

rownames(anv1)[which(anv1$"Sum Sq"==min(anv1$"Sum Sq"))]

############

write.csv(as.matrix(anova(lm1)), file = "kinshipEvolution/ANOVAandStatsTests/NewAnova.csv", na = "")
