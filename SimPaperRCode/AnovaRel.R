
# Author: Ruth
# Paper:Evolution of kinship 
# Carrying out the ANOVA on kin preference 26th March 2013
###############################################################################


averages <- read.csv("kinshipEvolution/DataAnalysis/averages10000.csv")

subAves<-averages
subAves$relTrans<-asin(sqrt(subAves$rel))

detach(subAves)

attach(subAves) #attaching averages table#

##(1) Full model
lm1<-lm(relTrans~(R+ Beta+C)^3 + I(R^2) +I(R^3)  + I(Beta^2) +   I(Beta^3)+ I(C^2) + C*I(Beta^2) +C*I(Beta^3) + C*I(R^2) +C*I(R^3) + Beta*I(R^2) +Beta*I(R^3)+ R*I(C^2)+ Beta*I(C^2)  +R*I(Beta^2) +R*I(Beta^3) )
#see notes in R document Anova....  I am including an intercepts so no -1

anova(lm1)

anv1<-as.data.frame(anova(lm1))
rownames(anv1)[which(anv1$"Sum Sq"==min(anv1$"Sum Sq"))] ##gives minimum value

write.csv(as.matrix(anova(lm1)), file = "kinshipEvolution/ANOVAandStatsTests/NewAnova.csv", na = "")


### (2) removing C:I(Beta^3) and all its interactions

lm1<-lm(relTrans~(R+ Beta+C)^3 + I(R^2) +I(R^3)  + I(Beta^2) +   I(Beta^3)+ I(C^2) + C*I(Beta^2) + C*I(R^2) +C*I(R^3) + Beta*I(R^2) +Beta*I(R^3)+ R*I(C^2)+ Beta*I(C^2)  +R*I(Beta^2) +R*I(Beta^3) )
#see notes in R document Anova....  I am including an intercepts so no -1

anova(lm1)

anv1<-as.data.frame(anova(lm1))
rownames(anv1)[which(anv1$"Sum Sq"==min(anv1$"Sum Sq"))] ##gives minimum value

### (3) removing R:C and R:Beta:C

lm1<-lm(relTrans~R+ Beta+C + R*Beta + Beta*C + I(R^2) +I(R^3)  + I(Beta^2) +   I(Beta^3)+ I(C^2) + C*I(Beta^2) + C*I(R^2) +C*I(R^3) + Beta*I(R^2) +Beta*I(R^3)+ R*I(C^2)+ Beta*I(C^2)  +R*I(Beta^2) +R*I(Beta^3))
#see notes in R document Anova....  I am including an intercepts so no -1
anv1<-as.data.frame(anova(lm1))
rownames(anv1)[which(anv1$"Sum Sq"==min(anv1$"Sum Sq"))] ##gives minimum value

### (4) removing Beta^ 3 and all relevent interactions

lm1<-lm(relTrans~R+ Beta+C + R*Beta + Beta*C + I(R^2) +I(R^3)  + I(Beta^2) +  I(C^2) + C*I(Beta^2) + C*I(R^2) +C*I(R^3) + Beta*I(R^2) +Beta*I(R^3)+ R*I(C^2)+ Beta*I(C^2)  +R*I(Beta^2))
#see notes in R document Anova....  I am including an intercepts so no -1
anova(lm1)
anv1<-as.data.frame(anova(lm1))
rownames(anv1)[which(anv1$"Sum Sq"==min(anv1$"Sum Sq"))] ##gives minimum value


### (5) removing C: R^2 and C:R^3

lm1<-lm(relTrans~R+ Beta+C + R*Beta + Beta*C + I(R^2) +I(R^3)  + I(Beta^2) +  I(C^2) + C*I(Beta^2) + Beta*I(R^2) +Beta*I(R^3)+ R*I(C^2)+ Beta*I(C^2)  +R*I(Beta^2))
#see notes in R document Anova....  I am including an intercepts so no -1
anova(lm1)
anv1<-as.data.frame(anova(lm1))
rownames(anv1)[which(anv1$"Sum Sq"==min(anv1$"Sum Sq"))] ##gives minimum value

### (6) removing R: C^2 

lm1<-lm(relTrans~R+ Beta+C + R*Beta + Beta*C + I(R^2) +I(R^3)  + I(Beta^2) +  I(C^2) + C*I(Beta^2) + Beta*I(R^2) +Beta*I(R^3)+ Beta*I(C^2)  +R*I(Beta^2))
#see notes in R document Anova....  I am including an intercepts so no -1
anova(lm1)
anv1<-as.data.frame(anova(lm1))
rownames(anv1)[which(anv1$"Sum Sq"==min(anv1$"Sum Sq"))] ##gives minimum value

### (6) removing c:Beta^2

lm1<-lm(relTrans~R+ Beta+C + R*Beta + Beta*C + I(R^2) +I(R^3)  + I(Beta^2) +  I(C^2) + Beta*I(R^2) +Beta*I(R^3)+ Beta*I(C^2)  +R*I(Beta^2))
#see notes in R document Anova....  I am including an intercepts so no -1
anova(lm1)
anv1<-as.data.frame(anova(lm1))
rownames(anv1)[which(anv1$"Sum Sq"==min(anv1$"Sum Sq"))] ##gives minimum value

### (7) removing R^3 and all relevent interactions

lm1<-lm(relTrans~R+ Beta+C + R*Beta + Beta*C + I(R^2)  + I(Beta^2) +  I(C^2) + Beta*I(R^2) + Beta*I(C^2)  +R*I(Beta^2))
#see notes in R document Anova....  I am including an intercepts so no -1
anova(lm1)
anv1<-as.data.frame(anova(lm1))
rownames(anv1)[which(anv1$"Sum Sq"==min(anv1$"Sum Sq"))] ##gives minimum value


### (8) removing Beta:R^2

lm1<-lm(relTrans~R+ Beta+C + R*Beta + Beta*C + I(R^2)  + I(Beta^2) +  I(C^2)  + Beta*I(C^2)  +R*I(Beta^2))
#see notes in R document Anova....  I am including an intercepts so no -1
anova(lm1)
anv1<-as.data.frame(anova(lm1))
rownames(anv1)[which(anv1$"Sum Sq"==min(anv1$"Sum Sq"))] ##gives minimum value

### (9) removing R:Beta^2

lm1<-lm(relTrans~R+ Beta+C + R*Beta + Beta*C + I(R^2)  + I(Beta^2) +  I(C^2)  + Beta*I(C^2) )
#see notes in R document Anova....  I am including an intercepts so no -1
anova(lm1)
anv1<-as.data.frame(anova(lm1))
rownames(anv1)[which(anv1$"Sum Sq"==min(anv1$"Sum Sq"))] ##gives minimum value

### (10) removing C^2 and all relevent interactions

lm1<-lm(relTrans~R+ Beta+C + R*Beta + Beta*C + I(R^2)  + I(Beta^2) )
#see notes in R document Anova....  I am including an intercepts so no -1
anova(lm1)
anv1<-as.data.frame(anova(lm1))
rownames(anv1)[which(anv1$"Sum Sq"==min(anv1$"Sum Sq"))] ##gives minimum value

### (11) removing Beta^2

lm1<-lm(relTrans~R+ Beta+C + R*Beta + Beta*C + I(R^2) )
#see notes in R document Anova....  I am including an intercepts so no -1
anova(lm1)
anv1<-as.data.frame(anova(lm1))
rownames(anv1)[which(anv1$"Sum Sq"==min(anv1$"Sum Sq"))] ##gives minimum value

### (12) removing Beta:C

lm1<-lm(relTrans~R+ Beta+C + R*Beta + I(R^2) )
#see notes in R document Anova....  I am including an intercepts so no -1
anova(lm1)
anv1<-as.data.frame(anova(lm1))
rownames(anv1)[which(anv1$"Sum Sq"==min(anv1$"Sum Sq"))] ##gives minimum value

### (13) removing R:Beta

lm1<-lm(relTrans~R+ Beta+C + I(R^2) )
#see notes in R document Anova....  I am including an intercepts so no -1
anova(lm1)
anv1<-as.data.frame(anova(lm1))
rownames(anv1)[which(anv1$"Sum Sq"==min(anv1$"Sum Sq"))] ##gives minimum value


write.csv(as.matrix(anova(lm1)), file = "kinshipEvolution/ANOVAandStatsTests/NewAnova.csv", na = "") #output final model

