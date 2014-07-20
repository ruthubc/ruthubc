# GroupSizeVsCoop Max Correlation ANOVA
###############################################################################

library (lme4)
library(lmerTest)

Corrs<-read.csv("kinshipEvolution/Correlations/LagMeansTransSplineSamples2014.csv")
Corrs$Run <- paste(Corrs$R, Corrs$Beta, Corrs$C) 


##(1) Full model
lm1<-lmer(GSvsCoopMax~(R+ Beta+C)^3 + I(R^2) +I(R^3)  + I(Beta^2) +   I(Beta^3)+ I(C^2) + C*I(Beta^2) +
				C*I(Beta^3) + C*I(R^2) +C*I(R^3) + Beta*I(R^2) +Beta*I(R^3)+ R*I(C^2)+ Beta*I(C^2) + 
				R*I(Beta^2) +R*I(Beta^3) + (1|Run), Corrs, REML = FALSE)
#see notes in R document Anova....  I am including an intercepts so no -1

statsFun(lm1, 'n')

write.csv(as.matrix(anova(lm1)), file = "kinshipEvolution/Correlations/Stats/GSvsCoopCorrAnova.csv", na = "")


## Removing c^2 and all related interactions
lm2<-lmer(GSvsCoopMax~(R+ Beta+C)^3 + I(R^2) +I(R^3)  + I(Beta^2) +   I(Beta^3)+ C*I(Beta^2) +
				C*I(Beta^3) + C*I(R^2) +C*I(R^3) + Beta*I(R^2) +Beta*I(R^3) + 
				R*I(Beta^2) +R*I(Beta^3) + (1|Run), Corrs, REML = FALSE)

statsFun(lm2, 'n')

## Removing R:I(Beta^2) and  R:I(Beta^3) 
lm3<-lmer(GSvsCoopMax~(R+ Beta+C)^3 + I(R^2) +I(R^3)  + I(Beta^2) +   I(Beta^3)+ C*I(Beta^2) +
				C*I(Beta^3) + C*I(R^2) +C*I(R^3) + Beta*I(R^2) +Beta*I(R^3) + 
				 (1|Run), Corrs, REML = FALSE)

statsFun(lm3, 'n')

## Removing Beta:I(R^3)
lm4<-lmer(GSvsCoopMax~(R+ Beta+C)^3 + I(R^2) +I(R^3)  + I(Beta^2) +   I(Beta^3)+ C*I(Beta^2) +
				C*I(Beta^3) + C*I(R^2) +C*I(R^3) + Beta*I(R^2) + 
				(1|Run), Corrs, REML = FALSE)

statsFun(lm4, 'y')


## Removing Beta^3 and any beta^3 interactions
lm5<-lmer(GSvsCoopMax~(R+ Beta+C)^3 + I(R^2) +I(R^3)  + I(Beta^2) + C*I(Beta^2) +
				+ C*I(R^2) +C*I(R^3) + Beta*I(R^2) + 
				(1|Run), Corrs, REML = FALSE)

statsFun(lm5, 'y')

## Removing C:I(R^3)
lm6<-lmer(GSvsCoopMax~(R+ Beta+C)^3 + I(R^2) +I(R^3)  + I(Beta^2) + C*I(Beta^2) +
				+ C*I(R^2) + Beta*I(R^2) + 
				(1|Run), Corrs, REML = FALSE)

statsFun(lm6, 'n')

## Removing R^2 and relevant interactions and higher order terms
lm7<-lmer(GSvsCoopMax~(R+ Beta+C)^3   + I(Beta^2) + C*I(Beta^2) +
				(1|Run), Corrs, REML = FALSE)

statsFun(lm7, 'y')

## Removing R:Beta:C
lm8<-lmer(GSvsCoopMax~(R+ Beta+C)^2 + I(Beta^2) + C*I(Beta^2) +
				(1|Run), Corrs, REML = FALSE)

statsFun(lm8, 'y')

##DONE Everything except the main constants are now significant

write.csv(as.matrix(anova(lm8)), file = "kinshipEvolution/Correlations/Stats/GSvsCoopCorrAnovaFinal.csv", na = "")