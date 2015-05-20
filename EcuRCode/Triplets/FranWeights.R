# TODO: Add comment
# 
# Author: Ruth
###############################################################################
library(ggplot2)
library(lme4)
library(lmerTest)
library(summarySE)
library(plyr)



levels(Weights$Instar)



hist(log10(Weights$Weight), breaks = 30)

Weights$LogWeight <- log10(Weights$Weight)

Weights$LogLeg <- log10(Weights$LegLen)

hist(log10(Weights$LengthTibiapatella), breaks = 30)

ggplot(Weights, aes(x = Treatment, y = LogWeight)) + geom_boxplot()  +
		 stat_summary(fun.y=mean, colour="red", geom="point") + facet_grid(.~MeasNo)
 
ggplot(Weights, aes(x = Treatment, y = TotalLen)) + geom_boxplot()+
		 stat_summary(fun.y=mean, colour="red", geom="point") + facet_grid(.~MeasNo)

ggplot(Weights, aes(x = Treatment, y = log10(LegLen))) + geom_boxplot() + 
		 stat_summary(fun.y=mean, colour="red", geom="point") + facet_grid(.~MeasNo)
 

 
 
WeightsSum<- ddply(Weights, .(Treatment, MeasNo, Instar ), summarise, 
		 N = length(!is.na(JuvNo)),
		 Weight.Mean = mean(LogWeight, na.rm = TRUE),
		 Weight.sd   = sd(LogWeight),
		 Weight.se   = Weight.sd / sqrt(N),
		 Leg.Mean = mean(LogLeg, na.rm = TRUE),
		 Leg.sd   = sd(LogLeg),
		 Leg.se   = Leg.sd / sqrt(N)
 )
 
 
 pd <- position_dodge(0.1)
 
 ggplot(WeightsSum, aes(x=MeasNo, y=Weight.Mean, colour=Treatment)) + 
		 geom_errorbar(aes(ymin=Weight.Mean - Weight.se, ymax=Weight.Mean + Weight.se),width=.1, position=pd) +
		 geom_line(position=pd, aes(group= Treatment)) + geom_point(position=pd) + facet_grid(.~Instar)
 
 
 ggplot(WeightsSum, aes(x=MeasNo, y=Leg.Mean, ymax = max(Leg.Mean), colour=Treatment)) + 
		 geom_errorbar(aes(ymin=Leg.Mean - Leg.se, ymax=Leg.Mean + Leg.se),width=.1, position=pd) +
		 geom_line(position=pd, aes(group= Treatment)) + geom_point(position=pd) + facet_grid(.~Instar)
 
 mod1 <- lmer(log10(LengthTibiapatella) ~ Treatment*MeasNo + Instar + (1|ColonyID), Weights, REML = FALSE)
 
 summary(mod1)
 anova(mod1)
 
 
 mod2 <- lmer(TotalLength ~ Treatment*MeasNo + (1|ColonyID) + (1|Instar), Weights, REML = FALSE)
 
 summary(mod2)
 anova(mod2)
 
 mod3 <- lmer(LogWeight ~ Treatment*MeasNo + (1|ColonyID) + (1|Instar), Weights, REML = FALSE)
 
 summary(mod3)
 anova(mod3)
 
 
