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

#### Weights

Weights$ID<-seq.int(nrow(Weights))

Weights$LogWeight <- log10(Weights$Weight)
# scaling the weight and leg length so they can be plotted together


Juv2Weights <- subset(Weights, Instar =="2")
Juv2Weights$LogWt.Scal <- scale(Juv2Weights$LogWeight,  center = TRUE, scale = TRUE)[,1]
Juv3Weights <- subset(Weights, Instar =="3")
Juv3Weights$LogWt.Scal <- scale(Juv3Weights$LogWeight,  center = TRUE, scale = TRUE)[,1]
Weights <- rbind(Juv2Weights, Juv3Weights)


ggplot(data = Weights, aes(LogWeight)) + geom_histogram() + facet_wrap(~Instar)# log weights normal

ggplot(data = Weights, aes(LogWt.Scal)) + geom_histogram() + facet_wrap(~Instar)



## LegLength

Weights$LogLeg <- log10(Weights$LegLen)

## Scaling legs

Juv2Weights <- subset(Weights, Instar =="2")
Juv2Weights$LogLeg.Scal <- scale(Juv2Weights$LogLeg,  center = TRUE, scale = TRUE)[,1]
Juv3Weights <- subset(Weights, Instar =="3")
Juv3Weights$LogLeg.Scal <- scale(Juv3Weights$LogLeg,  center = TRUE, scale = TRUE)[,1]
Weights <- rbind(Juv2Weights, Juv3Weights)


ggplot(data = Weights, aes(LegLen)) + geom_histogram() + facet_wrap(~Instar)# log weights normal

ggplot(data = Weights, aes(LogLeg.Scal)) + geom_histogram() + facet_wrap(~Instar)



### Graphs

ggplot(Weights, aes(x = Treatment, y = LogWeight)) + geom_boxplot()  +
		 stat_summary(fun.y=mean, colour="red", geom="point") + facet_grid(.~MeasNo)
 
ggplot(Weights, aes(x = Treatment, y = TotalLen)) + geom_boxplot()+
		 stat_summary(fun.y=mean, colour="red", geom="point") + facet_grid(.~MeasNo)

ggplot(Weights, aes(x = Treatment, y = log10(LegLen))) + geom_boxplot() + 
		 stat_summary(fun.y=mean, colour="red", geom="point") + facet_grid(.~MeasNo)
 

 
 
WeightsSum<- ddply(Weights, .(Treatment, MeasNo, Instar ), summarise, 
		 N = length(!is.na(ID)),
		 Weight.Mean = mean(LogWeight, na.rm = TRUE),
		 Weight.sd   = sd(LogWeight),
		 Weight.se   = Weight.sd / sqrt(N),
		 Leg.Mean = mean(LogLeg, na.rm = TRUE),
		 Leg.sd   = sd(LogLeg),
		 Leg.se   = Leg.sd / sqrt(N),

 )
 

WeightsScaleSum<- ddply(Weights, .(Treatment, MeasNo), summarise, 
		N = length(!is.na(ID)),
 		WtScl.Mean = mean(LogWt.Scal, na.rm = TRUE),
 		WtScl.sd   = sd(LogWt.Scal),
		WtScl.se   = WtScl.sd  / sqrt(N),
 		LegScl.Mean = mean(LogLeg.Scal, na.rm = TRUE),
 		LegScl.sd   = sd(LogLeg.Scal),
 		LegScl.se   = LegScl.sd  / sqrt(N)
)

WeightsScaleSum$MeasNo <- as.numeric(WeightsScaleSum$MeasNo)
 
 # box plots
 pd <- position_dodge(0.1)
 
 ggplot(WeightsSum, aes(x=MeasNo, y=Weight.Mean, colour=Treatment)) + 
		 geom_errorbar(aes(ymin=Weight.Mean - Weight.se, ymax=Weight.Mean + Weight.se),width=.1, position=pd) +
		 geom_line(position=pd, aes(group= Treatment)) + geom_point(position=pd) + facet_grid(.~Instar)
 
 
 ggplot(WeightsSum, aes(x=MeasNo, y=Leg.Mean, ymax = max(Leg.Mean), colour=Treatment)) + 
		 geom_errorbar(aes(ymin=Leg.Mean - Leg.se, ymax=Leg.Mean + Leg.se),width=.1, position=pd) +
		 geom_line(position=pd, aes(group= Treatment)) + geom_point(position=pd) + facet_grid(.~Instar)
 

 
# Dot and line!

ggplot(WeightsScaleSum, aes(x = MeasNo, y = LegScl.Mean, colour = Treatment)) + geom_point() + geom_line()
 
 

## Statistical analysis 
 mod1 <- lmer(log10(LengthTibiapatella) ~ Treatment*MeasNo + Instar + (1|ColonyID), Weights, REML = FALSE)
 
 summary(mod1)
 anova(mod1)
 
 
 mod2 <- lmer(TotalLength ~ Treatment*MeasNo + (1|ColonyID) + (1|Instar), Weights, REML = FALSE)
 
 summary(mod2)
 anova(mod2)
 
 mod3 <- lmer(LogWeight ~ Treatment*MeasNo + (1|ColonyID) + (1|Instar), Weights, REML = FALSE)
 
 summary(mod3)
 anova(mod3)
 
 
