# TODO: Add comment
# 
# Author: Ruth
###############################################################################


### eating with prey capture vs not prey capture

BoxComboEat <- BoxComboEat[!BoxComboEat$SpiderID == "sp336",]# removed this spider as hunger etc. = NA

## All data, there are some spiders with repeated measures, but I am assuming that IndTrialID takes care of that?????
#Can't put spider ID as a random variable with lmer
BoxEatCapFull <- lmer(LogCond ~ AveCap*Instar*Treatment + (1|IndBoxID), BoxFeedAve )

summary(BoxEatCapFull)

### remove three way interaction and instar and treatment interaction as very non significant

BoxEatCap1 <- lmer(LogCond ~ Instar + Treatment*AveCap + (1|IndBoxID), BoxFeedAve )

summary(BoxEatCap1)


### Just large
BoxEatLarge<- BoxFeedAve[BoxFeedAve$Treatment == 'large',]

Largefull<-lmer(LogCond ~ AveCap + Instar  + (1|IndBoxID) + (1 | IndBoxID:TrialID), 
		BoxEatLarge)



Largered<-lmer(LogCond ~ Instar + (1|IndBoxID) + (1 | IndBoxID:TrialID), 
		BoxEatLarge)


anova(Largefull, Largered)

### Just small
BoxEatSmall<- BoxFeedAve[BoxFeedAve$Treatment == 'small',]

Smallfull<-lmer(LogCond ~ AveCap + Instar  + (1|IndBoxID) + (1 | IndBoxID:TrialID), 
		BoxEatSmall)


Smallred<-lmer(LogCond ~ Instar + (1|IndBoxID) + (1 | IndBoxID:TrialID), 
		BoxEatSmall)


anova(Smallfull, Smallred)

## checking ratio of capture's to non-capturers by treatment

RatioFull<- glmer(PerNoCap ~ Treatment + Instar + (1|IndBoxID),	BoxFeedRatio, family = binomial)
summary(RatioFull)

RatioNull<-glmer(PerNoCap ~ Instar + (1|IndBoxID),	BoxFeedRatio, family = binomial)
anova(RatioFull, RatioNull)

################### Redoing the order with glmer

BoxFed<-subset(BoxComboMorn, IndFeed == "y")

FedCapFull.glmer <- glmer(IndCapture ~ Treatment+ LogCond + Instar +  Treatment*LogCond + (1|IndBoxID) + (1|IndBoxID:SpiderID),
		BoxFed, family = binomial(logit))

summary(FedCapFull.glmer)

## Testing just the interaction
FedCapInt.glmer <- glmer(IndCapture ~ Treatment+ LogCond + Instar  + (1|IndBoxID) + (1|IndBoxID:SpiderID),
		BoxFed, family = binomial(logit))
anova(FedCapFull.glmer, FedCapInt.glmer)

## Testing the interaction and LogCond
FedCapCond.glmer <- glmer(IndCapture ~ Treatment+ Instar  + (1|IndBoxID) + (1|IndBoxID:SpiderID),
		BoxFed, family = binomial(logit))
anova(FedCapFull.glmer, FedCapCond.glmer)

## Testing the interaction and Treatment
FedCapTreat.glmer <- glmer(IndCapture ~ LogCond+ Instar  + (1|IndBoxID) + (1|IndBoxID:SpiderID),
		BoxFed, family = binomial(logit))
anova(FedCapFull.glmer, FedCapTreat.glmer)

## Testing Instar

FedCapInstar.glmer <- glmer(IndCapture ~ Treatment+ LogCond +  Treatment*LogCond + (1|IndBoxID) + (1|IndBoxID:SpiderID),
		BoxFed, family = binomial(logit))

anova(FedCapFull.glmer, FedCapInstar.glmer)

#####Testing condition for large prey
FedCapLarge.glmer<-glmer(IndCapture ~ LogCond + Instar+ (1|IndBoxID) + (1|IndBoxID:SpiderID),
		subset(BoxFed, Treatment == "large"), family = binomial(logit))

summary(FedCapLarge.glmer)



FedCapLargeRed.glmer<-glmer(IndCapture ~ Instar+ (1|IndBoxID) + (1|IndBoxID:SpiderID),
		subset(BoxFed, Treatment == "large"), family = binomial(logit))

anova(FedCapLarge.glmer, FedCapLargeRed.glmer)

#####Testing condition for small prey
FedCapsmall.glmer<-glmer(IndCapture ~ Rank.Cond + Instar+ (1|IndBoxID) + (1|IndBoxID:SpiderID),
		subset(BoxFed, Treatment == "small"), family = binomial(logit))

FedCapsmallRed.glmer<-glmer(IndCapture ~ Instar+ (1|IndBoxID) + (1|IndBoxID:SpiderID),
		subset(BoxFed, Treatment == "small"), family = binomial(logit))

anova(FedCapsmall.glmer, FedCapsmallRed.glmer)

table(BoxFed$IndCapture, BoxFed$Treatment)


##Difference in number that didn't capture but ate between treatments

DifCap.lmer <- lmer(logCap.n ~ Treatment + Instar + (1|IndBoxID), BoxFeedRatio)
modelPlot(DifCap.lmer)

summary(DifCap.lmer)

DifCapTreat.lmer <- lmer(logCap.n ~ Instar + (1|IndBoxID), BoxFeedRatio)
anova(DifCap.lmer, DifCapTreat.lmer)

## getting means
by(BoxFeedRatio$logCap.n, BoxFeedRatio$Treatment, mean)

BoxRatioStats = function(x) c(mean = (10^mean(x))-1, se = (10^sd(x)-1)/sqrt(length(x)), n = length(x), max = (10^max(x))-1)
tapply(BoxFeedRatio$logCap.n, BoxFeedRatio$Treatment, BoxRatioStats)
