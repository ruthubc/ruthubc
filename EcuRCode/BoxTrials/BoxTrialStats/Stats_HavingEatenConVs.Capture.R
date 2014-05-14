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
