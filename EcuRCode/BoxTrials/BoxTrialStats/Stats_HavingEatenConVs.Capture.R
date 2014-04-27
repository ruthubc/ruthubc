# TODO: Add comment
# 
# Author: Ruth
###############################################################################


### eating with prey capture vs not prey capture

BoxComboEat <- BoxComboEat[!BoxComboEat$SpiderID == "sp336",]

## All data

BoxEatCapFull <- lmer(LogCond ~ Cap*Instar*Treatment + (1|IndBoxID), BoxComboEat )

summary(BoxEatCapFull)


### Just large
BoxEatLarge<- BoxComboEat[BoxComboEat$Treatment == 'large',]

full<-lmer(LogCond ~ Cap + Instar  + (1|IndBoxID) + (1|IndBoxID:TrialID), 
		BoxEatLarge)

plot(full)
summary(full)

red<-lmer(LogCond ~ Instar + (1|IndBoxID), 
		BoxEatLarge)

summary(red)
plot(full)
anova(full, red)
