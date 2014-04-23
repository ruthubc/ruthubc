# TODO: Add comment
# 
# Author: Ruth
###############################################################################


### eating with prey capture vs not prey capture

BoxComboEat <- BoxComboEat[!BoxComboEat$SpiderID == "sp336",]
BoxEatLarge<- BoxComboEat[BoxComboEat$Treatment == 'large',]

full<-lmer(LogCond ~ CaptureIndPos + Instar  + (1|IndBoxID), 
		BoxEatLarge)

plot(full)
summary(full)

red<-lmer(LogCond ~ Instar + (1|IndBoxID), 
		BoxEatLarge)

summary(red)
plot(full)
anova(full, red)
