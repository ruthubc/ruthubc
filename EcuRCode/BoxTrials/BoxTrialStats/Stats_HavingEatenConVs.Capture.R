# TODO: Add comment
# 
# Author: Ruth
###############################################################################


### eating with prey capture vs not prey capture

BoxComboEat <- BoxComboEat[!BoxComboEat$SpiderID == "sp336",]
BoxEatLarge<- BoxComboEat[BoxComboEat$Treatment == 'large',]

full<-glmer(RelHun ~ CaptureIndPos + Instar  + (1|IndBoxID) + (1|IndBoxID:SpiderID), 
		BoxEatLarge, family = binomial)


summary(full)

red<-glmer(RelHun ~ Instar + (1|IndBoxID) + (1|IndBoxID:SpiderID), 
		BoxEatLarge, family = binomial)

summary(red)

anova(full, red)
