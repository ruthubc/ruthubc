# Statistical analysis of box trial data
# 
# Author: Ruth
###############################################################################
library (lme4)

#################################################################################
##### feeding vs prey capture

#I haven't included interactions in this but not sure there are significant.
		
CapModel = glmer(IndCapture ~ IndFeed + Instar + (Weight.1|Instar) + Treatment + (1+ SpiderID|IndBoxID), data = BoxComboMorn, family = binomial(logit))

CapRedMod= glmer(IndCapture ~  IndFeed + Instar + Treatment + (1|IndBoxID) + (1|SpiderID),
		data = BoxComboMorn, family = binomial(logit) )

summary(CapModel)
fixef(CapModel)
ranef(CapModel)

overdisp_fun(model)
deviance(model)

## doesn't look like there are any effects for interaction but prob need to check

summary(RedMod) # don't know what this means!!

anova(model, RedMod)

anova(model) ## I have no idea the point in this..check the simulation paper anova


###################################################################################
#### Time eating vs hunger







