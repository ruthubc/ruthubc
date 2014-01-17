# Statistical analysis of box trial data
# 
# Author: Ruth
###############################################################################
library (lme4)

#################################################################################
##### feeding vs prey capture

#glm family = binomial(logit), but random? ned GLMM generalized mixed effects model

#****** remember to REMOVE ***** night trials (or include in the model)
#MUST NOT FORGET INTERACTIONS
		
model = glmer(IndCapture ~ IndFeed + Instar + Treatment + (1|IndBoxID) + (1|SpiderID),
		data = BoxCombo, family = binomial(logit))

summary(model)
fixef(model)

overdisp_fun(model)
deviance(model)

## doesn't look like there are any effects for interaction but prob need to check properly