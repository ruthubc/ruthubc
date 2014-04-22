# TODO: Add comment
# 
# Author: Ruth
###############################################################################

#################################################################
##Time eating vs Hunger
# Only include morning trials and might have to disregard boxes that did not eat for under 30 mins

#linear model
TimeHunMod1 <- lmer(TimeEatingLog1 ~ LogHunger*Treatment*Instar +
				(1|Instar:IndBoxID) + (1|Instar:IndBoxID:SpiderID), BoxComboMorn, REML = FALSE)

summary(TimeHunMod1)
anova(TimeHunMod1) 
qqnorm(resid(TimeHunMod1), main = "TimeHunMod1"); abline(0,1) # dips in the middle
overdisp_fun(TimeHunMod1) # it is massively over dispersed

# Glmer with untransformed data
TimeHunMod2 <- glmer(TotalTimeEating ~ Hunger*Treatment*Instar + 
				(1|Instar:IndBoxID) + (1|Instar:IndBoxID:SpiderID), BoxComboMorn, family = poisson(link = "log" ))

summary(TimeHunMod2)
anova(TimeHunMod2) 
qqnorm(resid(TimeHunMod2), main = "TimeHunMod2") # qqplot looked worse that TimeModHum1
overdisp_fun(TimeHunMod2) # But is less overdispersed, althoguh sitll over dispersed

#GLMER with transformed data, won't work with log transformed Time Eating
TimeHunMod3 <- glmer(TotalTimeEating ~ LogHunger*Treatment*Instar +
				(1|Instar:IndBoxID) + (1|Instar:IndBoxID:SpiderID), BoxComboMorn, family = poisson(link = "log" ))

summary(TimeHunMod3)
anova(TimeHunMod3) 
qqnorm(resid(TimeHunMod3), main = "TimeHunMod3"); abline(0, 1)  # same as TimeHunMod2
overdisp_fun(TimeHunMod3) # same as TimeHunMod2

#Trying to correct for overdispersion
BoxComboMorn$obsID<-as.factor(1:nrow(BoxComboMorn))

TimeHunMod4 <- glmer(TotalTimeEating ~ LogHunger*Treatment*Instar +
				(1|Instar:IndBoxID) + (1|Instar:IndBoxID:SpiderID) + (1|obsID), 
		BoxComboMorn, family = poisson(link = "log" ))

summary(TimeHunMod4)
anova(TimeHunMod4) 
qqnorm(resid(TimeHunMod4), main = "TimeHunMod4"); abline(0, 1) # once the line is added in it doesn't lookgood
overdisp_fun(TimeHunMod4) # massively over dispersed!! SHIT


#####The problem is that I need a zero-inflated model. So I have installed glmmadmb

TimeEatSub <- subset(BoxComboMorn, select = c("TotalTimeEating", "LogHunger", "Treatment", "Instar", "IndBoxID",
				"SpiderID"))

TimeEatSub <- na.omit(TimeEatSub)

TimeHunMod5 <- glmmadmb(TotalTimeEating ~LogHunger+Treatment+Instar +
				(1|IndBoxID) + (1|IndBoxID:SpiderID), data = TimeEatSub,
		zeroInflation = TRUE, family = "poisson")

### this doesn't work either.. I get an error meassage. I think best just to leave it

TimeHunRedMod <- glmer(TotalTimeEating ~ Hunger + Treatment +  Instar  + (1|Instar:LogHunger) +
				(1|Instar:IndBoxID) + (1|Instar:IndBoxID:SpiderID), BoxComboMorn, family = poisson(link = "log" ))



#############################################################################
#Time eating vs hunger given that they have fed .. not significant!! ZEROES REMOVED
##The zeros seem to matter in this case



BoxComboTest <- subset(BoxComboMorn, BoxCombo$Test == TRUE)

BoxMornFed <- subset(BoxComboMorn, TotalTimeEating > 0)

BoxMornFed$RowID<-(seq_len(nrow(BoxMornFed)))

TimeNoZeroHunMod1 <- lmer(TimeEatingLog ~ LogHunger*Treatment*Instar +
				(1|Instar:IndBoxID) + (1|Instar:IndBoxID:SpiderID), BoxMornFed, REML = FALSE)

qqnorm(resid(TimenNoZeroHunMod1), main = "TimenNoZeroHunMod1"); abline(0, 1) # 
overdisp_fun(TimeNoZeroHunMod1)# might be just about OK! Faraway book says might be fine p169
summary(TimeNoZeroHunMod1)
anova(TimenNoZeroHunMod1)

##testing the full model against completely reduced model: everything taken out
TimeNoZeroHunRedMod <- lmer(TotalTimeEating ~  1+
				(1|Instar:IndBoxID) + (1|Instar:IndBoxID:SpiderID), BoxMornFed)

qqnorm(resid(TimeNoZeroHunRedMod), main = "TimeNoZeroHunRedMod"); abline(0, 1) # strange
overdisp_fun(TimeNoZeroHunRedMod) #over disperesed

anova(GiveTimeHunRedMod, TimeNoZeroHunMod1 )

# removing all interactions as not significant
TimeNoZeroHunMod2 <- lmer(TimeEatingLog ~ LogHunger+Treatment+Instar  +
				(1|Instar:IndBoxID) + (1|Instar:IndBoxID:SpiderID), BoxMornFed, REML = FALSE)

qqnorm(resid(TimeNoZeroHunMod2), main = "TimeNoZeroHunMod2"); abline(0, 1) # same as model 1
overdisp_fun(TimeNoZeroHunMod2)# might be just about OK! Faraway book says might be fine p169
summary(TimeNoZeroHunMod2)
anova(TimeNoZeroHunMod2)

# removing all interactions as not significant
TimeNoZeroHunMod3 <- lmer(TimeEatingLog ~ Treatment+Instar  +
				(1|Instar:IndBoxID) + (1|Instar:IndBoxID:SpiderID), BoxMornFed, REML = FALSE)

qqnorm(resid(TimeNoZeroHunMod3), main = "TimeNoZeroHunMod3"); abline(0, 1) # same as model 1
overdisp_fun(TimeNoZeroHunMod3)# might be just about OK! Faraway book says might be fine p169
summary(TimeNoZeroHunMod3)
anova(TimeNoZeroHunMod3)

####################### Hunger rank vs time eating with zeros removed ###########

TimeEatHunRank1 <- lmer(TimeEatingLog ~ Rank.Hunger*Treatment*Instar + (1|Instar:IndBoxID) + 
				(1|Instar:IndBoxID:SpiderID), BoxMornFed)

modelPlot(TimeEatHunRank1)
summary(TimeEatHunRank1)

TimeEatHunRankRed <- lmer(TimeEatingLog ~ Treatment*Instar + (1|Instar:IndBoxID) + 
				(1|Instar:IndBoxID:SpiderID), BoxMornFed)

anova(TimeEatHunRankRed, TimeEatHunRank1) # not significant

