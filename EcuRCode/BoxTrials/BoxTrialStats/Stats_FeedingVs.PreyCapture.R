# TODO: Add comment
# 
# Author: Ruth
#################################################################################
##### feeding vs prey capture


#I haven't included interactions but they are not significant, nor is treatment or instar.
#I made instar a random variable
BoxComboCap <- subset(BoxComboMorn, IndFeed != "NA") # removing NA lines as the bootrstrapping can't deal


##numbers

BoxComboCap$FedWords<-ifelse(BoxComboCap$FeedIndPos == "y", "Fed", "Did Not Feed")
BoxComboCap$CapWords<-ifelse(BoxComboCap$CaptureIndPos == "y", "Cap", "Did Not Cap")
CapVsFedTb<-table(BoxComboCap$FedWords,BoxComboCap$CapWords, BoxComboCap$Treatment )
CapVsFedTb

CapVsFedTb[1]

## stats tests
CapMod1 <- glmer(IndCapture ~ IndFeed*Treatment*Instar + (1|IndBoxID) + 
				(1|IndBoxID:SpiderID), BoxComboCap, family = binomial(logit))

overdisp_fun(CapMod1)
summary(CapMod1)
anova(CapMod1) 


##Removing all interaction terms as they are massively not significant

CapMod2 <- glmer(IndCapture ~ IndFeed + Treatment + Instar + (1|IndBoxID) + 
				(1|IndBoxID:SpiderID), BoxComboCap, family = binomial(logit))

overdisp_fun(CapMod2)
summary(CapMod2)
anova(CapMod2, CapMod1) 


##Removing treatment as it is very not significat from summary!

CapMod3 <- glmer(IndCapture ~ IndFeed  + Treatment + (1|IndBoxID) + 
				(1|IndBoxID:SpiderID), BoxComboCap, family = binomial(logit))

overdisp_fun(CapMod3)
summary(CapMod3)
anova(CapMod3, CapMod2) 


##Testing for effect of indfeed

CapMod4 <- glmer(IndCapture ~ Treatment + Instar + (1|IndBoxID) + 
				(1|IndBoxID:SpiderID), BoxComboCap, family = binomial(logit))

overdisp_fun(CapMod4)
summary(CapMod4)
anova(CapMod4, CapMod2)

RedCapMod <- glmer(IndCapture ~ 1+ (1|Instar:IndBoxID) + 
				(1|Instar:IndBoxID:SpiderID), BoxComboCap, family = binomial(logit))

overdisp_fun(CapMod4); overdisp_fun(RedCapMod)

anova(CapMod4, RedCapMod) # testing the full model against the reduced model
