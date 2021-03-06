# TODO: Add comment
# 
# Author: Ruth
###############################################################################


######################################################################################
# Eating at all (binary) vs everything else
## has everything in it model
#glmer(IndFeed ~ LogHunger + Treatment +  Instar + LogHunger:Treatment + LogHunger:Instar +
#	Treatment:Instar + LogHunger:Treatment:Instar+  (1|Instar) + (1|Instar:IndBoxID) + 
#	(1|Instar:IndBoxID:SpiderID), BoxComboMorn, family = binomial(logit))


## finding teh differences in hunger..perhaps I should do percentage difference?
EatAtAllvsHngMn<- aggregate(BoxComboMorn$Hunger, by = list(BoxComboMorn$FeedIndPos, BoxComboMorn$Treatment, BoxComboMorn$Instar), 
		FUN = mean, na.rm=TRUE)
EatAtAllvsHngMn

#function to give the absoulte difference and the percentage differences
HungDiff<-function(table){
	
	for(i in c(2,4,6,8)){
		
		diff<-table[i, 4] - table[i-1, 4]
		per<- ((table[i, 4] - table[i-1, 4])/ ((table[i, 4] + table[i-1, 4])/2)) *100 
		print(paste(table[i, 2], table[i, 3], "diff:",  diff, ", %diff:", per))
		
	}
	
	
}

HungDiff(EatAtAllvsHngMn)


## finding teh differences in hunger..perhaps I should do percentage difference?
CapturevsHngMn<- aggregate(BoxComboMorn$Hunger, by = list(BoxComboMorn$CaptureIndPos, BoxComboMorn$Treatment, BoxComboMorn$Instar), 
		FUN = mean, na.rm=TRUE)
CapturevsHngMn

#function to give the absoulte difference and the percentage differences


HungDiff(CapturevsHngMn)


######### Statistical tests#######

EatBinMod1 <- glmer(IndFeed ~ LogHunger*Treatment*Instar + (1:IndBoxID)+
				(1|IndBoxID:SpiderID), BoxComboMorn, family = binomial(logit))

overdisp_fun(EatBinMod1)
summary(EatBinMod1)
anova(EatBinMod1) 

#Removing 3-way interaction and instar:Huger and instar:treatment (as they don't make sense and are insignificant)
EatBinMod2 <- glmer(IndFeed ~ LogHunger + Treatment+ Instar + LogHunger:Treatment + LogHunger:Instar+
				+ (1|IndBoxID)+ (1|IndBoxID:SpiderID), BoxComboMorn, family = binomial(logit))


overdisp_fun(EatBinMod2)
summary(EatBinMod2)
anova(EatBinMod2)

#Removing Instar:hunger as not significant. Keeping instar in though because although not significat important.
EatBinMod3 <- glmer(IndFeed ~ LogHunger + Treatment + Instar + Treatment:LogHunger +  (1|IndBoxID)+
				(1|IndBoxID:SpiderID), BoxComboMorn, family = binomial(logit))


anova(EatBinMod2, EatBinMod3)
overdisp_fun(EatBinMod3)
summary(EatBinMod3)
anova(EatBinMod3)

# testing intereaction effect with reduced model
EatBinRedModInt <- glmer(IndFeed ~ LogHunger + Treatment + Instar + (1|IndBoxID)+
				(1|IndBoxID:SpiderID), BoxComboMorn, family = binomial(logit))


overdisp_fun(EatBinRedModInt)

anova(EatBinRedModInt, EatBinMod3) #very significant interaction effect

# testing treatment with reduced model
EatBinRedModTreatment <- glmer(IndFeed ~ LogHunger  + Instar + (1|IndBoxID)+
				(1|IndBoxID:SpiderID), BoxComboMorn, family = binomial(logit))

overdisp_fun(EatBinRedModTreatment)

anova(EatBinRedModTreatment, EatBinMod3)

#testing hunger with reduced model
EatBinRedModHun <- glmer(IndFeed ~ Treatment  + Instar + (1|IndBoxID) +
				(1|IndBoxID:SpiderID), BoxComboMorn, family = binomial(logit))

overdisp_fun(EatBinRedModHun)
summary(EatBinRedModHun)
anova(EatBinRedModHun)
anova(EatBinRedModHun, EatBinMod3)

#testing instar with reduced model
EatBinRedModHun <- glmer(IndFeed ~ Treatment  + LogHunger + LogHunger:Treatment + (1|IndBoxID)+ 
				(1|IndBoxID:SpiderID), BoxComboMorn, family = binomial(logit))

overdisp_fun(EatBinRedModHun)

anova(EatBinRedModHun, EatBinMod3)


###################Testing  instar and condition separately####


##########Large, sub1 ##########

EatBinSub1largeFull <- glmer(IndFeed ~ LogCond + (1|IndBoxID)+ (1|IndBoxID:SpiderID), 
		subset(BoxComboMorn, Instar == "Sub1" & Treatment == "large"), family = binomial(logit))

EatBinSub1largeRed <- glmer(IndFeed ~ 1 + (1|IndBoxID)+ (1|IndBoxID:SpiderID), 
		subset(BoxComboMorn, Instar == "Sub1" & Treatment == "large"), family = binomial(logit))

anova(EatBinSub1largeFull, EatBinSub1largeRed)


##########small, sub1 ##########

EatBinSub1smallFull <- glmer(IndFeed ~ LogCond + (1|IndBoxID)+ (1|IndBoxID:SpiderID), 
		subset(BoxComboMorn, Instar == "Sub1" & Treatment == "small"), family = binomial(logit))

EatBinSub1smallRed <- glmer(IndFeed ~ 1 + (1|IndBoxID)+ (1|IndBoxID:SpiderID), 
		subset(BoxComboMorn, Instar == "Sub1" & Treatment == "small"), family = binomial(logit))

anova(EatBinSub1smallFull, EatBinSub1smallRed)

##########Large, Sub2 ##########

EatBinSub2largeFull <- glmer(IndFeed ~ LogCond + (1|IndBoxID)+ (1|IndBoxID:SpiderID), 
		subset(BoxComboMorn, Instar == "Sub2" & Treatment == "large"), family = binomial(logit))

EatBinSub2largeRed <- glmer(IndFeed ~ 1 + (1|IndBoxID)+ (1|IndBoxID:SpiderID), 
		subset(BoxComboMorn, Instar == "Sub2" & Treatment == "large"), family = binomial(logit))

anova(EatBinSub2largeFull, EatBinSub2largeRed)


##########small, Sub2 ##########

EatBinSub2smallFull <- glmer(IndFeed ~ LogCond + (1|IndBoxID)+ (1|IndBoxID:SpiderID), 
		subset(BoxComboMorn, Instar == "Sub2" & Treatment == "small"), family = binomial(logit))

EatBinSub2smallRed <- glmer(IndFeed ~ 1 + (1|IndBoxID)+ (1|IndBoxID:SpiderID), 
		subset(BoxComboMorn, Instar == "Sub2" & Treatment == "small"), family = binomial(logit))

anova(EatBinSub2smallFull, EatBinSub2smallRed)



############# Eating at all vs hunger but switching the variables to make a linear model ###################
# Can't use BoxComboMorn and spiderID as random as I think there is not enough samples for spiderID
#Therefore I am using BoxComboAve

BoxAveHug <- na.omit(subset(BoxComboAve, select = c( LogHunger, Feed, Treatment, Instar, IndBoxID)))



HungEatMod1 <- lmer(LogHunger ~ Feed+Treatment+Instar + Feed:Treatment +
				(1|IndBoxID), BoxAveHug, REML = FALSE)
modelPlot(HungEatMod1)
anova(HungEatMod1)
summary(HungEatMod1)

HungEatMod2 <- lmer(LogHunger ~ Feed+Treatment+Instar  +
				(1|IndBoxID), BoxAveHug, REML = FALSE)

anova(HungEatMod2, HungEatMod1)

HungEatMod3 <- lmer(LogHunger ~ Treatment +Instar  +
				(1|IndBoxID), BoxAveHug, REML = FALSE)

anova(HungEatMod3, HungEatMod1)

HungEatMod4 <- lmer(LogHunger ~ Feed +Instar  +
				(1|IndBoxID), BoxAveHug, REML = FALSE)

anova(HungEatMod4, HungEatMod1)

HungEatMod5 <- lmer(LogHunger ~  Feed+Treatment + Feed:Treatment +
				(1|IndBoxID), BoxAveHug, REML = FALSE)

anova(HungEatMod5, HungEatMod1)

#### Need to individually test these things!

BoxAveFeed <- subset(BoxComboAve, Feed != "NA"  & LogCond != "NA")

### Sub1 & Large

EatSub1LgFull <- lmer(LogCond ~ Feed + (1|IndBoxID), subset(BoxAveFeed, 
				Instar == "Sub1" & Treatment == "large"), REML = FALSE   )

summary(EatSub1LgFull)

EatSub1LgRed <- lmer(LogCond ~ 1+ (1|IndBoxID), subset(BoxAveFeed, 
				Instar == "Sub1" & Treatment == "large"), REML = FALSE    )

modelPlot(EatSub1LgRed)

anova(EatSub1LgFull, EatSub1LgRed)

### Sub2 & Large

EatSub2LgFull <- lmer(LogCond ~ Feed + (1|IndBoxID), subset(BoxAveFeed, 
				Instar == "Sub2" & Treatment == "large"), REML = FALSE   )

summary(EatSub2LgFull)

EatSub2LgRed <- lmer(LogCond ~ 1+ (1|IndBoxID), subset(BoxAveFeed, 
				Instar == "Sub2" & Treatment == "large"), REML = FALSE    )


anova(EatSub2LgFull, EatSub2LgRed)


### Sub1 & small

EatSub1SmFull <- lmer(LogCond ~ Feed + (1|IndBoxID), subset(BoxAveFeed, 
				Instar == "Sub1" & Treatment == "small"), REML = FALSE   )

summary(EatSub1SmFull)

modelPlot(EatSub1SmFull)

EatSub1SmRed <- lmer(LogCond ~ 1+ (1|IndBoxID), subset(BoxAveFeed, 
				Instar == "Sub1" & Treatment == "small"), REML = FALSE    )



anova(EatSub1SmFull, EatSub1SmRed)

### Sub2 & small

EatSub2SmFull <- lmer(LogCond ~ Feed + (1|IndBoxID), subset(BoxAveFeed, 
				Instar == "Sub2" & Treatment == "small"), REML = FALSE   )

modelPlot(EatSub2SmFull)
summary(EatSub2SmFull)

EatSub2SmRed <- lmer(LogCond ~ 1+ (1|IndBoxID), subset(BoxAveFeed, 
				Instar == "Sub2" & Treatment == "small"), REML = FALSE    )


anova(EatSub2SmFull, EatSub2SmRed)


############Trying to make a logistic regression graph
Graph.glmer <- glmer(IndFeed ~ LogHunger + Instar+ (1|IndBoxID)+
				(1|IndBoxID:SpiderID), BoxComboMorn, family = binomial(logit))

plot(BoxComboMorn$LogHunger, BoxComboMorn$IndFeed)
curve(predict(Graph.glmer, data.frame(LogHunger=x), type = "resp"), add = TRUE)

library(ez)
ezPredict(fit = Graph.glmer)
ezPlot(Graph.glmer)

##NOTES look up package ez i.e. ezPredict etc.


### October 2014 ###

### Small - instars combined

EatSmallFull <- lmer(LogCond ~ Feed + Instar+  (1|IndBoxID), subset(BoxAveFeed, 
				Treatment == "small"), REML = FALSE   )

modelPlot(EatSmallFull)
summary(EatSmallFull)

EatSmallRed <- lmer(LogCond ~ 1+ Instar + (1|IndBoxID), subset(BoxAveFeed, 
				Treatment == "small"), REML = FALSE    )


anova(EatSmallFull, EatSmallRed)

### large - instars combined

EatlargeFull <- lmer(LogCond ~ Feed + Instar+  (1|IndBoxID), subset(BoxAveFeed, 
				Treatment == "large"), REML = FALSE   )

modelPlot(EatlargeFull)
summary(EatlargeFull)

EatlargeRed <- lmer(LogCond ~ 1+ Instar + (1|IndBoxID), subset(BoxAveFeed, 
				Treatment == "large"), REML = FALSE    )


anova(EatlargeFull, EatlargeRed)

