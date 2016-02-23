# TODO: Add comment
# 
# Author: Ruth
###############################################################################

require(reshape2)
library (ggplot2)
library (gridExtra)
library (lme4)
library(lmerTest)
library(visreg)
mytheme <-theme_bw(base_size=15)  + theme(plot.title = element_text(vjust=2), panel.margin= unit(0.75, "lines"), axis.title.y = element_text(vjust=0),
		plot.margin=unit(c(1,1,1.5,1.2),"cm"), panel.border = element_rect(fill = NA, colour = "grey", linetype=1, size = 1), panel.grid.major = element_blank(), panel.grid.minor = element_blank())



spiders<- subset(spiders, !is.na(logWt) )


#calculating the residual index
ggplot(spiders, aes(logLeg, logWt)) + geom_point() + geom_smooth(method=lm, fullrange = TRUE)

model <- lm(logWt ~ logLeg, spiders )

summary(model)
spiders$condResiduals <- resid(model)  # putting the residuales into the dable

ggplot(spiders, aes(condResiduals)) + geom_histogram() # normal

################ graphs  ############

# condition vs nest size
ggplot(subset(spiders, type == 'multiple'), aes(x = logCtFm, y = condResiduals)) + geom_point()+ 
		geom_smooth(method = "lm", formula =y ~  poly(x, 1, raw = TRUE), se = TRUE) +
		facet_wrap(~Instar, scales = "free_y") + xlab("Log Nest Size (num ad females)") + ylab("Condition Residuals") + mytheme

## Stats condition vs nest size
condResLmFull <- lmer(condResiduals ~  logCtFm + Instar + logCtFm:Instar  + 
				(1|NestID), data = spiders, REML = FALSE)

visreg(condResLmFull, xvar = "logCtFm", by = "Instar")

anova(condResLmFull) # seems significant

condResLmRed <- lmer(condResiduals ~   Instar + 
				(1|NestID), data = spiders, REML = FALSE)


anova(condResLmFull, condResLmRed) # comparing full model to reduced model

levels(spiders$Instar)

condResByInstar <- lmer(condResiduals ~  logCtFm  + 
				(1|NestID), data = subset(spiders, Instar == 'AdMale'), REML = FALSE)

summary(condResByInstar)

#adult high significant
# Sub2 significant
#sub1 highly significant
# sub2 not signifcant
# juv 4 not significant
#submale not significant
#ad male not significant


###############Back to graphs ##############
#condition vs instar Males have significantly reduced condition compared to all other instars
ggplot(spiders, aes(x = Instar, y = condResiduals)) + geom_boxplot() + mytheme


# cond residuals vs multiple and single nests
ggplot(subset(spiders, Instar == 'Adult'), aes(x = type, y = condResiduals)) + geom_boxplot() + mytheme


## variances for residual conditions
CVCondRes<- ddply(spiders, .(NestID, type, Instar, logCtFm, CountFemales), summarise,
		N = length(!is.na(ID)),
		meanCondRes = mean(condResiduals, na.rm = TRUE),
		sdCondRes = sd(condResiduals, na.rm = TRUE),
		CVCondRes = (1 + (1/N)) * (sdCondRes / meanCondRes)
)


ggplot(CVCondRes, aes((CVCondRes))) + geom_histogram()  # pretty normal without transformatoin

# graphs of cond Variance, not very exciting!
ggplot(subset(CVCondRes, type == 'multiple'), aes(x = logCtFm, y = CVCondRes)) + geom_point()+ geom_smooth(method = "lm", formula =y ~  poly(x, 2, raw = TRUE), se = TRUE) +
		facet_wrap(~Instar, scales = "free_y") + xlab("Log Nest Size (num ad females)") + ylab("Condition Residuals Variance") + mytheme


#condition variance by instar
ggplot(CVCondRes, aes(x = Instar, y = CVCondRes)) + geom_boxplot() + mytheme # doesn't show anything anymore


## Difference between instars

CondInsrCols<- dcast(subset(CVCondRes, select = c(NestID, Instar, meanCondRes, CountFemales, logCtFm)), 
		NestID +  logCtFm + CountFemales ~ Instar, value.var= "meanCondRes",  drop = T) #transpose data

CondInsrCols$AdSub2Diff <- CondInsrCols$Adult- CondInsrCols$Sub2
CondInsrCols$Sub2Sub1Diff <- CondInsrCols$Sub2- CondInsrCols$Sub1
CondInsrCols$Sub1Juv4Diff <- CondInsrCols$Sub1- CondInsrCols$Juv4



ggplot(CondInsrCols, aes(AdSub2Diff)) + geom_histogram()

ggplot(data = subset(CondInsrCols, CountFemales > 5), aes(x = logCtFm, y = AdSub2Diff)) + geom_point() +
		stat_smooth(method="lm", se=TRUE, formula = y~ poly(x, 1)) + xlab("Log Nest Size") +
		ylab("Difference in mean condition (Adult - Sub2)") + mytheme

CondDiff <- melt(subset(CondInsrCols, select = c(NestID, CountFemales, logCtFm, AdSub2Diff, Sub2Sub1Diff, Sub1Juv4Diff)), 
				id.vars = c('NestID', 'CountFemales' , 'logCtFm'))
		
CondDiff <- na.omit(CondDiff)

CondDiff <- subset(CondDiff, CountFemales > 5)

ggplot(data = CondDiff, aes(x = logCtFm, y = value)) + geom_point() +
		stat_smooth(method="lm", se=TRUE, formula = y~ poly(x, 1)) + xlab("Log Nest Size") +
		ylab("Difference in mean condition") + facet_wrap(~variable) + mytheme

CondDiffLm <- lmer(value ~  logCtFm  + variable + logCtFm:variable + 
				(1|NestID), data = CondDiff, REML = FALSE)

summary(CondDiffLm)

visreg(CondDiffLm,  xvar = "logCtFm", by = "variable")
