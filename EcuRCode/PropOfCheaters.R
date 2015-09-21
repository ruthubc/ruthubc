# TODO: Add comment
# 
# Author: user
###############################################################################

library(MASS)

CheatAve<- ddply(BoxComboMorn, .(SpiderID, LogCond, Instar, IndBoxID, Treatment, CaptureIndPos, FeedIndPos ), summarise, # need to discount trials where no feeding obs and eve
		N = length(!is.na(SpiderID)),
		
)



cheat <- subset(BoxComboMorn,  !is.na(CaptureIndPos) & FeedIndPos == "y",  select = c(SpiderID, LogCond, Instar, IndBoxID, Treatment, CaptureIndPos, FeedIndPos) )

cheat$paste <- as.factor(paste(cheat$FeedIndPos, cheat$CaptureIndPos))

# remove the ny

cheat <- subset(cheat, paste != "n y")


tbl <- table(cheat$FeedIndPos, cheat$CaptureIndPos, cheat$Treatment)

print(tbl)

chisq.test(tbl)

fisher.test(tbl)

test <- glmer(paste ~ Treatment + Instar +  (1|IndBoxID), cheat, family = binomial(logit) )

summary(test)


cheatAve<- ddply(cheat, .(IndBoxID, Treatment), summarise, # need to discount trials where no feeding obs and eve
		N = length(!is.na(SpiderID)),
		NCap = length(SpiderID[which(CaptureIndPos == "y")]),
		NFeed = length(SpiderID[which(FeedIndPos == "y")])

)

cheatAve$Prop <- cheatAve$NCap/cheatAve$NFeed


ggplot(data = cheatAve, aes(x = Treatment, y = log(Prop + 1))) + geom_boxplot()


test2 <- glmer(Prop ~ Treatment +  (1|IndBoxID), cheatAve, family = binomial(logit) )

summary(test2)

