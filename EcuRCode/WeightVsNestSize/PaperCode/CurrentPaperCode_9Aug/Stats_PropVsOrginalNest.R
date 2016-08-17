# TODO: Add comment
# 
# Author: Ruth
###############################################################################


nestsSigOnly<- subset(spiders, type != "multiple" & Instar == "Adult")

OrgNests <- subset(spidersMul, Instar == "Adult")
OrgNests <- subset(OrgNests, NestID == "44.4EX03" | NestID == "28.8EX15")
OrgNests$OrigNest <- OrgNests$NestID



nestsSigOnly$OrigNest <- ifelse(grepl("^4",as.character(nestsSigOnly$NestID)) == TRUE, "44.4EX03", "28.8EX15")


nestsToTestSig <- rbind(nestsSigOnly, OrgNests)

nestsSigOnlyCond<- subset(nestsSigOnly, nestsSigOnly$FemalesHaveEggsOrJuvs == "y")

nestsToTestSigCond <- rbind(nestsSigOnlyCond, OrgNests)

## Testing leg length
FullModel <- lmer(logLeg ~  type  +  (1|NestID) + (1|OrigNest) , data = nestsToTestSig, REML = FALSE)

anova(FullModel)

RedModel <- lmer(logLeg ~  (1|NestID) + (1|OrigNest) , data = nestsToTestSig, REML = FALSE)

anova(FullModel, RedModel)


## Testing condition
FullModel <- lmer(condResiduals ~  type  +  (1|NestID) + (1|OrigNest) , data = nestsToTestSig, REML = FALSE)

anova(FullModel)

RedModel <- lmer(condResiduals ~  (1|NestID) + (1|OrigNest) , data = nestsToTestSig, REML = FALSE)

anova(FullModel, RedModel)



# Only remove nests with ads or juvs for condition calculations

mytheme <-theme_bw(base_size=15)  + theme(plot.title = element_text(vjust=2), panel.margin= unit(0.75, "lines"), axis.title.y = element_text(vjust=0),
		plot.margin=unit(c(1,1,1.5,1.2),"cm"), panel.border = element_rect(fill = NA, colour = "grey", linetype=1, size = 1), panel.grid.major = element_blank(), panel.grid.minor = element_blank())



p1 <- ggplot(nestsToTestSigCond, aes(x=type, y=condResiduals)) + geom_boxplot() + 
		 ylab("Individual Condition") + mytheme+ theme(axis.title.x = element_blank()) +
		scale_x_discrete(breaks=c("multiple", "single"), labels=c("source: multiple", "propagules: single"))

p2 <- ggplot(nestsToTestSig, aes(x=type, y=logLeg)) + geom_boxplot() +
		ggtitle("Leg Length") + ylab("Log Leg Length") +
		mytheme +  theme(axis.title.x = element_blank()) +
		scale_x_discrete(breaks=c("multiple", "single"), labels=c("source: multiple", "propagules: single"))

grid.arrange(p1, p2, ncol = 2)

