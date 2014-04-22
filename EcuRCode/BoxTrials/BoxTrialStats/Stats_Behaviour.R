# TODO: Add comment
# 
# Author: Ruth
###############################################################################


#### Is personality consistant over time? Intra class correlation

#Use weights table!

Boldness<-na.omit(subset(Weights, select = c("SpiderID", "BoldnessRank.1", "BoldnessRank.2")))
Boldness$BoldnessRank.1 <- as.numeric(Boldness$BoldnessRank.1)
Boldness$BoldnessRank.2 <- as.numeric(Boldness$BoldnessRank.2)


MeltBold<-melt(Boldness, id = c("SpiderID"))
MeltBold$SpiderID <- as.character(MeltBold$SpiderID)

BoldICC<- ICCest(SpiderID, value, data= MeltBold, alpha = 0.05)

##poke

Poke<-na.omit(subset(Weights, select = c("SpiderID", "Poke.1", "Poke.2")))
Poke$Poke.1 <- as.numeric(Poke$Poke.1)
Poke$Poke.2 <- as.numeric(Poke$Poke.2)


MeltPoke<-melt(Poke, id = c("SpiderID"))
MeltPoke$SpiderID <- as.character(MeltPoke$SpiderID)


PokeICC <- ICCest(SpiderID, value, data= MeltPoke, alpha = 0.05)

##Testing ICC

ID <- c("ID1", "ID2", "ID3", "ID4", "ID5", "ID1", "ID2", "ID3", "ID4", "ID5")
Mes1 <- c(1, 2, 3, 4, 5, 5, 4, 3, 2, 1)

ICCTest <- data.frame(ID, Mes1)

ICCest(ID, Mes1, data= ICCTest, alpha = 0.05)

###Trying to make a graph of this

Behv <- c("Bold", "Poke")
ICC <- c(BoldICC$ICC, PokeICC$ICC)
UpCI <- c(BoldICC$UpperCI, PokeICC$UpperCI)
LowCI <- c(BoldICC$LowerCI, PokeICC$LowerCI)

ICCdatafrm <- data.frame(Behv, ICC, UpCI, LowCI)


pdf("RuthEcuador2013/BoxFeedingTrials/Graphs/BehvICC.pdf") 

ggplot(ICCdatafrm, aes(x = Behv, y = ICC)) + geom_point(size = 3) + 
		geom_errorbar(aes(ymin = LowCI, ymax = UpCI), ICCdatafrm, width = 0.20) + ylim(-0.2, 1) +
		geom_hline(yintercept= c(0, 1), linetype = "dotted") + 
		annotate("text", size = 4,  x=0.95, y = -0.02, label = "no correlation between measurements" ) +
		annotate("text", size = 4,  x=0.95, y = 0.98, label = "perfect correlation between measurements")

dev.off()

##### Boldness and behaviour #######################################

## move at all during boldness vs prey capture
## taking out interaction as noeffect
BldMvMod1 <- glmer(Feed ~ Move + Instar + (1|Instar:IndBoxID) + (1|Instar:IndBoxID:SpiderID), 
		BoxComboAve, family = binomial(logit))

qqnorm(resid(BldMvMod1, main = "BldMvMod1")) ; abline(0, 1) # OK ish
overdisp_fun(BldMvMod1) # good
summary(BldMvMod1)

##testing against reduced model

BldMvRedModMove <- glmer(Feed ~ Instar + (1|Instar:IndBoxID) + (1|Instar:IndBoxID:SpiderID), 
		BoxComboAve, family = binomial(logit))

qqnorm(resid(BldMvRedModMove, main = "BldMvRedModMove")) ; abline(0, 1) # OK ish
overdisp_fun(BldMvRedModMove) # good
summary(BldMvRedModMove)

anova(BldMvMod1, BldMvRedModMove)

### Move at all vs capture

BldMvCap1 <- glmer(Cap ~ Move + Instar + (1|Instar:IndBoxID) + (1|Instar:IndBoxID:SpiderID), 
		BoxComboAve, family = binomial(logit))

qqnorm(resid(BldMvCap1 , main = "BldMvCap1 ")) ; abline(0, 1) # OK ish
overdisp_fun(BldMvCap1 ) # good
summary(BldMvCap1 )

## nothing... no point continuing

