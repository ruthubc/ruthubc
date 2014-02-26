library(plyr)
library(ggplot2)
require(reshape2)
library(nlme)
library(gridExtra)
library(histogram)

mytheme <-theme_bw(base_size=15)  + theme(plot.title = element_text(vjust=2), panel.margin= unit(0.75, "lines"), axis.title.y = element_text(vjust=0),
		plot.margin=unit(c(1,1,1.5,1.2),"cm"), panel.border = element_rect(fill = NA, colour = "grey", linetype=1, size = 1))


#### Box trails graphs. Code importing and manipulating the data is in BoxTrialsData.R
source("G:/PhDWork/EclipseWorkspace/R/EcuRCode/BoxTrials/BoxTrialsData.R")

################### Histograms ######################################################
##Looking at the total time eating in box. Need to remove maybe all records time < 1hour?
BoxSubset<-subset(BoxCombo, TimeOfDay == "morn")
BoxEating <- aggregate(BoxSubset, by = list(BoxSubset$TrialID), FUN = mean)
ggplot(BoxEating, aes(x= x)) + geom_histogram(binwidth = 15)

BoxWeight <- unique(subset(BoxCombo, select = c(Weight.1, Instar)))
ggplot(BoxWeight, aes(x=(Weight.1)) ) + geom_histogram() + facet_wrap(~Instar)

#########  Barplot Capture vs eating  ###################################################
#####################################################################################

CapVsEat <-subset(BoxCombo, select = c("FeedIndPos", "CaptureIndPos", "Treatment", "Instar") )
CapVsEat <-na.omit(CapVsEat)
CapVsEat$FeedIndPos <- factor(CapVsEat$FeedIndPos, levels =c("y", "n"))

pdf("RuthEcuador2013/BoxFeedingTrials/Graphs/CaptureVsFeed.pdf")

##separate bars
ggplot(data=CapVsEat, aes(x=CaptureIndPos, fill = FeedIndPos)) +
		geom_bar(stat="bin", position="fill", colour = "black") + xlab("Participated in Prey Capture") + ylab("") + 
		scale_x_discrete(breaks=c("y", "n"), labels=c("Yes", "No")) +
		theme(axis.text=element_text(colour="black"), axis.title = element_blank()) +
		scale_fill_discrete(name = "Fed?", breaks = c("y", "n"),
				labels = c("Yes", "No")) + ggtitle("Prey Capture Vs Feeding") + mytheme

##comparing the proportion of eaters and captures by TREATMENT
##separate bars
ggplot(data=CapVsEat, aes(x=CaptureIndPos, fill = FeedIndPos)) +
		geom_bar(stat="bin", position="fill", colour = "black") + xlab("Participated in Prey Capture") + ylab("") + 
		scale_x_discrete(breaks=c("y", "n"), labels=c("Yes", "No")) +
		theme(axis.text=element_text(colour="black"), axis.title = element_blank()) +
		scale_fill_discrete(name = "Fed?", breaks = c("y", "n"),
				labels = c("Yes", "No")) + ggtitle("Prey Capture Vs Feeding") + mytheme +
		facet_wrap(~Treatment)

ggplot(data=CapVsEat, aes(x=FeedIndPos, fill = CaptureIndPos)) +
		geom_bar(stat="bin", position="fill", colour = "black") + 
		scale_x_discrete(breaks=c("y", "n"), labels=c("Fed", "Did Not Feed")) +
		theme(axis.text=element_text(colour="black"), axis.title = element_blank()) +
		scale_fill_discrete(name = "Involved with\nprey capture?", breaks = c("n", "y"),
				labels = c("No", "Yes")) + ggtitle("Prey Capture Vs Feeding by prey size") +
		facet_wrap(~Instar)


dev.off()

#########################################################################################
########graph of number of individuals and total duration vs prey size and instar########
 
####removing evening feeds as no or little feeding observations
##Counting the number of individuals eating in each trial

pdf("RuthEcuador2013/BoxFeedingTrials/Graphs/NoAndDurationFeeding.pdf", onefile = "TRUE") 

 ##graph total number of individuals feeding vs prey size
 ggplot(AveByTrial, aes(x=Treatment, y=noFeed)) + geom_boxplot() + 
		 stat_summary(fun.y=mean, geom="point", shape=5, size=4) +
		 ggtitle("Log of Total number of spiders fed on prey against prey size") +
		 xlab("Prey Size") + ylab("Total number of spiders feeding on prey")  + 
		 scale_y_log10()

#Number feeding vs treatment 
ggplot(AveByTrial, aes(x=Treatment, y=noFeed)) + geom_boxplot() + 
		 stat_summary(fun.y=mean, geom="point", shape=5, size=4) +
		 ggtitle("Total number of spiders fed on prey against prey size") +
		 xlab("Prey Size") + ylab("Total number of spiders feeding on prey by instar") +
		 facet_wrap(~Instar)  + scale_y_log10() 
 
 ##### total box time eating vs prey
 ggplot(AveByTrial, aes(x=Treatment, y=feedDur)) + geom_boxplot() + 
		 stat_summary(fun.y=mean, geom="point", shape=5, size=4) +
		 ggtitle("Log of Total amount of time feeding on prey per box") + ylab("Total time feeding (mins)") +
		 xlab("Prey Size") + scale_y_log10()
 
#total box time eating vs treatment by instar
ggplot(AveByTrial, aes(x=Treatment, y=feedDur)) + geom_boxplot() + 
		 stat_summary(fun.y=mean, geom="point", shape=5, size=4) + facet_wrap(~Instar) + 
		 ggtitle("Log of Total amount of time feeding on prey per box by instar") + 
		 ylab("Total time feeding (mins)") + xlab("Prey Size") + scale_y_log10() 
 
 ################################  Feeding fraction  #####################################
 
 ##Feed Fraction histograms
 
 ggplot((subset(BoxComboMorn, FeedFraction > 0)), aes(ASFeedFrac)) + geom_histogram() #asin makes it more normal
 
 ####graph of individual feeding fractionvs prey size and instar (graph looks pretty much the same
 # with zeros included compared to no zeros included
 ggplot((subset(BoxComboMorn, FeedFraction > 0)), aes(x=Treatment, y=ASFeedFrac)) + geom_boxplot() + 
		 stat_summary(fun.y=mean, geom="point", shape=5, size=4) +
		 ggtitle("Fraction of time feeding by individual (no eaters removed)") + ylab("Fraction of time spent eating prey by each individual") +
		 xlab("Prey Size")
 
# feeding fraction by instar
ggplot((subset(BoxComboMorn, FeedFraction > 0)), aes(x=Treatment, y=ASFeedFrac)) + geom_boxplot() + 
		 stat_summary(fun.y=mean, geom="point", shape=5, size=4) +
		 ggtitle("Fraction of time feeding by individual (zero eaters removed)") + ylab("Fraction of time spent eating prey by each individual") +
		 xlab("Prey Size") + facet_wrap(~Instar)
 
 ggplot(subset(BoxComboMorn, FeedFraction > 0), aes(x= Hunger, y = ASFeedFrac, colour = Treatment)) + geom_point() +
		 geom_smooth(method = "lm", formula =y ~  poly(x, 1, raw = TRUE), se = TRUE) + 
		 ggtitle("Feeding Fraction against hunger level (head length / weight") + 
		 facet_wrap(Treatment~Instar, scales = "free_x")
 
dev.off()


##########################################################################################
##Feeding duration (rank?) vs weight rank

##need to combine small trials ......
####remove evening trials and moulted individuals


pdf("RuthEcuador2013/BoxFeedingTrials/Graphs/InitalWeights.pdf", onefile = "TRUE")

# weight rank vs rank eating duration by treatment
ggplot(BoxComboAve, aes(x = Rank.Weights, y = RankEatDur.Mean)) +
		geom_point() + geom_smooth(method = "lm", formula =y ~  poly(x, 1, raw = TRUE), se = TRUE) +
		ggtitle("Weight ranked within box vs time eating ranked within box ") + ylab("Rank of Time Eating") +
		xlab("Weight rank within box") + facet_wrap(~Treatment)

# leg length rank vs rank of eating duration 
ggplot(BoxComboAve, aes(x = Rank.Legs, y = RankEatDur.Mean, colour = Treatment)) + 
		geom_point() + geom_smooth(method = "lm", formula =y ~  poly(x, 1, raw = TRUE), se = TRUE) +
		ggtitle("Rank of leg eating vs rank of time eating") + ylab("Rank of time eating") + 
		xlab("Rank of leg length")

# hunger by sum of time eating
ggplot(BoxComboAve, aes(x= Hunger, y = SumIndEat, colour = Treatment)) + geom_point() +
		geom_smooth(method = "lm", formula =y ~  poly(x, 1, raw = TRUE), se = TRUE) + 
		ggtitle("Total Time Eating against hunger level (head length / weight") +
		facet_wrap(Treatment~Instar, scales = "free_x")

# hunger by sum of time eating by instar
ggplot(subset(BoxComboAve, SumIndEat>0), aes(x= Hunger, y = SumIndEat, colour = Treatment)) + geom_point() +
		geom_smooth(method = "lm", formula =y ~  poly(x, 2, raw = TRUE), se = TRUE) + 
		ggtitle("Total Time Eating against hunger level (head length / weight") + 
		facet_wrap(Treatment~Instar, scales = "free_x")

dev.off()

pdf("RuthEcuador2013/BoxFeedingTrials/Graphs/FeedingAndHunger.pdf", width= 10)

# hunger boxplot by ate or didn't
ggplot(subset(BoxComboMorn, IndFeed != "NA") , aes(x = IndFeed, y = LogHunger)) + geom_boxplot() + 
		facet_wrap(Treatment~Instar) + mytheme + ylab("Log Hunger") + xlab("")+
		scale_x_discrete(breaks=c("y", "n"), labels=c("Fed", "Did Not Feed"))

ggplot(subset(BoxComboMorn, IndFeed == "y"), aes(x= LogHunger, y = TimeEatingLog1, colour = Treatment)) + geom_point() +
		geom_smooth(method = "lm", formula =y ~  poly(x, 1, raw = TRUE), se = TRUE) + 
		ggtitle("Total Time Eating against hunger level- zeros removed") + 
		facet_wrap(Treatment~Instar, scales = "free_x")

#TODO: hunger rank

dev.off()


#####################################################################################
#### Behavior vs physiological things

# Poke rating vs inital weight by instar
ggplot(Weights, aes(x= AvePokeRating, y = Weight.1)) + geom_point() + 
		geom_smooth(method = "lm", formula =y ~  poly(x, 1, raw = TRUE), se = TRUE) + 
		facet_wrap(~Instar, scales = "free_y")

# Boldness vs inital weight by instar
ggplot(Weights, aes(x= AveBoldness, y = Weight.1)) + geom_point() + 
		facet_wrap(~Instar, scales = "free_y") + 
		geom_smooth(method = "lm", formula =y ~  poly(x, 1, raw = TRUE), se = TRUE)

# Boldness vs hunger by instar
ggplot(Weights, aes(x= AveBoldness, y = Hunger)) + geom_point() + 
		facet_wrap(~Instar, scales = "free_y") + 
		geom_smooth(method = "lm", formula =y ~  poly(x, 1, raw = TRUE), se = TRUE)

# Boldness vs leg length by instar
ggplot(Weights, aes(x= AveBoldness, y = LegLen.mm)) + geom_point() + 
		facet_wrap(~Instar) + 
		geom_smooth(method = "lm", formula =y ~  poly(x, 1, raw = TRUE), se = TRUE)


#######################################################################################
#######  Behaviours over time #########################

# Poke time1 vs time2 barchart although NA's are included ANNOYING!!
ggplot(data=Weights, aes(x=Poke.1, fill = as.factor(Poke.2))) +
		geom_bar(stat="bin", position="fill", colour = "black")

pdf("RuthEcuador2013/BoxFeedingTrials/Graphs/BehaviourOverTime.pdf")

# Poke1 vs Poke2
ggplot(data=Weights, aes(x= PokeRating.1, y = PokeRating.2)) + geom_jitter(position = position_jitter(w = 0.1, h = 0.1)) +
		geom_smooth(method = "lm", formula =y ~  poly(x, 2 , raw = TRUE), se = TRUE) +
		ggtitle("Poke rating of same spider at different times (jittered points)")

# Boldness1 vs Boldness2
ggplot(data = Weights, aes(x = BoldnessRank.1, y = BoldnessRank.2)) + geom_jitter(position = position_jitter(w = 0.1, h = 0.1)) +
		geom_smooth(method = "lm", formula =y ~  poly(x, 2 , raw = TRUE), se = TRUE) +
		ggtitle("Boldness rank of same spider at different times (jittered points)")

dev.off()

#Bar chart of cup drop1 vs cup drop2
ggplot(data= Weights, aes(x = as.factor(CupDrop.2), fill = as.factor(CupDrop.1))) + 
		geom_bar(stat="bin", position="fill", colour = "black")
# Bar chart of box drop1 vs box drop 2
ggplot(data= Weights, aes(x = as.factor(DropBox.2), fill = as.factor(BoxDrop.1))) + 
		geom_bar(stat="bin", position="fill", colour = "black")



#####################################################################################
##Behaviour vs feeding and capture



pdf("RuthEcuador2013/BoxFeedingTrials/Graphs/Behaviour.pdf")

# Capture vs boldness by instar
ggplot(BoxComboAve, aes(x= AveCap, y = AveBoldness)) + geom_jitter(position = position_jitter(w = 0.1, h = 0.1)) +
		geom_smooth(method = "lm", formula =y ~  poly(x, 1 , raw = TRUE), se = TRUE) +
		facet_wrap(~Instar) + ggtitle("Ave capture vs average boldness rating")

# Capture vs poke by instar
ggplot(BoxComboAve, aes(x= AveCap, y = AvePokeRating)) + geom_jitter(position = position_jitter(w = 0.1, h = 0.1)) +
		geom_smooth(method = "lm", formula =y ~  poly(x, 1 , raw = TRUE), se = TRUE)+
		facet_wrap(~Instar) + ggtitle("Ave capture vs average poke rating")

# Move at all vs capture
ggplot(subset(BoxComboAve, Cap != "NA"), aes(x=Move, fill = Cap)) +
		geom_bar(stat="bin", position="fill", colour = "black") + 
		ggtitle("Move at all during boldness test vs particitpated in capture") +
		scale_x_discrete(breaks=c("y", "n"), labels=c("Moved", "Did Not Move")) +
		theme(axis.text=element_text(colour="black"), axis.title = element_blank()) +
		scale_fill_discrete(name = "Involved with\nprey capture?", breaks = c("n", "y"),
				labels = c("No", "Yes")) + facet_wrap(Instar~Treatment)

#Move at all vs feed
ggplot(subset(BoxComboAve, Feed != "NA"), aes(x=Move, fill = Feed)) +
		geom_bar(stat="bin", position="fill", colour = "black")  +
		ggtitle("Move at all during boldness test with eat at all") +
		scale_x_discrete(breaks=c("y", "n"), labels=c("Moved", "Did Not Move")) +
		theme(axis.text=element_text(colour="black"), axis.title = element_blank()) +
		scale_fill_discrete(name = "Ate Food?", breaks = c("n", "y"),
				labels = c("No", "Yes")) + facet_wrap(Instar~Treatment)

##Boldness against instar

ggplot(subset(BoxComboAve), aes(x=Instar, fill = Move)) +  geom_bar(stat="bin", position="fill", colour = "black")


dev.off()


ggplot()


############################################################################
#Difference in weights.. need percentage change in weight?

# (1) Feeding time vs weight change

ggplot(BoxComboAve, aes(x=SumIndEat, y = WeightDiffPer)) + geom_point() +
		geom_smooth(method = "lm", formula =y ~  poly(x, 2 , raw = TRUE), se = TRUE)

ggplot(AveByTrial, aes(x = Treatment, y = SimpAlt )) + geom_boxplot() + facet_wrap(~Instar)

AveByTrSub<- subset(AveByTrial, Instar == "Sub1")

t.test(AveByTrSub$Simpsons ~ AveByTrSub$Treatment)


#####################################################################################
##### Pielou's J graphs

#histogram

ggplot(AveByTrial, aes(AsinPJEven)) + geom_histogram()

pdf("RuthEcuador2013/BoxFeedingTrials/Graphs/PJEven.pdf", width = 7, height =5.5)

SubsetAveByTrial<- subset(AveByTrial, PJEven > -1)

ggplot(SubsetAveByTrial, aes(x= Treatment, y =AsinPJEven)) + geom_boxplot() + mytheme + ylab("asin of box evenness") + xlab("Prey Size")

ggplot(SubsetAveByTrial, aes(x= Instar, y =AsinPJEven)) + geom_boxplot() + mytheme + ylab("asin of box evenness") + xlab("Prey Size")

ggplot(SubsetAveByTrial, aes(x= Treatment, y =AsinPJEven)) + geom_boxplot() + facet_wrap(~Instar) + mytheme + ylab("asin of box evenness") + xlab("Prey Size")

dev.off()

#################################################################################

##histogram of hunger
ggplot(BoxComboMorn, aes((Hunger))) + geom_histogram()  + facet_wrap(~Instar)

ggplot(BoxComboMorn, aes(x = Treatment, y=  Hunger)) + geom_boxplot() + facet_wrap(~Instar)
