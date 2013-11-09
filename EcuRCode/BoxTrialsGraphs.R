# TODO: Add comment
# 
# Author: Ruth
###############################################################################

library(data.table)
library(ggplot2)

#BoxData <- read.csv("RuthEcuador2013/BoxFeedingTrials/CombinedBoxData.csv")

Trials <- read.csv("RuthEcuador2013/BoxFeedingTrials/Trials.csv")

Feeding <- read.csv("RuthEcuador2013/BoxFeedingTrials/Feeding.csv")

#updates new field whether individual fed or not
Feeding$IndFeed <- ifelse (Feeding$TotalTimeEating > 0, "y", "n")

Feeding$IndFeed <- as.factor(Feeding$IndFeed)



Weights <- read.csv("RuthEcuador2013/BoxFeedingTrials/Weights.csv")

levels(BoxData$Nest)




BoxData$WeightDiff <- BoxData$Weight.2 - BoxData$Weight.1

ggplot(BoxData, aes(x=TotalTimeFeeding, y = IndCapture)) + geom_point(shape= 16)

ggplot(SumsLegN , aes(x=lnArea, y = cvByN)) + geom_point(shape = 16) + 
		geom_smooth(method = "lm", formula =y ~  poly(x, 2, raw = TRUE), se = TRUE) + 
		ggtitle(paste("Coefficient of variation of leg length when nest min of ", MinNoSpis, " spiders", sep = ""))+
		xlab("Log Approx. Nest Area") + ylab("CV of Len Length") + facet_wrap(~ Instar, scales = "free_y")



boxplot(BoxData$IndCapture)

##test for data table merging one to many works!

x <- data.table(one=c(1, 2, 1, 2), two=c('a','a','b', 'b'), three = c("1a", "2a", "1b", "2b" ))

y <- data.table(id = 1:6, one=c(1, 1, 1, 2, 2, 1), two=c('b','a','a', 'a', "b", "b"))

setkeyv(x, c("one", "two")) # two setkeys!

setkeyv(y, c("one", "two"))

merge(x, y)

##combining Trials and feeding
Feeding <- data.table(Feeding)
Trials <- data.table(Trials)
Weights <- data.table(Weights)

setkey(Trials, TrialID)

setkey(Feeding, TrialID)

TrialsFeeding <- merge(Trials, Feeding)

##Updating TrailsFeeding to take account of whether or not

#making lookup table for capture
Capture <- data.table (IndCapture = c("y", "n", "n"), BoxCapture = c("y", "y", "n"), 
		 CaptureIndPos = c("y", "n", NA))


setkeyv(TrialsFeeding, c("IndCapture", "BoxCapture"))

setkeyv(Capture, c("IndCapture", "BoxCapture"))

TrialsFeeding<-merge(TrialsFeeding, Capture)


###Lookup table for feeding
Feed <- data.table (IndFeed = c("y", "n", "n"), BoxFeedObs = c("y", "y", "n"), 
		FeedIndPos = c("y", "n", NA))

setkeyv(TrialsFeeding, c("IndFeed", "BoxFeedObs"))

setkeyv(Feed, c("IndFeed", "BoxFeedObs"))

TrialsFeeding<-merge(TrialsFeeding, Feed)


CapVsEat <- data.frame(TrialsFeeding$FeedIndPos, TrialsFeeding$CaptureIndPos)

CapVsEat <-subset( TrialsFeeding, select = c("FeedIndPos", "CaptureIndPos") )

CapVsEat <-na.omit(CapVsEat)

test <- table(CapVsEat)
## Graph for capturing vs eating

##start of with two binary measures?
#Need to combine trials with feeding

# grouped bar plot http://www.cookbook-r.com/Graphs/Bar_and_line_graphs_(ggplot2)/

df <- data.frame(time = factor(c("Lunch","Dinner"), levels=c("Lunch","Dinner")),
		total_bill = c(14.89, 17.23))

Test <- melt(CapVsEat, id.vars=c("activity"), measure.vars=c("yes","no","dontknow"),
		variable.name="haveused", value.name="responses")

pdf("RuthEcuador2013/BoxFeedingTrials/Graphs/CaptureVsFeed.pdf")

##separate bars
ggplot(data=CapVsEat, aes(x=FeedIndPos, fill = CaptureIndPos)) +
		geom_bar(stat="bin", position="fill", colour = "black") + 
		scale_x_discrete(breaks=c("y", "n"), labels=c("Fed", "Did Not Feed")) +
		theme(axis.text=element_text(colour="black"), axis.title = element_blank()) +
		scale_fill_discrete(name = "Involved with\nprey capture?", breaks = c("n", "y"),
				labels = c("No", "Yes")) + ggtitle("Prey Capture Vs Feeding")



##100% stacked bar graph
ggplot(data=CapVsEat, aes(x=FeedIndPos, fill = CaptureIndPos)) + 
		geom_bar(stat="bin", position='fill')

##chi squared test
chisq.test(table(CapVsEat))



##comparing the proportion of eaters and captures between prey type

CapVsEatSize <-subset( TrialsFeeding, select = c("FeedIndPos", "CaptureIndPos", "Treatment") )

CapVsEatSize <-na.omit(CapVsEat)

##separate bars
ggplot(data=CapVsEat, aes(x=FeedIndPos, fill = CaptureIndPos)) +
		geom_bar(stat="bin", position="fill", colour = "black") + 
		scale_x_discrete(breaks=c("y", "n"), labels=c("Fed", "Did Not Feed")) +
		theme(axis.text=element_text(colour="black"), axis.title = element_blank()) +
		scale_fill_discrete(name = "Involved with\nprey capture?", breaks = c("n", "y"),
				labels = c("No", "Yes")) + ggtitle("Prey Capture Vs Feeding by prey size") +
		facet_wrap(~Treatment)

#larger prey requires more individuals to capture it

dev.off()
