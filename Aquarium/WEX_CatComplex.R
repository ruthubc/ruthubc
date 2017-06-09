# TODO: Add comment
# 
# Author: Ruth
###############################################################################

library("lme4")
library("lmerTest")
library("ggplot2")
library('plyr')

mytheme2 <-theme_bw(base_size=20)  + theme(plot.title = element_text(vjust=2, size = 15), panel.margin= unit(0.75, "lines"),  
		plot.margin=unit(c(1,1,1.5,1.2),"cm"), panel.border = element_rect(fill = NA, colour = "grey", linetype=1, size = 1), 
		panel.grid.major = element_blank(), panel.grid.minor = element_blank())



intp <- read.csv("Aquarium/WexData/CatInterpCSV.csv")

intp$When<- ifelse(intp$time == 1, "before", "after")


intp$When <- as.factor(intp$When, levels = c("before", "after"))

### Graph comparing the count of categories before and after

Score_lmer <- lmer(score ~ time + (1|WEX_ID), data = intp)

anova(Score_lmer)


commSumByCode <- ddply(intp, c("Code", "When"), summarise,
		count = length(WEX_ID))

commSumByCode <- rbind(commSumByCode, c("JellyFAndTurtles", "before", 0))

commSumByCode$count <- as.numeric(commSumByCode$count)

commSumByCode$When <- as.factor(commSumByCode$When)#, 

commSumByCode$When <- ordered(commSumByCode$When, levels = c("before", "after"))


commSumByCode <- commSumByCode[order(-commSumByCode$count),] 


png("Aquarium/WexData/CountCatB4AndAfter.png", width = 700, height = 480)


ggplot(aes(x = reorder(Code, count), y = count, fill = When), data = commSumByCode) + 
		geom_bar(stat = "identity", position = position_dodge(width = NULL), colour = "black", width = 0.6) +
		xlab("") + ylab("count") + mytheme2 + theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
		ggtitle("Number of comments by category.\nYou can see which topics are most impacted during the programs.")

dev.off()


## Graph comparing category score before and after

ggplot(aes(x = as.factor(time), y = score), data = intp) + geom_bar(stat = "summary", fun.y = "mean") +
		xlab("time") + ylab("score") + mytheme2


intp$When <- ordered(intp$When, levels = c("before", "after"))


png("Aquarium/WexData/CommCountByScoreB4AndAfter.png", width = 700, height = 480)

ggplot(aes(x = score, fill = When), data = intp) + geom_bar(stat = "count", position = "dodge", colour = "black") +
		xlab("Comment complexity Score") + ylab("count") + mytheme2 + 		
		annotate("text", x =2.6, y = 35, label = "There is a highly significant difference between\ncomplexity scores of each student's comments\nbefore and after (p = 0.0001)\n\n
The students made the same amount of low\ncomplexity comments before and after,\nbut many more high complexity\ncomments after the program", hjust = 0) + 
		ggtitle("The number of comments each student seperated\nby the complexity score of that comment.")



dev.off()

