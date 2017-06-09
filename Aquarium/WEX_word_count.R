# TODO: Add comment
# 
# Author: Ruth
###############################################################################

library("lme4")
library("lmerTest")
library("ggplot2")
library('dplyr')

mytheme2 <-theme_bw(base_size=20)  + theme(plot.title = element_text(vjust=2, size = 15), panel.margin= unit(0.75, "lines"),  
		plot.margin=unit(c(1,1,1.5,1.2),"cm"), panel.border = element_rect(fill = NA, colour = "grey", linetype=1, size = 1), 
		panel.grid.major = element_blank(), panel.grid.minor = element_blank())


#theme(plot.title = element_text(size = 40, face = "bold"))

### Word Count

word_count <- read.csv("Aquarium/WexData/WEX_word_count.csv")

word_count$Time <- factor(word_count$Time, levels = c("pre", "post"))



# Histogram
ggplot(aes(x = WordCount), data = word_count) +  geom_histogram()

WordCount_lmer <- lmer(WordCount ~ Time + Department + (1|WexNum), data = word_count)

anova(WordCount_lmer)

## Boxplot

ggplot(aes(x = Time, y = WordCount), data = word_count) + geom_boxplot()


## Barplot

wordSummary <- word_count %>% group_by(Time) %>% summarize(mean=mean(WordCount))

png("Aquarium/WexData/WEX_word_count.png")

ggplot(aes(x = Time, y = mean, fill = Time), data = wordSummary) + geom_bar(stat = "identity", colour = "black") +
		xlab("") + ylab("average word count per student") + mytheme2 + 
		annotate("text", x =0.5, y = 50, label = "Highly significant difference\nbetween number of words\nused (p = 0.003),\nbut there is no difference\nbetween programs", hjust = 0) + 
		ggtitle("The number of words used by the students\nbefore and after the program")

dev.off()

########################
## Categories

cats <- read.csv("Aquarium/WexData/WEX_cats.csv")
cats$Time <- factor(cats$Time, levels = c("pre", "post"))

# histogram
ggplot(aes(x = Cats), data = cats) +  geom_histogram()  # could log transform. right skewed

# boxplot
ggplot(aes(x = Time, y = Cats), data = cats) + geom_boxplot()

# barplot and summary

catSummary <- cats %>% group_by(Time) %>% summarize(mean=mean(Cats))



png("Aquarium/WexData/WEX_CategoriesCount.png")

ggplot(aes(x = Time, y = mean, fill = Time), data = catSummary) + geom_bar(stat = "identity", colour = "black") +
		xlab("") + ylab("average categories per student") + mytheme2 +
		annotate("text", x =0.5, y = 5, label = "There is an (almost) significant\ndifference between the number\nof topics the students\nmentioned before and\nafter the program\n(p = 0.06)", hjust = 0) + 
		ggtitle("The number of categories the students mentioned\nbefore and after the program") + ylim(0, 5.7)

dev.off()

## Stats Test

Cats_lmer <- lmer(Cats ~ Time + (1|Wex.ID), data = cats)

anova(Cats_lmer)




