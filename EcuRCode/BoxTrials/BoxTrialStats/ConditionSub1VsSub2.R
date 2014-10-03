# TODO: Add comment
# 
# Author: user
###############################################################################

# testing to see whether there is a difference in the coefficient of variation between instar conditions

table(Weights$SpiderID)

InstarConds<- ddply(Weights, .(Instar), summarise, # need to discount trials where no feeding obs and eve
		N = length(!is.na(SpiderID)),
		Cond.Mean = mean(LogCond, na.rm = TRUE),
		Cond.SD = sd(LogCond, na.rm = TRUE),
		Cond.CoV = Cond.SD/Cond.Mean
)

# Perhpas I want to graph this to check it is normal? Using the log value it is much better

ggplot(subset(Weights, Instar == "Sub1"), aes(x=LogCond)) + geom_histogram()
ggplot(subset(Weights, Instar == "Sub2"), aes(x=LogCond)) + geom_histogram()

sub1 <- subset(Weights, Instar == "Sub1")$LogCond
sub2 <- subset(Weights, Instar == "Sub2")$LogCond

var.test(sub1, sub2)

