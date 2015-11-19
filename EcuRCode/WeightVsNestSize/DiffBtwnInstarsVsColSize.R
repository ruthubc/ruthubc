# TODO: Add comment
# 
# Author: user
###############################################################################
require(reshape2)

#GRAPH TO EXPORT ---- Diff in weight between different instars
########################################################################################





DifSmWt <- ddply(spidersMul, .(NestID, logCtFm, Instar), summarise,
		N = length(!is.na(Weight.mg)),
		mean = mean(logcond, na.rm = TRUE)
)

DifSmWt <- subset(DifSmWt, N>3)

DifSmWt$NestID <- factor(DifSmWt$NestID)

WtInsrCols<- dcast(subset(DifSmWt, select = c(NestID, Instar, mean, logCtFm)), 
		NestID +  logCtFm ~ Instar, value.var= "mean",  drop = T) #transpose data

WtDiffs <- ddply(WtInsrCols, .(NestID, logCtFm), summarise,
		AdultVsSub2 = Adult/Sub2
)

ggplot(WtDiffs, aes(AdultVsSub2)) + geom_histogram()

#unstacks the data
SpiderWtDiffs <- melt(WtDiffs, id.vars=c("NestID","logCtFm"))#dcast(SpiderDiffs, NestID + logCtFm + Instar)

pdf("RuthEcuador2013/NestSize/Graphs/CondDiffBtwnInstarVsNestSize.pdf",  height=6.5, width=9)

ggplot(data = WtDiffs, aes(x = logCtFm, y = AdultVsSub2)) + geom_point() +
		stat_smooth(method="lm", se=TRUE, formula = y~ poly(x, 1)) + xlab("Log Nest Size") +
		ylab("Difference in mean condition (Adult - Sub2)") + mytheme + xlim(1.8,3.8) #+
#ggtitle("Difference in Weight Between Instars") 
dev.off()

CondDiffLm <- lm(AdultVsSub2 ~ logCtFm, data = WtDiffs)

summary(CondDiffLm)

