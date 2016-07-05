# Using Nathan's dispersal data
###############################################################################

library(plyr)
library(survival)
library(muhaz)
library(ggplot2)
library(survminer)

props <- read.csv("PropaguleData/dispersal_propagule_data.csv")

props$dateFormatted <- as.Date(props$date, format = "%d/%m/%Y")

props$propID <- as.factor(paste(props$nestID, props$Propagule, sep = "_"))
nlevels(props$propID)


props <- ddply(props, "propID", transform, dateFirstRec = min(dateFormatted)) # calculating max and min

head(props)

props$DiffDays <- difftime(props$dateFormatted, props$dateFirstRec, units = 'days')

props$DiffDays <- as.numeric(props$DiffDays)

ggplot(props, aes(x = DiffDays)) + geom_histogram(bins = max(props$DiffDays)) + scale_x_continuous(breaks= seq(0,60,by=7)) # changes axis  tick intervals


props$Dead_bin<- ifelse(props$AliveOrDeadOrEnd == "Dead", 1, 0)




msurv <- with(props, Surv(DiffDays, Dead_bin ==1, type = "right"))
survivalMean <- as.data.frame(mean(msurv[,1])) # don't use mean without the [,1] by itself, wrong!

## Graphing

prop.survfit <- survfit(Surv(DiffDays, AliveOrDeadOrEnd =="Dead", type = "right") ~ 1, data = props)

summary(prop.survfit)
print(prop.survfit)#, show.rmean=T)




pdf("PropaguleData/SurvivalCurve_Propagules.pdf", width = 8, height = 5)

# this is the best graph
## Info here http://www.sthda.com/english/wiki/survminer-r-package-survival-data-analysis-and-visualization

ggsurvplot(prop.survfit,color = "#2E9FDF", break.time.by = 5, xlab = "Days until propagule death", ylab = "Propagule survival probability",  censor = FALSE)

dev.off()

#### Experimenting with code ######

#propsHaz <- kphaz.fit(props$DiffDays, props$Dead_bin)
#kphaz.plot(propsHaz)
propsHaz <- muhaz(props$DiffDays, props$Dead_bin)
plot(propsHaz)
summary(propsHaz)

library(ggplot2)
library(survMisc)

autoplot(prop.survfit, type = "CI", pVal = TRUE)

library(GGally)

ggsurv(prop.survfit, plot.cens = FALSE)

# this is the best graph
## Info here http://www.sthda.com/english/wiki/survminer-r-package-survival-data-analysis-and-visualization
library(survminer)
ggsurvplot(prop.survfit,color = "#2E9FDF", break.time.by = 5, xlab = "Days")