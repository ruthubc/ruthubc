# Using Nathan's dispersal data
###############################################################################

library(dplyr)
library(survival)

props <- read.csv("PropaguleData/dispersal_propagule_data.csv")

props$dateFormatted <- as.Date(props$date, format = "%d/%m/%Y")

props$propID <- paste(props$nestID, props$Propagule, sep = "_")

props <- ddply(props, "propID", transform, dateFirstRec = min(dateFormatted)) # calculating max and min

head(props)

props$DiffDays <- difftime(props$dateFormatted, props$dateFirstRec, units = 'days')

props$DiffDays <- as.numeric(props$DiffDays)



msurv <- with(props, Surv(DiffDays, AliveOrDeadOrEnd =="Dead", type = "right"))
survivalMean <- as.data.frame(mean(msurv[,1])) # don't use mean without the [,1] by itself, wrong!

## Graphing

prop.survfit <- survfit(Surv(DiffDays, AliveOrDeadOrEnd =="Dead", type = "right") ~ 1, data = props)

plot(prop.survfit)
