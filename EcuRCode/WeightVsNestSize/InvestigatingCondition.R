# TODO: Add comment
# 
# Author: user
###############################################################################


spiders$condition <- spiders$Weight.mg/spiders$HeadLength.mm  # my original choice

spiders$condition <- spiders$Weight.mg/spiders$LegLen.mm

spiders$condition <- spiders$logWt/spiders$LegLen.mm # less of a correlation between weight and condition

spiders$condition <- (spiders$Weight.mg)/(spiders$LegLen.mm^3) # very high correlation between weight and condition

spiders$condition <- spiders$Weight.mg/(spiders$LegLen.mm *10)


# spiders$condition <- spiders$logWt/spiders$logLeg .. doesn't look very pretty
# histogram to check the normality
ggplot(spiders, aes(condition)) + geom_histogram() + facet_wrap(~Instar)

spiders$cond.sqrt <- sqrt(spiders$condition)

spiders$cond.log <- log10(spiders$condition)

ggplot(spiders, aes(cond.sqrt)) + geom_histogram() + facet_wrap(~Instar)





ggplot(spiders, aes(x = LegLen.mm, y = Weight.mg, colour = Instar )) + geom_point() + geom_smooth()

ggplot(spiders, aes(x = LegLen.mm, y = condition)) + geom_point() + geom_smooth() + facet_wrap(~Instar, scales = "free")

ggplot(spiders, aes(x = Weight.mg, y = condition)) + geom_point() + geom_smooth() + facet_wrap(~Instar, scales = "free")




# looking at what the graphs look like with the different measures of condition
ggplot(spiders, aes(x = logCtFm, y = logWt)) + geom_smooth(method = "lm", formula =y ~  poly(x, 2, raw = TRUE)) + facet_wrap(~Instar, scales = "free")

ggplot(spiders, aes(x = logCtFm, y = cond.sqrt)) + geom_smooth(method = "lm", formula =y ~  poly(x, 2, raw = TRUE)) + facet_wrap(~Instar, scales = "free")



# directly comparing condition and weight

spiders$cond.sqrt <- sqrt(spiders$condition)

spidersSub <- subset(spiders, select = c(logCtFm, logWt, Instar))

spidersSub$type <- "weight"

colnames(spidersSub) <- c("logCtFm", "meas",  "Instar",  "type")  

spidersSub1 <- subset(spiders, select = c(logCtFm, cond.sqrt, Instar))

spidersSub1$type <- "condition"

colnames(spidersSub1) <- c("logCtFm", "meas",  "Instar",  "type")  

spidersRbind <- rbind(spidersSub1, spidersSub)



ggplot(spidersRbind, aes(x = logCtFm, y = meas, colour = type)) + geom_smooth(method = "lm", formula =y ~  poly(x, 2, raw = TRUE)) + facet_wrap(~Instar, scales = "free")