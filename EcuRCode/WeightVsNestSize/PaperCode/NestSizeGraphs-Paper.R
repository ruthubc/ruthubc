# TODO: Add comment
# 
# Author: user
###############################################################################

library(ggplot2)
library(grid)


mytheme <-theme_bw(base_size=15)  + theme(plot.title = element_text(vjust=2), panel.margin= unit(0.75, "lines"), axis.title.y = element_text(vjust=0),
		plot.margin=unit(c(1,1,1.5,1.2),"cm"), panel.border = element_rect(fill = NA, colour = "grey", linetype=1, size = 1), panel.grid.major = element_blank(), panel.grid.minor = element_blank())


pdf("RuthEcuador2013/NestSize/Graphs/CondLegVsNestSize.pdf", height=8, width=13)

legAve<- ddply(spidersMul, .(NestID, Instar, logCtFm, CountFemales), summarise,
		N = length(!is.na(logLeg)),
		meanLogLeg = mean(logLeg, na.rm = TRUE)
)

### Overall graph of leg length vs nest size
ggplot(spidersMul, aes(x = logCtFm, y = logLeg)) + geom_point()+ geom_smooth(method = "lm", formula =y ~  poly(x, 1, raw = TRUE), se = TRUE) +
		facet_wrap(~Instar, scales = "free_y") + xlab("Log Nest Size (num ad females)") + ylab("Log Leg Length") + mytheme


### graph with just the means for each nest
ggplot(legAve, aes(x = CountFemales, y = meanLogLeg)) + geom_point()+ scale_x_log10() + geom_smooth(method = "lm", formula =y ~  poly(x, 1, raw = TRUE), se = FALSE) +
		facet_wrap(~Instar, scales = "free_y") + xlab("Nest Size (num ad females)") + ylab("Log Leg Length") + mytheme



dev.off()

## leg condition
ggplot(spidersMul, aes(x = logCtFm, y = logcond)) + geom_point()+ geom_smooth(method = "lm", formula =y ~  poly(x, 1, raw = TRUE), se = TRUE) +
		facet_wrap(~Instar, scales = "free_y") + xlab("Log Nest Size (num ad females)") + ylab("Log Condition") + mytheme

pdf("RuthEcuador2013/NestSize/Graphs/VarianceVsNestSize.pdf", height=8, width=13)

### Overall graph of leg length vs nest size
ggplot(SpiNestCV, aes(x = logCtFm, y = CVCond^0.5)) + geom_point()+ geom_smooth(method = "lm", formula =y ~  poly(x, 1, raw = TRUE), se = TRUE) +
		facet_wrap(~Instar, scales = "free_y") + xlab("Log Nest Size (num ad females)") + ylab("CV of Condition") + mytheme


ggplot(SpiNestCV, aes(x = logCtFm, y = CVLeg^0.5)) + geom_point()+ geom_smooth(method = "lm", formula =y ~  poly(x, 1, raw = TRUE), se = TRUE) +
		facet_wrap(~Instar, scales = "free_y") + xlab("Log Nest Size (num ad females)") + ylab("CV of Leg") + mytheme


dev.off()