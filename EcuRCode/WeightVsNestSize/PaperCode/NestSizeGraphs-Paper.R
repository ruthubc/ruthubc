# TODO: Add comment
# 
# Author: user
###############################################################################

library(ggplot2)
library(grid)


mytheme <-theme_bw(base_size=15)  + theme(plot.title = element_text(vjust=2), panel.margin= unit(0.75, "lines"), axis.title.y = element_text(vjust=0),
		plot.margin=unit(c(1,1,1.5,1.2),"cm"), panel.border = element_rect(fill = NA, colour = "grey", linetype=1, size = 1), panel.grid.major = element_blank(), panel.grid.minor = element_blank())



### Overall graph of leg length vs nest size
ggplot(spidersMul, aes(x = logCtFm, y = logLeg)) + geom_point()+ geom_smooth(method = "lm", formula =y ~  poly(x, 1, raw = TRUE), se = TRUE) +
		facet_wrap(~Instar, scales = "free_y") + mytheme


