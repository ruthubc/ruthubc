# TODO: Add comment
# 
# Author: Ruth
###############################################################################
library(wq)

layOut(list(p1, 1:3, 1),   # takes three rows and the first column
		list(p2, 1, 2),    # next three are on separate rows
		list(p3, 2,2), 
		list(p4, 3,2))


p1 = rectGrob()

grid.arrange(p1, arrangeGrob(p1,p1,p1, heights=c(1/4, 1/4, 3/4), ncol=1), ncol=2)

p_grob <- arrangeGrob(p1,p2, ncol=2)
grid.arrange(p1, p2, p3, p4, p_grob, heights = c(1/3, 1/3, 1/3, 2/3, 2/3), ncol = 1)



grid.arrange( arrangeGrob(p2, p3, arrangeGrob(p14,p15, ncol=2), p16, p17, ncol= 1, heights = c(3/4, 1/4, 1/4)),  ncol = 1)