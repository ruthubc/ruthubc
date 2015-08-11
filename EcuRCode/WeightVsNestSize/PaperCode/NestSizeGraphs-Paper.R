# TODO: Add comment
# 
# Author: user
###############################################################################


### Overall graph of leg length

ggplot(SpiNestAveMul, aes(x=  logCtFm, y = meanLeg.Scal )) + geom_point()+ geom_smooth(method = "lm", formula =y ~  poly(x, 1, raw = TRUE), se = TRUE)
