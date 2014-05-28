# TODO: Add comment
# 
# Author: Ruth
###############################################################################


by(AveByTrial$noFeed, AveByTrial$Treatment, mean)
by(AveByTrial$noCap, AveByTrial$Treatment, mean)

BoxRatioStats = function(x) c(mean = mean(x), se = (sd(x)/sqrt(length(x))), n = length(x), max = max(x))
tapply(AveByTrial$noFeed, AveByTrial$Treatment, BoxRatioStats)
tapply(AveByTrial$noCap, AveByTrial$Treatment, BoxRatioStats)