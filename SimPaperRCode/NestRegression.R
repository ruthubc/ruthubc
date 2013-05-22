# TODO: Add comment
# 
# NestRegression
###############################################################################

NestSizes<-read.csv("EcuadorRuth/NestSizeRegressionData.csv", na.strings = "NA")

plot(NestSizes$Area.cm2, NestSizes$NoSpiders)
