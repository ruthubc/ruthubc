# TODO: Add comment
# 
# Author: Ruth
###############################################################################

library(ggplot2)


lab_marks <- read.csv("C:/Users/Ruth/Dropbox/RuthSync/Biol230_summer/LabMarks.csv")


png("C:/Users/Ruth/Dropbox/RuthSync/Biol230_summer/LabMarks.png")

ggplot(data = lab_marks, aes(Mark.100)) + geom_histogram(colour = "white")

dev.off()