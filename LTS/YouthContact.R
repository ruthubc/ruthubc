# TODO: Add comment
# 
# Author: Ruth
###############################################################################



setwd("~")
getwd()

library(ggplot2)

Acts <- read.csv("Dropbox/LTS/ActivityExport.csv", stringsAsFactors = FALSE)

ggplot(Acts, aes(x = ActivityReport.NumberOfChildren)) + geom_histogram() + scale_x_log10()

# Brent Vs Andrew

ActBntAnd <- subset(Acts, Acts$ActivityReport.ModifiedByUser == "Andrew Santos" | Acts$ActivityReport.ModifiedByUser == "Brent Gali")

xtabs(~ActBntAnd$ActivityReport.ModifiedByUser)

ggplot(data = ActBntAnd, aes(x = ActivityReport.ModifiedByUser, y = log(ActivityReport.NumberOfChildren) )) + geom_boxplot()

Acts$orderedMonth <- factor(Acts$month, levels = c("September",
				"October", "November", "December", "January", "February", "March", "April", "May", "June", "July", "August"))

Acts$log10Kids <- log10(Acts$ActivityReport.NumberOfChildren)

#children by month
ggplot(data = Acts, aes(x = orderedMonth, y =  ActivityReport.NumberOfChildren, fill = month)) +
		geom_bar(stat = "sum") + facet_wrap(~year, ncol = 1) + theme_bw()


ggplot(data = Acts, aes(x = Program, y =  ActivityReport.NumberOfChildren, fill = Program)) +
		geom_bar(stat = "sum") + facet_wrap(~year, ncol = 1) + theme_bw()