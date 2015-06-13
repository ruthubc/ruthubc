# TODO: Add comment
# 
# Author: user
###############################################################################


gam_nnplus1 <- gam(NPlus1~s(N), data = nnplus1)
plot <- plot.gam(gam_nnplus1)


#####################################
## try nls http://robinlovelace.net/2013/10/23/nls-demonstation.html

logistic <- nls(NPlus1 ~ I((N^(1+a)) * exp(b) * (exp(-c * N))) , data = nnplus1, start = list(a=0.4, b=1.5, c=0.02), 
		algorithm= "port", trace = T)

# include to set lower limits for variable calculated lower = list(a=0, b=0, c=0)

# ricker equation, seems to work best! 	
m <- nls(NPlus1 ~ I((N^(1+a)) * b * (1-(N/K))) , data = nnplus1, start = list(a=0.4, b=1.5, K=100), 
		algorithm= "port", trace = T)

tidy(m)

plot(m)

plot(nnplus1$N, nnplus1$NPlus1)


points(nnplus1$N,predict(m), pch = 17)


summary(m)

predictm <- predict(m)


rm(nnplus1)
rm(col_subset)

## old gam attempts that didn't work

model <-gam_nnplus1$model
gamDF <- data.frame(gam_nnplus1$y, gam_nnplus1$linear.predictors)

plot(gamDF$gam_nnplus1.y, test)

plot(model$N, model$NPlus1)
test <- fitted.values(gam_nnplus1)
