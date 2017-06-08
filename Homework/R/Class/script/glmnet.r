#basis_function.r
#generate Gaussian basis functions

library(glmnet)
setwd('C:\\Users\\Hsinmin\\svn_drive2\\teaching\\statistical learning\\2016\\slides\\r_regression')
adv=read.csv('Advertising.csv')

#create x-matrix, excluding constant column
xmat = model.matrix(Sales~., data=adv)[,-1]


glmmod <- glmnet(xmat, adv$Sales, alpha=0, family="gaussian")

# Plot variable coefficients vs. shrinkage parameter lambda.


lbs_fun <- function(fit, ...) {
        L <- length(fit$lambda)
        x <- log(fit$lambda[L])
        y <- fit$beta[, L]
        labs <- names(y)
        text(x+1, y+0.001, labels=labs, cex=1.5, ...)
}
plot(glmmod, xvar="lambda", label=FALSE, lwd=2)
# label
lbs_fun(glmmod)

glmmod2 <- glmnet(xmat, adv$Sales, alpha=1, nlambda=30, family="gaussian")
plot(glmmod2, xvar="lambda", label=FALSE, lwd=2)
# label
lbs_fun(glmmod2)



#cex=0.6
#cex.lab=1.1, cex.axis=1.1, cex.main=1.1, cex.sub=1.1





