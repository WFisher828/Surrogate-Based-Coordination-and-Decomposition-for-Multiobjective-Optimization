source("function.R")
library(lhs)
library(glmnet)
library(seriation)
n <- 100
d <- 8

#generate x and y
x <- randomLHS(n,d)*2-1
y <- t(apply(x, 1, compute_spring_objectives))


# find coef through lasso
coefs <- NULL
for(i in 1:10) {
  cv_model <- cv.glmnet(x, log(y[,i]))
  coefs <- rbind(coefs, 
                 coef(cv_model)[-1,1])
}

row.names(coefs) <- paste("f", 1:10, sep="")
names(coefs) <- paste("x", 1:8, sep="")

# seriation for decomposition
order <- seriate(coefs)
bertinplot(coefs, order)
