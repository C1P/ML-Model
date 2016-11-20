##
library(ggplot2)
set.seed(1)
x <- seq(0,1,by = 0.01)
y <- sin(2*pi*x) + rnorm(length(x),0,0.1)
dat <- data.frame(x,y)
g <- ggplot(dat,aes(x,y)) + geom_point()
## 均方根误差
RMSE <- function(x,y){
    return(sqrt(mean((x-y)^2)))
}

##
fit <- lm(y~poly(x,degree = 5),data = dat)
dat$d1 <- predict(fit)
ggplot(dat) + geom_point(aes(x,y)) + geom_line(aes(x,d1),col = "steelblue")
summary(fit)
RMSE(dat$y,dat$d1)
##
Perf <- data.frame()
for(i in 1:15){
    fit <- lm(y~poly(x,degree = i),data = dat)
    RMSE1 <- RMSE(dat$y,predict(fit))
    Perf <- rbind(Perf,data.frame(Degree = i,PRMSE = RMSE1))
}
ggplot(Perf,aes(Degree,PRMSE)) + geom_line() + geom_point()

##
