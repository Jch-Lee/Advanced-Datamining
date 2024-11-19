library(tree)
library(ISLR)
data(Auto)
x <- scale(Auto[,3:7])
y <- Auto$mpg


rt <- tree(target~., data=data.frame(cbind(x, target=y)))

plot(rt)
text(rt)

predict(rt)==predict(rt, newdata = data.frame(cbind(x, target=y)))

unique(predict(rt))

sumr <- summary(rt)
sumr


lm <- lm(target ~., data=data.frame(cbind(x,target=y)))
as.numeric(y-predict(lm))-as.numeric(lm$residuals)

data.frame(x[,c("displacement","weight")], target=y)
dt <- data.frame(x[,c("displacement")], target=y)
colnames(dt) <- c("displacement", "target")
lm(target~., data=dt)


set.seed(13579)
gr <- sample(rep(seq(5), length=length(y)))

set.seed(111)
boot <- vector(mode="list", length=max(gr))
for (k in 1:max(gr)) {
  mat <- matrix(1:sum(gr!=k), sum(gr!=k), 500)
  mat <- apply(mat, 2, function(t) sample(t, replace=TRUE))
  boot[[k]] <- mat
}
boot[[1]][,1]
dim(boot[[1]])



gamma <- 3
order(c(0.1,3,4,2,5))[1:gamma]
