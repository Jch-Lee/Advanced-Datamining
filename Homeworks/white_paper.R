library(tree)
library(ISLR)
data(Auto)
x <- scale(Auto[,3:7])
y <- Auto$mpg

set.seed(13579)
gr <- sample(rep(seq(5), length=length(y)))

R_squared <- function(y_pred, y_actual) {
  rss <- sum((y_actual - y_pred)^2)
  tss <- sum((y_actual - mean(y_actual))^2)
  rsq <- 1-(rss/tss)
  return(rsq)
}

### 1.
combs <- list()

for (i in 1:max(gr)) {
  combs[[i]] <- combn(colnames(x), i)
}

combs

for (i in 1:length(combs)) {
  for (p in 1:ncol(combs[[i]])) {
    print("--------------------------")
    print(combs[[i]][,p])
  }
}
predictors <- combs[[2]][,3]
tr <- data.frame(x[,predictors], target=y)
RT <- tree(target ~., data=tr)

predict(RT, newdata = data.frame(x[,predictors]))
RT$y

plot(RT)
text(RT)


set.seed(111)
boot <- vector(mode="list", length=max(gr))
for (k in 1:max(gr)) {
  mat <- matrix(1:sum(gr!=k), sum(gr!=k), 500)
  mat <- apply(mat, 2, function(t) sample(t, replace=TRUE))
  boot[[k]] <- mat
}

dim(boot[[1]])


matrix(c(1,2,3,4,5,6), ncol=2)[c(1,2,2,2),]
seq(from=1,to=3,length.out=20)


for (i in 1:20) {
  print(-i)
  for (i in 1:5) {
    print(i)
  }
}


vec <- c(1,2,3,4,5)
abs(vec) < 3
ifelse(abs(vec) < 3, 2*vec, 0)


vec <- 1:5000000
res <- rep(NA, 5000000)
execution_time <- system.time({
  # 실행할 코드
  for (i in vec) {
    res[i] <- sqrt(vec[i])
  }
})

print(execution_time)

vec <- 1:5000000
res <- rep(NA, 5000000)
execution_time <- system.time({
  # 실행할 코드
  res <- sapply(vec, sqrt)
})

print(execution_time)
