library(rpart)
library(rpart.plot)
#library(randomForest)
library(FNN)
#library(gam)
############## data
n <- 1250
m <- 10
p <- 20
M <- matrix(NA, nrow = m, ncol = p)
f <- function(x) {
  sapply(1:s, function(x){(-1)^x})%*%x[1:s]
}
for(i in 1:m) {
for(s in 1:p)  {
  X <- matrix(runif(p*n, 0, 1), n, p)
  Y <- apply(X, 1, f)
  df <- data.frame(Y, X)
  colnames(df) <- c("Y", paste("X", 1:p, sep = "_"))
  dt <- rpart(Y~., data = df, method = "anova", cp = 0, minbucket = 1, maxcompete = 0, maxsurrogate = 0)
  opt <- dt$cptable[which.min(dt$cptable[,"xerror"]), "CP"]
  dt <- prune(dt, cp = opt)
  test$predict <- predict(dt, test, type = "vector")
  M[i,s] <- sqrt(min(dt$splits[,"improve"], na.rm = TRUE))
 }
}
plot(apply(M, 2, mean), col = "black", type = "p", xlab = "sparsity", ylab = "correlation", ylim = c(0, 1), cex.axis=1.5, cex.lab=1.5)

#####################################################################

############## data
n <- 1250
m <- 10
d <- 100
s <- 5
M1 <- matrix(NA, nrow = m, ncol = (d-s+1))
M2 <- matrix(NA, nrow = m, ncol = (d-s+1))
#M3 <- matrix(NA, nrow = m, ncol = (d-s+1))
#M4 <- matrix(NA, nrow = m, ncol = (d-s+1))
f <- function(x) {
#  x[1]+2*x[2]-x[3]+10*x[4]+0.5*x[5]
#  sapply(1:s, function(x){(-1)^x})%*%x[1:s]+sum(x[1:s]%*%t(x[1:s]))-t(x[1:s])%*%x[1:s]
  sapply(1:s, function(x){(-1)^x})%*%(x[1:s])^2
  }
for(i in 1:m) {
for(p in s:d) {
  X <- matrix(runif(p*n, 0, 1), n, p)
  Y <- apply(X, 1, f)
  df <- data.frame(Y, X)
  colnames(df) <- c("Y", paste("X", 1:p, sep = "_"))
  type <- sample(1:3, size = n, replace = TRUE, prob = c(0.6, 0.2, 0.2))
  train <- df[type == 1,]
  test <- df[type == 2,]
  validate <- df[type == 3,]
  full <- df[type == 1|type == 3,]
  
  ################# nearest neighbors
  knn_pred = function(k) {
    validate$predict <- FNN::knn.reg(train = as.data.frame(train[,-1]), test = as.data.frame(validate[,-1]), y = as.data.frame(train[,1]), k)$pred
    mean((validate$predict-validate$Y)^2)
  }
  k <- seq(3, 100, by = 1)
  kopt <- k[which.min(sapply(k, knn_pred))]
  
  test$predict <- FNN::knn.reg(train = as.data.frame(train[,-1]), test = as.data.frame(test[,-1]), y = as.data.frame(train[,1]), kopt)$pred
  M1[i,(p-s+1)] <- mean((test$predict-test$Y)^2)
  
  ############## decision tree
  
  dt <- rpart(Y~., data = full, method = "anova", cp = -1, minbucket = 1, minsplit = 2)
              #, maxcompete = 0, maxsurrogate = 0)
  opt <- dt$cptable[which.min(dt$cptable[,"xerror"]), "CP"]
  dt <- prune(dt, cp = opt)
  test$predict <- predict(dt, test, type = "vector")
  M2[i,(p-s+1)] <- mean((test$predict-test$Y)^2)
  
  ############## linear model
  
#  lm <- lm(Y~., data = full)
#  test$predict <- predict(lm, test, type = "response")
#  M3[i,(p-s+1)] <- mean((test$predict-test$Y)^2)
  ############## random forest

#  rf <- randomForest(Y~., data = full)
#  test$predict <- predict(rf, test, type = "response")
#  M3[i,(p-s+1)] <- mean((test$predict-test$Y)^2)
  
  ############## backfitting
#  am <- gam(Y~., data = full)
#  test$predict <- predict(am, test, type = "response")
#  M4[i,(p-s+1)] <- mean((test$predict-test$Y)^2)
 }
}
par(mar=c(5, 5, 5, 5))
plot(s:d, apply(M1, 2, mean), col = "blue", pch = 19, type = "p", xlab = "d", ylab = "test error", cex.axis=2.5, cex.lab=2.5, cex=2)
lines(s:d, apply(M2, 2, mean), col = "red", pch = 19, type = "p", cex=2)
#lines(s:d, apply(M3, 2, mean), col = "green", pch = 19, type = "p", cex=2)
#lines(s:d, apply(M4, 2, mean), col = "black", pch = 19, type = "p", cex=2)
legend("topleft", legend=c("k-NN", "CART"), fill=c("blue", "red"), bty = "n", pt.cex = 1, cex=1.5)

################################# decision stump

n <- 20
X <- runif(n, 0, 1)
f <- function(x) { x }
Y <- f(X) + rnorm(n, 0, 0.1)
data <- data.frame(Y, X)
dt <- rpart(Y~., data, method = "anova", cp = 0, minsplit = 1, minbucket = 1, maxdepth = 1, maxcompete = p, maxsurrogate = 0, usesurrogate = 0)
g <- function(x) {
  predict(dt, data.frame(X = x), type = "vector")
}
plot(X, Y, col = "black", pch = 19, type = "p", xlab = "Xj", xlim = c(0, 1), cex.axis=2.5, cex.lab=2.5, cex=2)
plot(g, col = "blue", pch = 19, type = "l",  lwd = 5, xlim = c(0, 1), add=TRUE)


