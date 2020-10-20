install.packages("rpart")
install.packages("FNN")
install.packages("mlbench")
library(rpart)
library(FNN)
library(mlbench)
data(BostonHousing)

#####################################################################

############## correlation
n <- 1000
m <- 10
p <- 20
M <- matrix(NA, nrow = m, ncol = p)
f <- function(x) {
  sapply(1:s, function(x){(-1)^x})%*%(x[1:s])^2
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
  M[i,s] <- sqrt(min(dt$splits[,"improve"], na.rm = TRUE))
 }
}
par(mar=c(5, 5, 5, 5))
plot(apply(M, 2, mean), col = "black", pch = 19, type = "p", xlab = "sparsity", ylab = "correlation", ylim = c(0, 1), cex.axis=2.5, cex.lab=2.5, cex=2)

#####################################################################

############## synthetic data
n <- 1250
m <- 10
d <- 100
s <- 5
M1 <- matrix(NA, nrow = m, ncol = (d-s+1))
M2 <- matrix(NA, nrow = m, ncol = (d-s+1))
f <- function(x) {
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
 }
}
par(mar=c(5, 5, 5, 5))
plot(s:d, apply(M1, 2, mean), col = "blue", pch = 19, type = "p", xlab = "d", ylab = "test error", cex.axis=2.5, cex.lab=2.5, cex=2)
lines(s:d, apply(M2, 2, mean), col = "red", pch = 19, type = "p", cex=2)
legend("topleft", legend=c("k-NN", "CART"), fill=c("blue", "red"), bty = "n", pt.cex = 1, cex=1.5)

#####################################################################

############## real data
n <- 506
m <- 10
d <- 100
s <- 10
M1 <- matrix(NA, nrow = m, ncol = d)
M2 <- matrix(NA, nrow = m, ncol = d)
for(i in 1:m) {
  for(p in 0:d) {
    X <- cbind(BostonHousing[,c(-2, -4, -9, -14)], matrix(runif(p*n, 0, 1), n, p))
    maxs <- apply(X, 2, max)
    mins <- apply(X, 2, min)
    X <- scale(X, center = mins, scale = maxs - mins)
    Y <- BostonHousing[,14]
    df <- data.frame(Y, X)
    colnames(df) <- c("Y", paste("X", 1:(p+s), sep = "_"))
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
    M1[i,p] <- mean((test$predict-test$Y)^2)
    
    ############## decision tree
    
    dt <- rpart(Y~., data = full, method = "anova", cp = -1, minbucket = 1, minsplit = 2)
    #, maxcompete = 0, maxsurrogate = 0)
    opt <- dt$cptable[which.min(dt$cptable[,"xerror"]), "CP"]
    dt <- prune(dt, cp = opt)
    test$predict <- predict(dt, test, type = "vector")
    M2[i,p] <- mean((test$predict-test$Y)^2)
  }
}
par(mar=c(5, 5, 5, 5))
plot(s:(d+s-1), apply(M1, 2, mean), col = "blue", pch = 19, type = "p", ylim = c(10, 90), xlab = "d", ylab = "test error", cex.axis=2.5, cex.lab=2.5, cex=2)
lines(s:(d+s-1), apply(M2, 2, mean), col = "red", pch = 19, type = "p", cex=2)
legend("topleft", legend=c("k-NN", "CART"), fill=c("blue", "red"), bty = "n", pt.cex = 1, cex=1.5)
