library(keras)
mnist_lst <- dataset_mnist()

X_train <- mnist_lst[[1]]$x
y_train <- mnist_lst[[1]]$y
X_val <- mnist_lst[[2]]$x
y_val <- mnist_lst[[2]]$y

X_train <- X_train / 255
X_val <- X_val / 255

X_train_reshape <- matrix(X_train, nrow = 60000, ncol = 28 * 28)
X_val_reshape <- matrix(X_val, nrow = 10000, ncol = 28 * 28)

y_train_one_hot <- matrix(0, nrow = 60000, ncol = 10)
y_val_one_hot <- matrix(0, nrow = 10000, ncol = 10)

for(i in 1:60000) {
  val <- y_train[i]
  y_train_one_hot[i, val + 1] <- 1
}

for(i in 1:10000) {
  val <- y_val[i]
  y_val_one_hot[i, val + 1] <- 1
}

init <- function() {
 w1 <<- matrix(rnorm(784 * 100, 0, 1), 100, 784)
 w2 <<- matrix(rnorm(100 * 50, 0, 1), 50, 100)
 w3 <<- matrix(rnorm(50 * 30, 0, 1), 30, 50)
 w4 <<- matrix(rnorm(30 * 10, 0, 1), 10, 30)
}

sigmoid <- function(z) {
  return(1 / (1 + exp(-z)))
}

softmax <- function(z) {
  return(exp(z) / sum(exp(z)))
}

sigmoid_prime <- function(z) {
  return(sigmoid(z) * (1 - sigmoid(z)))
}

crossentropy <- function(y_hat, y) {
  return(- y %*% log(y_hat))
}

crossentropy_prime <- function(y_hat, y) {
  return(y_hat - y)
}

feedforward <- function(x) {
  z1 <- w1 %*% x
  a1 <- sigmoid(z1)
  z2 <- w2 %*% a1
  a2 <- sigmoid(z2)
  z3 <- w3 %*% a2
  a3 <- sigmoid(z3)
  z4 <- w4 %*% a3
  y_hat <- softmax(z4)
  return(y_hat)
}

backprop <- function(x, y) {
  z1 <- w1 %*% x
  a1 <- sigmoid(z1)
  z2 <- w2 %*% a1
  a2 <- sigmoid(z2)
  z3 <- w3 %*% a2
  a3 <- sigmoid(z3)
  z4 <- w4 %*% a3
  y_hat <- softmax(z4)
  
  delta4 <- crossentropy_prime(y_hat, y)
  delta3 <- sigmoid_prime(z3) * t(w4) %*% delta4
  delta2 <- sigmoid_prime(z2) * t(w3) %*% delta3
  delta1 <- sigmoid_prime(z1) * t(w2) %*% delta2
  
  backprop_w1 <- delta1 %*% t(x)
  backprop_w2 <- delta2 %*% t(a1)
  backprop_w3 <- delta3 %*% t(a2)
  backprop_w4 <- delta4 %*% t(a3)
  
  backprop_w_list <- list(backprop_w1, backprop_w2, backprop_w3, backprop_w4)
  
  return(backprop_w_list)
}

mini_batch <- function(X, y, lr = 0.001, batch_size = 200) {
  n <- nrow(X)
  m <- batch_size
  suffle_index <- sample(n, n)
  X_suffle <- X[suffle_index, ]
  y_suffle <- y[suffle_index, ]
  
  for(i in 1:(n / m)) {
    X_batch <- X_suffle[(m * (i - 1) + 1):(m * i), ]
    y_batch <- y_suffle[(m * (i - 1) + 1):(m * i), ]
    n_w1 <- matrix(0, nrow = dim(w1)[1], ncol = dim(w1)[2])
    n_w2 <- matrix(0, nrow = dim(w2)[1], ncol = dim(w2)[2])
    n_w3 <- matrix(0, nrow = dim(w3)[1], ncol = dim(w3)[2])
    n_w4 <- matrix(0, nrow = dim(w4)[1], ncol = dim(w4)[2])
    
    for(j in 1:m) {
      backprop_w_list = backprop(X_batch[j, ], y_batch[j, ])
      n_w1 <- n_w1 + matrix(unlist(backprop_w_list[1]), nrow = dim(w1)[1], ncol = dim(w1)[2])
      n_w2 <- n_w2 + matrix(unlist(backprop_w_list[2]), nrow = dim(w2)[1], ncol = dim(w2)[2])
      n_w3 <- n_w3 + matrix(unlist(backprop_w_list[3]), nrow = dim(w3)[1], ncol = dim(w3)[2])
      n_w4 <- n_w4 + matrix(unlist(backprop_w_list[4]), nrow = dim(w4)[1], ncol = dim(w4)[2])
    }

    w1 <<- w1 - (lr / m) * n_w1
    w2 <<- w2 - (lr / m) * n_w2
    w3 <<- w3 - (lr / m) * n_w3
    w4 <<- w4 - (lr / m) * n_w4
  }
}

loss <- function(X, y) {
  n <- nrow(X)
  sum <- 0
  for(i in 1:n){
    y_hat <- feedforward(X[i, ])
    sum <- sum + crossentropy(y_hat, y[i, ])
  }
  return(sum / n)
}

evaluate <- function(X, y) {
  n <- nrow(X)
  sum <- 0
  for(i in 1:n){
    y_hat <- feedforward(X[i, ])
    if(which.max(y_hat) == which.max(y[i, ])) {
      sum <- sum + 1
    }
  }
  return(sum / n)
}

fit <- function(X, y, epochs, lr = 0.001, batch_size = 100) {
  n <- nrow(X)
  for(i in 1:epochs) {
    mini_batch(X, y, lr=lr, batch_size=batch_size)
    loss <- loss(X, y)
    accuracy <- evaluate(X, y)
    cat('epoch : ', i, '\t loss : ', loss, '\t acc : ', accuracy, '\n')
  }
}

predict <- function(X) {
  y_hat <- feedforward(X)
  pred <- which.max(y_hat)
  return(pred - 1)
}

init()
fit(X_train_reshape, y_train_one_hot, epochs = 20, lr = 0.1)
evaluate(X_val_reshape, y_val_one_hot)

predict(X_train_reshape[11, ])
y_train[11]
