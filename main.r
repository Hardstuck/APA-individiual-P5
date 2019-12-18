g <- function(x){
  if(x == 0) return (1)
  else return (sin(x)/x)
}
f <- function(x){
  noise <- rnorm(1, 0, 0.01)
  return (g(x) + noise)
}

library(nnet)
library(caret)

n <- readline("Option 1: 100, Option 2: 200, Option 3: 500 (Introduce the option number)")
n <- as.integer(n)
if (n == 1) {
  n <- 100
}
else if (n == 2) {
  n <- 200
}
else {
  print("You are stupid and have no idea to choose an option =)")
  n <- 500
}

x <- runif(n, min = -20, max = 20)
results <- unlist(lapply(x, f))
data <- data.frame("x" = x, "fx" = results)
learn = sample(round(2*n/3), n/3)

sizes <- seq(1,8,by=1)
decays <- 10**seq(-3, 0, by = 0.1)
trc <- trainControl(method = "repeatedcv", number = 10, repeats = 10)
model.10x10CV <- train(x~., data = data, subset=learn, method = 'nnet', maxit = 500, trace = FALSE, tuneGrid = expand.grid(.size=sizes ,.decay = decays), trControl = trc)
prediction <- predict(model.10x10CV)

plot(data[,1], prediction)

question1 <- readline("Would you like to proceed untill the loop ends? (Y/N)")

if(regexpr(question1, 'y', ignore.case = TRUE) == 1){
#model.nnet <- nnet(x~., data = data, subset = learn, size = 2, maxit = 5000, decay = 0)
  model.LR <- lm(x ~ ., data = data)
  model.LRS <- lm(x ~ ., data = data, subset = learn)
  prediction.LR <- predict(model.LR)
  prediction.LRS <- predict(model.LR, newdata = data)

  norm.mseLR <- sum(data[,2]-prediction.LR)^2) /((N-1)*var(data[,2]))
  norm.mseLRS <- sum(data[,2]-prediction.LRS)^2) /((N-1)*var(data[,2]))
  R.squaredLR <- (1-norm.mseLR)*100
  R.squaredLRS <- (1-norm.mseLRS)*100

  question2 <- readline("Introduce the number of testing set, else answer No")
  if(regexpr(question1, 'n', ignore.case = TRUE) != 1){
    ntest <- as.integer(question2)
    test <- runif(1000, min = -20, max = 20)
    prediction.test <- predict(model.10x10CV, newdata = test)
    prediction.LRtest <- predict(model.LR, newdata = test)
    prediction.LRStest <- predict(model.LRS, newdata = test)

    norm.test <- sum(test[,2] - prediction.test)^2/((N-1)*var(test[,2]))
    norm.mseLRtest <- sum(test[,2]-prediction.LRtest)^2) /((N-1)*var(test[,2]))
    norm.mseLRStest <- sum(test[,2]-prediction.LRStest)^2) /((N-1)*var(test[,2]))
    R.squaredLRtest <- (1-norm.mseLR)*100
    R.squaredLRStest <- (1-norm.mseLRS)*100
    R.squaredtest <- (1-norm.msetest)*100
  }
}
