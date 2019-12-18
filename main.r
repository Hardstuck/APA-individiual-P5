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
#n <- 100
n <- 200
#n <- 500

x <- runif(n, min = -20, max = 20)
results <- unlist(lapply(x, f))
data <- data.frame("x" = x, "fx" = results)
learn = sample(round(2*n/3), n/3)

sizes <- seq(1,5,by=1)
decays <- 0.05 * seq(1, 20, by = 1)
trc <- trainControl(method = "repeatedcv", number = 10, repeats = 10)
model.10x10CV <- train(x~., data = data, subset=learn, method = 'nnet', maxit = 500, trace = FALSE, tuneGrid = expand.grid(.size=sizes ,.decay = decays), trControl = trc)

#model.nnet <- nnet(x~., data = data, subset = learn, size = 2, maxit = 5000, decay = 0)

model.linearRegression <- lm(x ~ ., data = data)
model.linearRegressionSubset <- lm(x ~ ., data = data, subset = learn)
