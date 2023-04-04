# Problem Set 8 Codes
# Trevor R Jones

library(nloptr)

## 4 ##
set.seed(100)

# N = 100000, K = 10
# had to change N to lower number because of storage issues
N <- 100000
K <- 10
# create matrix
X <- matrix(rnorm(N*K, mean = 0, sd =1), ncol = K)
X[,1] <- 1

# Create Eps
eps <- rnorm(N, mean = 0, sd = 0.5)

# Create the beta vector
beta <- c(1.5, -1, -0.25, 0.75, 3.5, -2, 0.5, 1, 1.25, 2)

# Create variable Y
y <- X %*% beta + eps

## 5 ##
beta.ols <- solve(t(X)%*%X) %*% t(X) %*% y
beta.ols
beta.ols - beta # checking difference

## 6 ##
alpha <- 0.0000003
iter <- 500
gradient <- function(x) return(solve(t(X)%*%X) %*% t(X) %*% y)
# randomly initialize a value to x
set.seed(100)
x <- floor(runif(1)*10)
# create a vector to contain all xs for all steps
x.All <- vector("numeric",iter)
# gradient descent method to find the minimum
for(i in 1:iter){
  x <- x - alpha*gradient(x)
  x.All[i] <- x
  print(x)
}
print(paste("The minimum of f(x) is ", x, sep = ""))


## 7 ##
# Our objective function
objfun <- function(beta,y,X) {
  return (sum((y-X%*%beta)^2))
}
# Gradient of our objective function
gradient <- function(beta,y,X) {
  return ( as.vector(-2*t(X)%*%(y-X%*%beta)) )
}
# initial values
beta0 <- runif(dim(X)[2]) #start at uniform random numbers equal to number  coefficients

# L-BFGS algorithm
# Algorithm parameters
options <- list("algorithm"="NLOPT_LD_LBFGS","xtol_rel"=1.0e-6,"maxeval"=1e3)
# Optimize
result <- nloptr( x0=beta0,eval_f=objfun,eval_grad_f=gradient,opts=options,y=y,X=X)
print(result) # Optimal value of objective function:  24953.283521585

# Nelder-Mead
options1 <- list("algorithm"="NLOPT_LN_NELDERMEAD","xtol_rel"=1.0e-6,"maxeval"=1e4)
beta0 <- runif(dim(X)[2])
result1 <- nloptr( x0=beta0,eval_f=objfun,eval_grad_f=gradient,opts=options1,y=y,X=X)
print(result1)
# Optimal value of objective function:  24953.2835222037 
# The two give very similar results


## 8 ##
# Compute BMLE using L-BFGS
# New objective function
objfun1  <- function(theta, y, X) {
  # need to slice our parameter vector into beta and sigma components
  beta    <- theta[1:(length(theta)-1)]
  sig     <- theta[length(theta)]
  # write objective function as *negative* log likelihood (since NLOPT minimizes)
  loglike <- -sum( -.5*(log(2*pi*(sig^2)) + ((y-X%*%beta)/sig)^2) ) 
  return (loglike)
}
# new gradient
gradient1 <- function(theta, y, X) {
  grad <- as.vector(rep(0, length(theta)))
  beta <- theta[1:length(theta)-1]
  sig <- theta[length(theta)]
  grad[1:(length(theta)-1)] <- -t(X) %*% (y-X%*%beta)/(sig^2)
  grad[length(theta)] <- dim(X)[1]/sig - crossprod(y-X%*%beta)/(sig^3)
  return(grad)
}
# initial values
theta0 <- runif(dim(X)[2]+1)
# Perameters
options2 <- list("algorithm"="NLOPT_LD_LBFGS","xtol_rel"=1.0e-6,"maxeval"=1e4)
# Results
result2 <- nloptr(x0=theta0,eval_f=objfun1,eval_grad_f=gradient2,opts=options2,y=y,X=X)
print(result2)
# Optimal value of objective function:  72485.6149015635 something clearly went
# but I don't know what it was

## 9 ## 
est <- lm(Y ~ X-1)
library(modelsummary)
modelsummary(est, output = "simplereg.tex")

