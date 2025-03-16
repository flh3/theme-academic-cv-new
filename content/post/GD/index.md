---
title: Gradient descent example 1
author: Francis Huang
date: '2023-11-01'
slug: GD_example
categories:
  - regression
tags:
  - regression
header:
  caption: ''
  image: ''
  preview: yes
---




Random notes. Regression based techniques often involve finding a maximum (e.g., the maximum likelihood) or a minimum (e.g., least squares or mean square error) value. Gradient descent is an iterative optimization algorithm used to find the minimum of a function (or gradient *ascent* to find the maximum).

The algorithm for solving for $\theta_j$ looks like:

$$\theta_j = \theta_j - \alpha\frac{\partial{}}{\partial{\theta_j} }J(\theta)$$

+ $\alpha$ is the learning rate (smaller step size takes more iterations)

+ $J(\theta)$ is the loss or cost function


### 1. Create a dataset


``` r
set.seed(1233)
ns <- 500
x1 <- rnorm(ns)
err <- rnorm(ns)
y <- x1 * .5 + err
dat <- data.frame(y, x1)
```

### 2. Initialize the algorithm

Specify the:

- design matrix (X), 
- learning rate (here .05), 
- tolerance level (`tol`), and 
- number of maximum iterations (`iter`).


``` r
X <- model.matrix(~x1, data = dat) #design matrix
k <- ncol(X) #number of predictors
lr <- .05 #this is the learning rate
b <- rnorm(k) #random 
pred <- X %*% b #predicted values
mse <- sum(y - X %*% b)^2 / ns #initial mean square error
tol <- 1e-16 #tolerance level
iter <- 1000 #number of max iterations
```

### 3. Run the algorithm

Iterate until the MSE doesn't change much (based on the tolerance level).


``` r
for (i in 1:iter){
  
  dint <- (-2 / ns) * sum(y - pred) # compute the partial der with respect to the intercept
  dslope <- (-2/ns) * sum((y - pred) * x1) # compute the partial der with respect to the slope                
        
  b[1] <- b[1] - lr * dint #update dMSE / dint
  b[2] <- b[2] - lr * dslope #update dMSE / db1
  pred <- X %*% b
  
  msenew <- sum(y - pred)^2 / ns
  conv <- abs(mse - msenew)
  mse <- msenew
  if (conv < tol) (break)
}
```

### 4. Compare the results:

Results are the same


``` r
cat("final iteration:", i, "coefs:", b, "convergence:", conv, "\n") #based on the iterations
```

```
final iteration: 196 coefs: -0.0724 0.448 convergence: 9.31e-17 
```

``` r
solve(t(X) %*% X) %*% t(X) %*% y #the usual way
```

```
               [,1]
(Intercept) -0.0724
x1           0.4476
```

``` r
lm(y ~ x1) #using built in function
```

```

Call:
lm(formula = y ~ x1)

Coefficients:
(Intercept)           x1  
    -0.0724       0.4476  
```

### 5. Using matrix notation (to make it scalable)


``` r
set.seed(246)
ns <- 1000
x1 <- rnorm(ns)
err <- rnorm(ns)
y <- x1 * .5 + err
dat <- data.frame(y, x1)
X <- model.matrix(~x1, data = dat)
k <- ncol(X)
lr <- .05
b <- rnorm(k)
pred <- X %*% b
mse <- sum(y - X %*% b)^2 / ns
tol <- 1e-16
iter <- 1000
for (i in 1:iter){
  
  grad <- (-2 / ns) * (t(X) %*% (y - pred))
  b <- b - lr * grad
  pred <- X %*% b
  
  msenew <- sum(y - pred)^2 / ns
  conv <- abs(mse - msenew)
  mse <- msenew
  if (conv < tol) (break)
  
}
cat("final iteration:", i, "coefs:", b, "convergence:", conv, "\n")
```

```
final iteration: 198 coefs: 0.0325 0.479 convergence: 8.41e-17 
```

``` r
solve(t(X) %*% X) %*% t(X) %*% y
```

```
              [,1]
(Intercept) 0.0325
x1          0.4794
```

### 6. Another test...

Using matrix notation and testing it with a more complex formula (e.g., including interactions).


``` r
### scaling up

set.seed(12345)
ns <- 1000
x1 <- rnorm(ns)
x2 <- rnorm(ns)
err <- rnorm(ns)
y <- x1 * .5 + .5 * x2 + .5 * x1 * x2 + err
dat <- data.frame(y, x1, x2)
X <- model.matrix(~x1 + x2 + x1 * x2, data = dat)
k <- ncol(X)
lr <- .05
b <- rnorm(k)
pred <- X %*% b
mse <- sum(y - X %*% b)^2 / ns
tol <- 1e-16
iter <- 1000
for (i in 1:iter){
  
  grad <- (-2 / ns) * (t(X) %*% (y - pred))
  b <- b - lr * grad
  pred <- X %*% b
  
  msenew <- sum(y - pred)^2 / ns
  conv <- abs(mse - msenew)
  mse <- msenew
  if (conv < tol) (break)
  
}

cat("final iteration:", i, "coefs:", b, "convergence:", conv, "\n")
```

```
final iteration: 217 coefs: -0.026 0.49 0.495 0.521 convergence: 9.04e-17 
```

#### Just putting it all together


``` r
u <- pred - y #residual
num <- t(u) %*% u #numerator
den <- ns - k #denominator
rse <- sqrt(num/den) #residual standard error (manually)

se <- sqrt(diag(as.numeric(rse^2) * (solve(t(X) %*% X))))
t <- b / se
pv <- 2 * pt(-abs(t), df = ns - k)
data.frame(b = b, se = se, t = t, pv = pv)
```

```
                 b     se      t       pv
(Intercept) -0.026 0.0304 -0.856 3.92e-01
x1           0.490 0.0304 16.098 5.33e-52
x2           0.495 0.0301 16.454 5.40e-54
x1:x2        0.521 0.0296 17.601 1.38e-60
```

``` r
#solve(t(X) %*% X) %*% t(X) %*% y
summary(lm(y ~ x1 + x2 + x1 * x2))$coef
```

```
            Estimate Std. Error t value Pr(>|t|)
(Intercept)   -0.026     0.0304  -0.856 3.92e-01
x1             0.490     0.0304  16.098 5.33e-52
x2             0.495     0.0301  16.454 5.40e-54
x1:x2          0.521     0.0296  17.601 1.38e-60
```

This is of course just an example. We don't really run our regressions this way... (e.g., it can be inefficient, need to specify the learning rate hyperparameter).


--- END



