---
title: Logistic regression from scratch (Newton Raphson and Fisher Scoring)
author: Francis Huang
date: '2022-02-15' 
slug: logistic-regression-fisher-scoring
categories:
  - logistic regression
  - R
tags:
  - logistic regression
header:
  caption: ''
  image: ''
  preview: yes
---



## 

In an earlier [post](../logistic-regression-using-matrices/), I had shown this using iteratively reweighted least squares (IRLS). This is just an alternative method using Newton Raphson and the Fisher scoring algorithm. For further details, you can look here as [well](http://www.jtrive.com/estimating-logistic-regression-coefficents-from-scratch-r-version.html).



``` r
library(MLMusingR)
data(suspend)

m1 <- glm(sus ~ male + gpa * frpl + fight + frmp.c * pminor.c, 
          data = suspend, 
          family = binomial)

### extracting raw components

dat <- model.frame(m1)
fml <- formula(m1)
X <- model.matrix(fml, dat)
y <- model.response(dat, 'numeric')
k <- ncol(X)
#pp <- function(x) 1 / (1 + exp(-x)) #convert logit to pred prob

beta_0 <- rep(0, k) #initialize with zeroes
eta <- X %*% beta_0 #predicted logit
mu <- plogis(eta) #same as pp but built-in; convert logit to pred prob
vr <- (mu * (1 - mu)) #variance

wts <- diag(as.vector(vr)) #create weight matrix
d2 <- t(X) %*% wts %*% X #the Fisher information matrix
u1 <- t(X) %*% (y - mu) #the score function
iter <- 50 #max number of iterations
tol <- 1e-8 #tolerance

### now iterate

for (i in 1:iter){
  #beta_1 <- beta_0 + solve(d2) %*% u1 #update beta
  beta_1 <- beta_0 + chol2inv(chol(d2)) %*% u1 #update beta avoiding inversion
  #print(beta_1) #show estimates
  
  if(any(abs((beta_1 - beta_0) / beta_0) < tol)) break #stop loop if no change

  eta <- X %*% beta_1 #predicted logit
  mu <- plogis(eta) #this is just pp(eta)
  vr <- mu * (1 - mu) #variance
  
  wts <- diag(as.vector(vr)) #putting in a weight matrix
  d2 <- t(X) %*% wts %*% X #information matrix
  u1 <- t(X) %*% (y - mu) #score function
  beta_0 <- beta_1
  cat(i, "::") #show iteration number
}
```

```
1 ::2 ::3 ::4 ::5 ::6 ::7 ::
```

``` r
### compare results

se <- sqrt(diag(solve(d2))) #get standard error, inverse of information matrix
z <- beta_1 / se
p.val <- 2 * pt(-abs(z), df = Inf)
df <- data.frame(beta = beta_1, se = se, z = z, p = format(p.val, 3))
df
```

```
                     beta       se      z        p
(Intercept)     -1.592202 0.269404 -5.910 3.42e-09
male             0.324897 0.099384  3.269 1.08e-03
gpa             -0.795479 0.084849 -9.375 6.90e-21
frpl            -0.562734 0.318874 -1.765 7.76e-02
fight            2.078100 0.098472 21.103 7.40e-99
frmp.c           0.003004 0.003189  0.942 3.46e-01
pminor.c        -0.002236 0.002302 -0.971 3.31e-01
gpa:frpl         0.387256 0.109169  3.547 3.89e-04
frmp.c:pminor.c  0.000124 0.000107  1.167 2.43e-01
```

``` r
#### to compare

m2 <- update(m1, epsilon = 1e-16) #change tolerance so results match
summary(m2)$coef
```

```
                 Estimate Std. Error z value Pr(>|z|)
(Intercept)     -1.592202   0.269404  -5.910 3.42e-09
male             0.324897   0.099384   3.269 1.08e-03
gpa             -0.795479   0.084849  -9.375 6.90e-21
frpl            -0.562734   0.318874  -1.765 7.76e-02
fight            2.078100   0.098472  21.103 7.40e-99
frmp.c           0.003004   0.003189   0.942 3.46e-01
pminor.c        -0.002236   0.002302  -0.971 3.31e-01
gpa:frpl         0.387256   0.109169   3.547 3.89e-04
frmp.c:pminor.c  0.000124   0.000107   1.167 2.43e-01
```

``` r
## equal deviances

-2 * sum((y * log(mu)) + (1 - y) * log(1 - mu))
```

```
[1] 3331
```

``` r
deviance(m2)
```

```
[1] 3331
```


## An alternative using maximum likelihood estimation (MLE)



``` r
m3 <- glm(sus ~ male + gpa + frpl + gpa * frpl, 
          data = suspend, 
          family = binomial)
X <- model.matrix(m3)

## this is the function to maximize
mle <- function(b){
  pp <- plogis(X %*% b)
  sum(dbinom(y, 1, pp, log = TRUE))
}

k <- ncol(X) #number of variables
sta <- rep(0, k) #initial values

out <- optim(par = sta, mle, #starting values and function to optimize
            control = list(fnscale = -1, #-1 to maximize
            reltol = 1e-15, #tolerance
            maxit = 1000),
            hessian = TRUE) #so it doesn't keep iterating
out$par #coefficients, difference due to tolerance?
```

```
[1] -0.371  0.673 -0.988 -0.543  0.406
```

``` r
coef(m3) #using built in function
```

```
(Intercept)        male         gpa        frpl    gpa:frpl 
     -0.371       0.673      -0.988      -0.543       0.406 
```

``` r
out$value #the log likelihood
```

```
[1] -1908
```

``` r
logLik(m3)
```

```
'log Lik.' -1908 (df=5)
```

``` r
sqrt(diag(solve(-out$hessian))) #standard errors
```

```
[1] 0.2386 0.0931 0.0777 0.2941 0.1010
```

``` r
sqrt(diag(vcov(m3)))
```

```
(Intercept)        male         gpa        frpl    gpa:frpl 
     0.2386      0.0931      0.0777      0.2941      0.1010 
```


-- END
