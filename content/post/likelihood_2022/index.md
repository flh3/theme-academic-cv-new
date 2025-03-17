---
title: Using REML and ML for estimation
author: Francis Huang
date: '2021-06-19'
categories:
  - R
tags:
  - mlm
header:
  caption: ''
  image: ''
  preview: yes
---

*More notes to self...* Obtaining estimates of the unknown parameters in multilevel models is often done by optimizing a likelihood function. The estimates are the values that maximize the likelihood function given certain distributional assumptions.

The likelihood function differs depending on whether maximum (ML) or restricted maximum (REML) likelihood is used. For ML, the log likelihood function to be maximized is:

$$
\ell_{ML}(\theta)=-0.5n \times ln(2\pi) -0.5 \times \sum_{i}{ln(det(V_i))} - 0.5 \times \sum_{i}{r^{\prime}_iV_i^{-1}r_i}
$$

where the residual $r_i$:

$$r_i = y_i - X_i \beta$$ The likelihood function for ML is made up of three terms added together. The first one is just based on the sample size $n$. The second needs the determinant of $V_i$. The third involves the residuals.

## ML example


``` r
library(lme4)
m1 <- lmer(Reaction ~ Days + (Days | Subject), sleepstudy, REML = F) #for ML estimates

## some functions to get the FE
getFE <- function(X, V, y){
  solve(t(X) %*% solve(V) %*% X) %*% t(X) %*% solve(V) %*% y
}

## and the V matrix
# https://stackoverflow.com/questions/45650548/get-residual-variance-covariance-matrix-in-lme4
getV <- function(x){
  var.d <- crossprod(getME(x, "Lambdat"))
  Zt <- getME(x, "Zt")
  vr <- sigma(x)^2
  var.b <- vr * (t(Zt) %*% var.d %*% Zt)
  sI <- vr * Matrix::Diagonal(nobs(x)) #for a sparse matrix
  var.y <- var.b + sI
}
X <- model.matrix(m1) #design matrix
y <- m1@resp$y #outcome
V <- getV(m1) #V matrix
getFE(X, V, y) #get FE
```

```
## 2 x 1 Matrix of class "dgeMatrix"
##              [,1]
## (Intercept) 251.4
## Days         10.5
```

``` r
fixef(m1) #automated FE- the same
```

```
## (Intercept)        Days 
##       251.4        10.5
```

Just obtaining the information above to check that the $V$ matrix is correctly computed. Now we can compute the log likelihood:


``` r
getf1 <- function(x, clust){
  ch1 <- which(clust == x)
  Xs <- X[ch1, , drop = F] #X per cluster
  ns <- nrow(Xs)
  Vm <- V[ch1, ch1]
  f1 <- log(det(Vm))
}

rr <- y - X %*% fixef(m1) #residuals
cnames <- row.names(getME(m1, 'Ztlist')[[1]])
Gname <- names(getME(m1, 'l_i'))
Vinv <- solve(V) #can take a while if inverting a large matrix

firsttermML = -.5 * (nobs(m1) * log(2 * pi))
c1a <- -.5 * sum(sapply(cnames, getf1, clust = m1@frame[,Gname]))
#c1 <- -.5 * log(det(V)) #can only do this with small matrices
c2a <- -.5 * as.numeric(t(rr) %*% solve(V) %*% rr) #doing this all together
firsttermML + c1a + c2a #our manual likelihood computation
```

```
## [1] -876
```

``` r
logLik(m1) #these are the same
```

```
## 'log Lik.' -876 (df=6)
```

## REML example

Just using the earlier model but now using REML. The likelihood function differs from that of ML:

$$
\ell_{REML}(\theta)=-0.5 \times (n-p) \times ln(2\pi) -0.5 \times \sum_{i}{ln(det(V_i))} - 0.5 \times \sum_{i}{r^{\prime}_iV_i^{-1}r_i} -0.5 \times \sum_{i}{ln(det(X_i^{\prime}V_i^{-1}X_i))}
$$

Now there are four terms to add together (the first and last terms differ; the first term is now $n - p$ rather than just $n$).


``` r
m2 <- update(m1, REML = T)
#X and y are the same, just need to change V
V <- getV(m2) #different now
rr <- y - X %*% fixef(m2) #residuals
Vinv <- solve(V)
```

We can then compute the components and add them together:


``` r
c1 <- -.5 * sum(sapply(cnames, getf1, clust = m2@frame[,Gname]))
c2 <- -.5 * as.numeric(t(rr) %*% Vinv %*% rr)
c3 <- -.5 * log(det(t(X) %*% Vinv %*% X))

nadj <- nobs(m2) - ncol(X)
firstterm = -.5 * (nadj * log(2 * pi))

firstterm + c1 + c2 + c3 ##our manual REML likelihood computation
```

```
## [1] -872
```

``` r
logLik(m2) #the same
```

```
## 'log Lik.' -872 (df=6)
```

## Estimating a model

In reality, we do not know the variance components which is why computers solve this iteratively using different optimization algorithms. But, since we know the log likelihood computation, we can tell the computer to figure out the unknowns that maximize that function.

Here's an example (this is not usually solved in this manner). Just using the `mtcars` dataset (with `cyl` as the clustering variable). All of the following are computed as inputs used by the log likelihood function.


``` r
data(mtcars)
mtcars <- mtcars[order(mtcars$cyl), ]
X <- model.matrix(~wt + am, data = mtcars) #X design matrix
y <- mtcars$mpg #outcome
# make a z matrix for a random intercept model
mtcars$int <- 1 #the intercept
tmp <- split(mtcars[,'int'], mtcars$cyl)
Z <- bdiag(tmp) #getME(m1, "Z"); create a block diagonal Z matrix
cnames <- c('4', '6', '8')
Gname <- 'cyl' #cluster name
nadj <- nrow(mtcars) - ncol(X) #for REML
firstterm = -.5 * (nadj * log(2 * pi)) #constant
ngs <- length(cnames) #number of groups
ns <- nrow(X) #total number of observations
dframe <- mtcars
```

The log likelihood function to be maximized:


``` r
rml <- function(x){
  #the variances are exponentiated to make sure it does not become negative
  G <- kronecker(diag(ngs), exp(x[1])) #estimate T00 var
  R <- diag(exp(x[2]), ns) #estimate sigma
  V <- Z %*% G %*% t(Z) + R
  beta <- solve(t(X) %*% solve(V) %*% X) %*% t(X) %*% solve(V) %*% y
  rr <- y - X %*% beta
  Vinv <- solve(V)
  
  getf1 <- function(x, clust){
    ch1 <- which(clust == x)
    Xs <- X[ch1, , drop = F] #X per cluster
    ns <- nrow(Xs)
    Vm <- V[ch1, ch1]
    f1 <- log(det(Vm))
  } #only need this for the log determinant of the v matrix
  
  c1 <- -.5 * sum(sapply(cnames, getf1, clust = dframe[,Gname]))
  c2 <- -.5 * as.numeric(t(rr) %*% Vinv %*% rr)
  c3 <- -.5 * log(det(t(X) %*% Vinv %*% X))
  xx <- firstterm + c1 + c2 + c3 #REML log likelihood function
  return(xx)
}
```

Can just test this out to see if it gives us back a value:


``` r
rml(x = c(1, 1)) #does it return a value? just testing
```

```
## [1] -83.9
```

Then, we can use an optimization algorithm to figure out the variance components. NOTE: This is generally *NOT* how this is done but just showing to see how this works. If we know the variance components, can solve for everything else. We can compare this to the results using `lmer`.


``` r
out <- optim(par = c(1, 1), rml, #starting values and function to optimize
 control = list(fnscale = -1, #-1 to maximize
 reltol = 1e-20, #tolerance
 maxit = 100)) #so it doesn't keep iterating

#1, 1 are just starting values. optim_nm (Nelder Mead) will take it from there
round(exp(out$par), 4) #exponentiate to get variance components
```

```
## [1] 8.04 6.79
```

``` r
out$value #this is the maximum log likelihood 
```

```
## [1] -75.2
```

``` r
## what if we run this using lmer?
## doing this so we can compare
g1 <- lmer(mpg ~ wt + am + (1|cyl), data = mtcars)
logLik(g1) #the same
```

```
## 'log Lik.' -75.2 (df=5)
```

``` r
print(VarCorr(g1), comp = 'Variance') #the same
```

```
##  Groups   Name        Variance
##  cyl      (Intercept) 8.04    
##  Residual             6.79
```

---END
