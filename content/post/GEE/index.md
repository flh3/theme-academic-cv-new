---
title: 🎉 Analyzing clustered data using GEEs
summary: Generalized estimating equations
date: 2021-02-27
authors:
- admin
tags:
- generalized estimating equations
---


I show this in a recent JEBS article on using Generalized Estimating Equations (GEEs). Shown below is some annotated syntax and examples. 

> Huang, F. (2021). Analyzing cross-sectionally clustered data using generalized estimating equations. *Journal of Educational and Behavioral Statistics.* doi: 10.3102/10769986211017480

In the original paper draft, I had a section which showed how much more widely used mixed models (i.e., MLMs, HLMs) were compared to GEEs but was asked to remove that (to save space). I thought the usage was interesting so I am including it here:

In psychology, mixed model studies are much more popular than studies using GEEs by a ratio of 15:1 (Bauer & Sterba, 2011)

Citations in JEBS:

- In the Journal of Educational and Behavioral Statistics (JEBS): one article on how to use multilevel models by Singer (1998) has over 3,300 citations (as of 2020.06.11, Google Scholar)

- In the same journal, Ghisletta & Sini (2004) provided an introduction to GEEs. This article has 329 citations. GS wrote (p. 431):

> Although GEEs are widely applied in biological, pharmacological, and closely related disciplines, their application in educational and social sciences remains relatively scarce.

There is a difference of 6 years but the Singer article has been cited over **10** times more! If using average citations per year, **7.5** times more.

## Solving GEEs using IRLS

This example is only for an identity link using either an independence or exchangeable working correlation matrix.

1. Initial estimates for $\beta$ are estimated using a standard GLM (with an independence
correlation structure). Residuals are obtained. If an independence working correlation
structure is specified, these are also the final estimates, go to Step 7.

2. Using the residuals, $\alpha$ is estimated. An example of how this is computed with an
exchangeable correlation matrix is shown in Hardin and Hilbe (2013, pp. 65–67),
and we show this in our syntax as well.

3. The working exchangeable correlation matrix is updated to include a in the off
diagonals. The correlation matrix is converted to a covariance matrix, $V_j$, based on
the variance of the outcome. An overall $V$ block diagonal matrix is formed which
has the same $V_j$ for every cluster.

4. When using an identity link, new coefficients are reestimated using
$\hat{\beta} = (X'V^{-1}X)^{-1}X'V^{-1}y$

5. New residuals are obtained using the updated $\beta$.

6. Steps 2–5 are repeated until some convergence criteria are met (e.g., for continuous
outcomes, minimal changes in the sum of squared residuals).

7. Compute and apply cluster-robust/empirical standard errors based on formulas
also developed by Liang and Zeger (1986). CRSEs can be written as: $(X'X)^{-1}\hat{\Omega}(X'X)^{-1}$ where $\hat{\Omega}=\sum^{J}_{j=1}X^{'}_j\hat{u}_j\hat{u}_j'X_j$ and $\hat{u}_j$ are the residuals from the observations in cluster $j$.

## ICC estimation based on residuals

This example shows how to compute the exchangeable correlation coefficient (ICC) based on residuals. This is a different approach from using the between and within group variance components in a mixed model. This comes from Hardin and Hilbe's (2013) book on *Generalized Estimating Equations, 2nd edition*. Crespi et al. (2011) and Wu et al. (2012) show how there can be several approaches in computing the ICC/exchangeable correlation coefficient. 


``` r
NG = 2
id <- c(1,1,1,1,2,2,2,2)
t <- c(1,2,3,4,1,2,3,4) 
y <- c(4,5,6,7,5,6,7,8) 
x <- c(0,1,0,1,0,1,0,1)
dat <- data.frame(id, t, y, x)
```

- Scale parameter ($\phi$) must be estimated.
- NG = number of groups.
- GS = group size. Number of observations in the cluster.

$$
\hat{\phi} = \frac{1}{n}\sum^{NG}_{i=1}\sum^{GS}_{t=1}\text{res}^2_{it} 
$$
This is just the mean of the squared (Pearson) residuals. A basic OLS regression is used:


``` r
m1 <- lm(y ~ x, data = dat)
res <- resid(m1) #vector
dat$res <- res #save it into the original dataset
(scalep = mean(res^2)) #scale parameter
```

```
[1] 1.25
```

``` r
dat
```

```
  id t y x  res
1  1 1 4 0 -1.5
2  1 2 5 1 -1.5
3  1 3 6 0  0.5
4  1 4 7 1  0.5
5  2 1 5 0 -0.5
6  2 2 6 1 -0.5
7  2 3 7 0  1.5
8  2 4 8 1  1.5
```

The exchangeable correlation coefficient (e.g., compound symmetry, common correlation, equal correlation) can be estimated using (formula 3.28 in the HH book). This can be thought of as the intraclass correlation coefficient and at times can be seen as $\rho$ or $\alpha$.

$$
\begin{aligned}
\hat{\rho} &= \hat{\phi}^{-1}\frac{1}{12}\sum^{NG}_{i=1}\sum^{GS}_{t=1}\sum_{t'>t}\text{res}_{it}\text{res}_{it'} \\
&= \frac{1}{1.25}\frac{1}{12}\{[-1.5(-1.5+.5+.5) - 1.5(.5+.5) + .5(.5)] + 
[-.5(-.5+1.5+1.5)-.5(1.5+1.5)+1.5(1.5)]\} \\
&= .8\frac{1}{12}([-.5] + [.5]) \\
&= -.06667
\end{aligned}
$$
So we need to get the residuals per group

``` r
rpg <- split(res, dat$id)
nm <- names(table(dat$id))
r1 <- rpg[[1]] #residuals from first group
egeg <- r1 %*% t(r1) #quicker way to do this, orig a loop
(forg1 = sum(egeg[lower.tri(egeg)]))
```

```
[1] -0.5
```

``` r
r2 <- rpg[[2]] #residuals from second group
egeg2 <- r2 %*% t(r2) #quicker way to do this, orig a loop
(forg2 = sum(egeg2[lower.tri(egeg2)]))
```

```
[1] -0.5
```

``` r
(1/scalep) * (1/12) * (forg1 + forg2) #putting it all together
```

```
[1] -0.06666667
```

The 12 is based on the number of residuals we have multiplied and added together. In a 4 x 4 matrix, there are 6 pieces of unique information. Since had two groups, this is 12 in total.

Putting this together in a function:


``` r
geticc <- function(data, cluster, r){
  
  scalep <- mean(r^2) #dispersion parameter
  rpg <- split(r, data[,cluster]) #individual residuals
  nm <- names(table(data[,cluster])) #names of clusters
  coll <- numeric() #empty container
  
  ### p. 63 Hardin and Hilbe
  ### get ICC based on residuals per cluster
  ### right now, need the data to be sorted by cluster
  
  multresid <- function(x){
    r2 <- rpg[[x]] #extract resid per group (rpg)
    egeg <- r2 %*% t(r2) #e %*% t(e)
    coll[x] <- sum(egeg[lower.tri(egeg)]) #only lower diag
  }
  
  tst <- sum(sapply(nm, multresid)) #how many per group 
  ns <- sapply(rpg, length) #add up how many were added
  den <- sum((ns * (ns - 1 )) / 2) #how many products were added
  icc.model <- (tst / den) * (1 / scalep) #the icc
  return(icc.model)
  
}
```

Using the function:


``` r
geticc(dat, 'id', res)
```

```
[1] -0.06666667
```

Comparing the output using the `geeglm` function in the `geepack` package. $\alpha$ and $\rho$ are at times used interchangeably. 


``` r
library(geepack)
test <- geeglm(y ~ x, id = id, corstr = 'exchangeable', family = gaussian, data = dat)
summary(test)
```

```

Call:
geeglm(formula = y ~ x, family = gaussian, data = dat, id = id, 
    corstr = "exchangeable")

 Coefficients:
            Estimate Std.err Wald Pr(>|W|)    
(Intercept)   5.5000  0.3536  242   <2e-16 ***
x             1.0000  0.0000  Inf   <2e-16 ***
---
Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

Correlation structure = exchangeable 
Estimated Scale Parameters:

            Estimate   Std.err
(Intercept)     1.25 2.453e-17
  Link = identity 

Estimated Correlation Parameters:
      Estimate   Std.err
alpha -0.06667 8.378e-18
Number of clusters:   2  Maximum cluster size: 4 
```

``` r
test$geese$alpha #this is the common correlation;
```

```
   alpha 
-0.06667 
```

## Example with a homemade GEE function

This syntax is not made for speed, but for transparency (i.e., hopefully, the steps can be seen).


``` r
# HOMEGROWN GEE FUNCTION using iterative reweighted least squares (IRLS)
jebsgee <- function(fml, data, cluster, corstr = 'independence'){
  ## extract data
  tmp <- cbind(data, cluster = data[,cluster]) #dataframe with cluster
  tmp <- tmp[order(tmp$cluster), ] #sorting by cluster
  fml <- formula(fml)
  df <- model.frame(fml, tmp)
  X <- model.matrix(fml, df)
  y <- model.response(df)

  if(sum(is.na(df)) > 0) (stop("You have missing data."))
  
  gpsz <- table(data[, cluster]) #how many per group; group size
  NG <- length(gpsz) #how many groups
  maxsize <- max(gpsz) #what's the biggest group size
  
  CS <- c('independence', 'exchangeable')
  cs <- pmatch(corstr, CS, -1) #allow to match corstr by keywords
  if (cs == -1) (stop("Currently can only use an independence or exchangeable correlation structure"))
  
  corstr <- CS[cs] #put in the whole word
  
  # STEP #1
  firstrun <- glm(formula(fml), data = df) #just a regular glm
  
  # STEP #2
  r <- resid(firstrun, 'pearson') #get residuals
  betas <- coef(firstrun) #get initial coefficients
  
  if (corstr == 'exchangeable') {

  ### setup iterations need for exchangeable structure
    dev <- 0
    delta.dev <- 1
    tol <- 1e-5 #can make this bigger or smaller
    maxiter <- 50 #number of iterations, can make this bigger
    i = 1 #starting at iteration 1
    
    cat("Iteration: ") 
    while(abs(delta.dev > tol & i < maxiter)){ #when change in deviance is small, stop
      cat(i, "::")
      
      # after iteration, this is STEP #5
      r <- y - X %*% betas #residuals / use Pearson if non-identity link
      
      icc <- geticc(data = tmp, 'cluster', r) #compute new iccs #going back to step #2
      results <- exchR(icc, maxsize = maxsize, y = y, NG, gpsz) #compute new weight matrix STEP3
      vm2 <- results$vm2
      # STEP #4
      betas <- solve(t(X) %*% vm2 %*% X) %*% t(X) %*% vm2 %*% y #update betas
      dev0 <- dev #get prior dev
      dev <- sum((y - X %*% betas)^2) #new deviance
      delta.dev <- dev - dev0 #change in deviance
      i = i + 1 #add one to the iteration
    }
    
    cat("\nFinal alpha:", icc, "\n")
  }
  
  ### STEP #7: computing Liang and Zeger SEs
  re <- as.numeric(y - X %*% betas) #get residuals
  k <- ncol(X) #how many predictors (including intercept)
  cdata <- data.frame(cluster = tmp$cluster, r = re) #data with cluster and residuals
  gs <- names(table(cdata$cluster)) #names of the clusters

  u <- matrix(NA, nrow = NG, ncol = k) #empty matrix
  gpsv <- tmp$cluster
  
  if (corstr == 'independence') wcv <- vm2 <- diag(nrow(X)) #if independence
  if (corstr == 'exchangeable') wcv <- results$wcv
  
  for(i in 1:NG){
      tmp <- nrow((df[gpsv == gs[i], ]))
      u[i,] <- t(cdata$r[cdata$cluster == gs[i]]) %*% 
        solve(wcv[1:tmp, 1:tmp]) %*% X[gpsv == gs[i], 1:k]
  }
  
  mt <- crossprod(u) #t(u) %*% u :: meat
  br <- solve(t(X) %*% vm2 %*% X) #bread matrix
  clvc <- br %*% mt %*% br #LZ robust vcov matrix
  
  ### putting it all together
  
  se <- as.numeric(sqrt(diag(clvc))) #standard error
  b <- as.numeric(betas) #betas
  wald <- (b / se)^2 #wald
  pv <- pchisq(wald, 1, lower.tail = F) #p value
  stars <- cut(pv, breaks = c(0, 0.001, 0.01, 0.05, 0.1, 1), 
               labels = c("***", "**", "*  ", ".  ", " "), 
               include.lowest = TRUE)
  
  res <- data.frame(estimates = b, se, wald, pv = round(pv, 4), s = stars)
  row.names(res) <- colnames(X) #getting the names of the coefficients
  
  cat("Working correlation structure:", corstr, "\n")
  print(res) #output results
}

## only for creating an exchangeable R matrix
exchR <- function(icc, maxsize, y, NG, gpsz, ...){
  wr1 <- matrix(icc, nrow = maxsize, ncol = maxsize)
  diag(wr1) <- 1
  ## converting to a covariance matrix
  wcv <- wr1 * var(y) #save it, used when getting the RSE
  wcl <- list() #empty list
  for (i in 1:NG){ #making several covariance matrices
    GS <- gpsz[i]  #depending on how many units per cluster
    tmp <- wcv[1:GS, 1:GS]
    wcl[[i]] <- tmp
  }
  vm2 <- solve(Matrix::bdiag(wcl)) #create block diagonal (this is V^-1)
  return(list(vm2 = vm2, wcv = wcv)) #return the inverse of the variance matrix
}
```

Just using the commonly-used High School and Beyond dataset.


``` r
library(mlmRev)
data(Hsb82)
summary(geeglm(mAch ~ sector + meanses + cses + sector * cses, 
        id = school, 
        corstr = 'ex', 
        data = Hsb82)) #using an existing package
```

```

Call:
geeglm(formula = mAch ~ sector + meanses + cses + sector * cses, 
    data = Hsb82, id = school, corstr = "ex")

 Coefficients:
                    Estimate Std.err   Wald Pr(>|W|)    
(Intercept)           12.128   0.174 4878.1  < 2e-16 ***
sectorCatholic         1.225   0.308   15.8  6.9e-05 ***
meanses                5.333   0.334  254.3  < 2e-16 ***
cses                   2.782   0.159  307.1  < 2e-16 ***
sectorCatholic:cses   -1.349   0.233   33.5  6.9e-09 ***
---
Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

Correlation structure = exchangeable 
Estimated Scale Parameters:

            Estimate Std.err
(Intercept)     39.1   0.671
  Link = identity 

Estimated Correlation Parameters:
      Estimate Std.err
alpha   0.0558  0.0099
Number of clusters:   160  Maximum cluster size: 67 
```

Using my function:


``` r
jebsgee(mAch ~ sector + meanses + cses + sector * cses, 
        data = Hsb82, 
        cluster = 'school',
        corstr = 'ex') #using my function
```

```
Iteration: 1 ::2 ::3 ::4 ::
Final alpha: 0.0558 
Working correlation structure: exchangeable 
                    estimates    se   wald    pv   s
(Intercept)             12.13 0.174 4878.1 0e+00 ***
sectorCatholic           1.23 0.308   15.8 1e-04 ***
meanses                  5.33 0.334  254.3 0e+00 ***
cses                     2.78 0.159  307.1 0e+00 ***
sectorCatholic:cses     -1.35 0.233   33.6 0e+00 ***
```

Results are the same. 

Here are the results if an independence working correlation matrix is used:


``` r
summary(geeglm(mAch ~ sector + meanses + cses + sector * cses, 
        id = school, 
        corstr = 'ind', 
        data = Hsb82)) #using an existing package
```

```

Call:
geeglm(formula = mAch ~ sector + meanses + cses + sector * cses, 
    data = Hsb82, id = school, corstr = "ind")

 Coefficients:
                    Estimate Std.err   Wald Pr(>|W|)    
(Intercept)           12.116   0.170 5087.1  < 2e-16 ***
sectorCatholic         1.280   0.299   18.3  1.9e-05 ***
meanses                5.164   0.334  239.0  < 2e-16 ***
cses                   2.782   0.159  307.1  < 2e-16 ***
sectorCatholic:cses   -1.349   0.233   33.5  6.9e-09 ***
---
Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

Correlation structure = independence 
Estimated Scale Parameters:

            Estimate Std.err
(Intercept)     39.1   0.671
Number of clusters:   160  Maximum cluster size: 67 
```

``` r
jebsgee(mAch ~ sector + meanses + cses + sector * cses, 
        data = Hsb82, 
        cluster = 'school',
        corstr = 'ind') #using my function
```

```
Working correlation structure: independence 
                    estimates    se   wald pv   s
(Intercept)             12.12 0.170 5087.1  0 ***
sectorCatholic           1.28 0.299   18.3  0 ***
meanses                  5.16 0.334  239.0  0 ***
cses                     2.78 0.159  307.1  0 ***
sectorCatholic:cses     -1.35 0.233   33.6  0 ***
```

Testing using the small `mtcars` dataset in R:


``` r
mtcars2 <- mtcars[order(mtcars$cyl), ] #needs to be sorted for geeglm
jebsgee(mpg ~ wt + am + qsec + hp + vs, 
        data = mtcars, 
        cluster = 'cyl', 
        corstr = 'ex')
```

```
Iteration: 1 ::2 ::3 ::4 ::5 ::6 ::7 ::8 ::9 ::10 ::11 ::
Final alpha: -0.0284 
Working correlation structure: exchangeable 
            estimates      se   wald     pv   s
(Intercept)    17.122 4.91011 12.160 0.0005 ***
wt             -3.260 0.70955 21.105 0.0000 ***
am              2.929 0.40431 52.496 0.0000 ***
qsec            0.817 0.20428 15.999 0.0001 ***
hp             -0.016 0.00962  2.753 0.0971 .  
vs              0.174 0.32731  0.282 0.5956    
```

``` r
summary(geeglm(mpg ~ wt + am + qsec + hp + vs, 
        id = cyl, 
        corstr = 'ex', 
        data = mtcars2)
)
```

```

Call:
geeglm(formula = mpg ~ wt + am + qsec + hp + vs, data = mtcars2, 
    id = cyl, corstr = "ex")

 Coefficients:
            Estimate  Std.err  Wald Pr(>|W|)    
(Intercept) 17.12208  4.91011 12.16  0.00049 ***
wt          -3.25966  0.70954 21.10  4.3e-06 ***
am           2.92939  0.40431 52.50  4.3e-13 ***
qsec         0.81711  0.20428 16.00  6.3e-05 ***
hp          -0.01596  0.00962  2.75  0.09710 .  
vs           0.17373  0.32731  0.28  0.59558    
---
Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

Correlation structure = exchangeable 
Estimated Scale Parameters:

            Estimate Std.err
(Intercept)        5    1.29
  Link = identity 

Estimated Correlation Parameters:
      Estimate Std.err
alpha  -0.0284  0.0775
Number of clusters:   3  Maximum cluster size: 14 
```

## References

Bauer, D. J., & Sterba, S. K. (2011). Fitting multilevel models with ordinal outcomes: Performance of alternative specifications and methods of estimation. *Psychological Methods, 16*(4), 373–390. https://doi.org/10.1037/a0025813

Crespi, C. M., Wong, W. K., & Wu, S. (2011). A new dependence parameter approach to improve the design of cluster randomized trials with binary outcomes. *Clinical Trials: Journal of the Society for Clinical Trials, 8*(6), 687–698. https://doi.org/10.1177/1740774511423851

Ghisletta, P., & Spini, D. (2004). An introduction to generalized estimating equations and an application to assess selectivity effects in a longitudinal study on very old individuals. *Journal of Educational and Behavioral Statistics, 29*(4), 421-437.

Hardin, J., & Hilbe, J. (2013). *Generalized estimating equations (2nd ed.)*. CRC Press.

Liang, K.-Y., & Zeger, S. L. (1986). Longitudinal data analysis using generalized linear models. *Biometrika, 73*(1), 13–22.

Singer, J. D. (1998). Using SAS PROC MIXED to fit multilevel models, hierarchical models, and individual growth models. *Journal of Educational and Behavioral Statistics, 23*(4), 323-355.

Wu, S., Crespi, C. M., & Wong, W. K. (2012). Comparison of methods for estimating the intraclass correlation coefficient for binary responses in cancer prevention cluster randomized trials. *Contemporary Clinical Trials, 33*(5), 869–880.

-- END
