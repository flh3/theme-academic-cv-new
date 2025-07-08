---
title: Note on Robust Standard Errors
author: Francis Huang
date: '2018-11-04'
slug: note-on-robust-standard-errors
categories:
  - robust standard errors
tags:
  - standard errors
header:
  caption: ''
  image: ''
---


Illustration showing different flavors of robust standard errors. Load
in library, dataset, and recode. Do not really need to dummy code but
may make making the **X** matrix easier. Using the High School & Beyond
(hsb) dataset.

    library(mlmRev) #has the hsb dataset
    library(summarytools) #for descriptives
    library(jtools) #for output
    library(dplyr) #for pipes and selecting
    library(sandwich) #robust SEs
    library(lmtest) #for coeftest

    hsb <- Hsb82
    names(hsb) <- tolower(names(hsb))
    dim(hsb)

    ## [1] 7185    8

    head(hsb)

    ##   school minrty     sx    ses   mach   meanses sector        cses
    ## 1   1224     No Female -1.528  5.876 -0.434383 Public -1.09361702
    ## 2   1224     No Female -0.588 19.708 -0.434383 Public -0.15361702
    ## 3   1224     No   Male -0.528 20.349 -0.434383 Public -0.09361702
    ## 4   1224     No   Male -0.668  8.781 -0.434383 Public -0.23361702
    ## 5   1224     No   Male -0.158 17.898 -0.434383 Public  0.27638298
    ## 6   1224     No   Male  0.022  4.583 -0.434383 Public  0.45638298

    hsb$private <- ifelse(hsb$sector == "Public", 0, 1)
    hsb$female <- ifelse(hsb$sx == "Male", 0, 1)

    freq(hsb$private)

    ## Frequencies  
    ## hsb$private  
    ## Type: Numeric  
    ## 
    ##               Freq   % Valid   % Valid Cum.   % Total   % Total Cum.
    ## ----------- ------ --------- -------------- --------- --------------
    ##           0   3642     50.69          50.69     50.69          50.69
    ##           1   3543     49.31         100.00     49.31         100.00
    ##        <NA>      0                               0.00         100.00
    ##       Total   7185    100.00         100.00    100.00         100.00

Run an OLS regression predicting math achievement. NOTE: private is a
level 2, school-level variable.

    m1 <- lm(mach ~ ses + female + private, data = hsb)
    summ(m1, digits = 3)

<table class="table table-striped table-hover table-condensed table-responsive" style="color: black; width: auto !important; margin-left: auto; margin-right: auto;">
<tbody>
<tr>
<td style="text-align:left;font-weight: bold;">
Observations
</td>
<td style="text-align:right;">
7185
</td>
</tr>
<tr>
<td style="text-align:left;font-weight: bold;">
Dependent variable
</td>
<td style="text-align:right;">
mach
</td>
</tr>
<tr>
<td style="text-align:left;font-weight: bold;">
Type
</td>
<td style="text-align:right;">
OLS linear regression
</td>
</tr>
</tbody>
</table>
<table class="table table-striped table-hover table-condensed table-responsive" style="color: black; width: auto !important; margin-left: auto; margin-right: auto;">
<tbody>
<tr>
<td style="text-align:left;font-weight: bold;">
F(3,7181)
</td>
<td style="text-align:right;">
454.392
</td>
</tr>
<tr>
<td style="text-align:left;font-weight: bold;">
R²
</td>
<td style="text-align:right;">
0.160
</td>
</tr>
<tr>
<td style="text-align:left;font-weight: bold;">
Adj. R²
</td>
<td style="text-align:right;">
0.159
</td>
</tr>
</tbody>
</table>
<table class="table table-striped table-hover table-condensed table-responsive" style="color: black; width: auto !important; margin-left: auto; margin-right: auto;border-bottom: 0;">
<thead>
<tr>
<th style="text-align:left;">
</th>
<th style="text-align:right;">
Est.
</th>
<th style="text-align:right;">
S.E.
</th>
<th style="text-align:right;">
t val.
</th>
<th style="text-align:right;">
p
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:left;font-weight: bold;">
(Intercept)
</td>
<td style="text-align:right;">
12.521
</td>
<td style="text-align:right;">
0.131
</td>
<td style="text-align:right;">
95.691
</td>
<td style="text-align:right;">
0.000
</td>
</tr>
<tr>
<td style="text-align:left;font-weight: bold;">
ses
</td>
<td style="text-align:right;">
2.884
</td>
<td style="text-align:right;">
0.097
</td>
<td style="text-align:right;">
29.586
</td>
<td style="text-align:right;">
0.000
</td>
</tr>
<tr>
<td style="text-align:left;font-weight: bold;">
female
</td>
<td style="text-align:right;">
-1.404
</td>
<td style="text-align:right;">
0.149
</td>
<td style="text-align:right;">
-9.393
</td>
<td style="text-align:right;">
0.000
</td>
</tr>
<tr>
<td style="text-align:left;font-weight: bold;">
private
</td>
<td style="text-align:right;">
1.963
</td>
<td style="text-align:right;">
0.152
</td>
<td style="text-align:right;">
12.949
</td>
<td style="text-align:right;">
0.000
</td>
</tr>
</tbody>
<tfoot>
<tr>
<td style="padding: 0; " colspan="100%">
<sup></sup> Standard errors: OLS
</td>
</tr>
</tfoot>
</table>

    ###

Reproduce results using matrix algebra:

*B* = (*X*<sup>*T*</sup>*X*)<sup>−1</sup>*X*<sup>*T*</sup>*Y*

    X <- model.matrix(m1)
    head(X)

    ##   (Intercept)    ses female private
    ## 1           1 -1.528      1       0
    ## 2           1 -0.588      1       0
    ## 3           1 -0.528      0       0
    ## 4           1 -0.668      0       0
    ## 5           1 -0.158      0       0
    ## 6           1  0.022      0       0

    #regression coefficients
    ###
    Bs <- solve(t(X) %*% X) %*% t(X) %*% hsb$mach
    Bs

    ##                  [,1]
    ## (Intercept) 12.520715
    ## ses          2.884130
    ## female      -1.403538
    ## private      1.963150

Now for the standard errors: method 1

$\sigma^2 = \frac{u^Tu}{n - k - 1}$

where *u* is the residual (or *y* − *X**B*).

    u <- resid(m1)
    num <- t(u) %*% u #numerator
    den <- nobs(m1) - m1$rank #denominator
    (num/den) %>% sqrt #residual standard error (manually)

    ##          [,1]
    ## [1,] 6.307042

    summary(m1)$sigma #residual standard error (computed using lm)

    ## [1] 6.307042

    var2 <- as.numeric(num/den) #from matrix, convert to numeric

To generate the variance covariance matrix:
*V**a**r*(*B*) = *σ*<sup>2</sup>(*X*<sup>*T*</sup>*X*)<sup>−1</sup>. The
square root of the diagonal are the standard errors.

    vce <- var2 * solve(t(X) %*% X)
    ses <- sqrt(diag(vce)) #standard errors

    cbind(B = Bs, ses, t = round(Bs/ses, 3))

    ##                              ses       
    ## (Intercept) 12.520715 0.13084584 95.691
    ## ses          2.884130 0.09748345 29.586
    ## female      -1.403538 0.14942396 -9.393
    ## private      1.963150 0.15160531 12.949

    summary(m1)

    ## 
    ## Call:
    ## lm(formula = mach ~ ses + female + private, data = hsb)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -19.9037  -4.6864   0.2104   4.8645  17.1868 
    ## 
    ## Coefficients:
    ##             Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept) 12.52071    0.13085  95.691   <2e-16 ***
    ## ses          2.88413    0.09748  29.586   <2e-16 ***
    ## female      -1.40354    0.14942  -9.393   <2e-16 ***
    ## private      1.96315    0.15161  12.949   <2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 6.307 on 7181 degrees of freedom
    ## Multiple R-squared:  0.1595, Adjusted R-squared:  0.1592 
    ## F-statistic: 454.4 on 3 and 7181 DF,  p-value: < 2.2e-16

Doing it using a general form using Method 2:

(*X*<sup>*T*</sup>*X*)<sup>−1</sup>*X*<sup>*T*</sup>*u*<sup>*T*</sup>*u**X*(*X*<sup>*T*</sup>*X*)<sup>−1</sup>.
In the general case, *u*<sup>*T*</sup>*u* is a *n* × *n* diagonal with a
constant *σ*<sup>2</sup> on the diagonal.

    ######### correct::: doing this using a general form::: METHOD 2
    ### bread * meat * bread

    #constant variance on the diagonal
    n <- nobs(m1)
    mm <- diag(var2, nrow = n) #creating a matrix with the constant variance on the diagonal
    mm[1:5, 1:5] #just taking a peek at the first five rows/columns

    ##          [,1]     [,2]     [,3]     [,4]     [,5]
    ## [1,] 39.77878  0.00000  0.00000  0.00000  0.00000
    ## [2,]  0.00000 39.77878  0.00000  0.00000  0.00000
    ## [3,]  0.00000  0.00000 39.77878  0.00000  0.00000
    ## [4,]  0.00000  0.00000  0.00000 39.77878  0.00000
    ## [5,]  0.00000  0.00000  0.00000  0.00000 39.77878

    mt <- t(X) %*% mm %*% X #the meat
    br <- solve(crossprod(X)) #this is the same as solve(t(X) %*% X)
    (vce3 <- br %*% mt %*% br)

    ##               (Intercept)           ses        female       private
    ## (Intercept)  0.0171206345  0.0008451577 -0.0115724586 -0.0110969152
    ## ses          0.0008451577  0.0095030234  0.0010249165 -0.0028145087
    ## female      -0.0115724586  0.0010249165  0.0223275203 -0.0004476095
    ## private     -0.0110969152 -0.0028145087 -0.0004476095  0.0229841695

    vce

    ##               (Intercept)           ses        female       private
    ## (Intercept)  0.0171206345  0.0008451577 -0.0115724586 -0.0110969152
    ## ses          0.0008451577  0.0095030234  0.0010249165 -0.0028145087
    ## female      -0.0115724586  0.0010249165  0.0223275203 -0.0004476095
    ## private     -0.0110969152 -0.0028145087 -0.0004476095  0.0229841695

    vcov(m1) #all the same :: this is automatically computed using the vcov function

    ##               (Intercept)           ses        female       private
    ## (Intercept)  0.0171206345  0.0008451577 -0.0115724586 -0.0110969152
    ## ses          0.0008451577  0.0095030234  0.0010249165 -0.0028145087
    ## female      -0.0115724586  0.0010249165  0.0223275203 -0.0004476095
    ## private     -0.0110969152 -0.0028145087 -0.0004476095  0.0229841695

In the case where homoskedasticity is not a reasonable assumption, we
can create a heteroskedasticity-consistent covariance matrix (HCCM).

    ########### Heteroscedasticity Consistent SEs

    u <- resid(m1) #just recreating
    uu <- diag(u %*% t(u)) %>% diag #get the diagonal, then make a diagonal matrix
    uu[1:5, 1:5] #taking a peek, differing variances on the diagonal

    ##           [,1]    [,2]     [,3]     [,4]     [,5]
    ## [1,] 0.6959333   0.000  0.00000 0.000000  0.00000
    ## [2,] 0.0000000 105.816  0.00000 0.000000  0.00000
    ## [3,] 0.0000000   0.000 87.44319 0.000000  0.00000
    ## [4,] 0.0000000   0.000  0.00000 3.287388  0.00000
    ## [5,] 0.0000000   0.000  0.00000 0.000000 34.02363

Unlike the first matrix with constant variance, the variance is allowed
to vary per observation. That new matrix is used in the meat matrix.

    mt <- t(X) %*% uu %*% X
    br <- solve(crossprod(X))
    (vce4 <- br %*% mt %*% br) * (7185 / 7181) # n / n- k--> uses an adjustment too

    ##             (Intercept)          ses       female      private
    ## (Intercept)  0.01941891  0.001236800 -0.012945247 -0.012716403
    ## ses          0.00123680  0.008964747  0.001322174 -0.003971380
    ## female      -0.01294525  0.001322174  0.022554135  0.000554395
    ## private     -0.01271640 -0.003971380  0.000554395  0.023658248

    vcovHC(m1, type = 'HC1') #same, automated using the sandwich package

    ##             (Intercept)          ses       female      private
    ## (Intercept)  0.01941891  0.001236800 -0.012945247 -0.012716403
    ## ses          0.00123680  0.008964747  0.001322174 -0.003971380
    ## female      -0.01294525  0.001322174  0.022554135  0.000554395
    ## private     -0.01271640 -0.003971380  0.000554395  0.023658248

There are different types of robust standard errors. What was shown was
using what was called ‘HC1’. The simplest version is HC0 with no degrees
of freedom adjustment.

    (vc.hc0 <- br %*% mt %*% br) #simplest version, no df adjustment

    ##              (Intercept)          ses        female       private
    ## (Intercept)  0.019408094  0.001236111 -0.0129380402 -0.0127093240
    ## ses          0.001236111  0.008959756  0.0013214379 -0.0039691692
    ## female      -0.012938040  0.001321438  0.0225415788  0.0005540864
    ## private     -0.012709324 -0.003969169  0.0005540864  0.0236450776

    vcovHC(m1, type = 'HC0') #identical

    ##              (Intercept)          ses        female       private
    ## (Intercept)  0.019408094  0.001236111 -0.0129380402 -0.0127093240
    ## ses          0.001236111  0.008959756  0.0013214379 -0.0039691692
    ## female      -0.012938040  0.001321438  0.0225415788  0.0005540864
    ## private     -0.012709324 -0.003969169  0.0005540864  0.0236450776

Other types, HC2 uses the hat values *X*(*X*′*X*)<sup>−1</sup>*X*′.

    ht <- diag(X %*% solve((t(X) %*% X)) %*% t(X))
    tmp <- hatvalues(m1) #same, just comparing
    #compare
    ht[1:10]

    ##            1            2            3            4            5            6            7 
    ## 0.0008239518 0.0004371588 0.0004745605 0.0005086124 0.0004296461 0.0004314467 0.0004429814 
    ##            8            9           10 
    ## 0.0006259305 0.0005147352 0.0004610464

    tmp[1:10]

    ##            1            2            3            4            5            6            7 
    ## 0.0008239518 0.0004371588 0.0004745605 0.0005086124 0.0004296461 0.0004314467 0.0004429814 
    ##            8            9           10 
    ## 0.0006259305 0.0005147352 0.0004610464

    ### for HC2, divided by 1-ht
    newu <- u^2/(1-ht)    
    newu[1:6]

    ##           1           2           3           4           5           6 
    ##   0.6965072 105.8623008  87.4847037   3.2890610  34.0382573  64.0462798

    dd <- diag(newu, nrow = n)
    (vcov.HC2 <- br %*% (t(X) %*% dd %*% X) %*% br)

    ##              (Intercept)          ses        female       private
    ## (Intercept)  0.019419000  0.001236993 -0.0129453531 -0.0127163888
    ## ses          0.001236993  0.008966698  0.0013225142 -0.0039723682
    ## female      -0.012945353  0.001322514  0.0225540732  0.0005542377
    ## private     -0.012716389 -0.003972368  0.0005542377  0.0236585058

    vcovHC(m1, type = 'HC2') #same

    ##              (Intercept)          ses        female       private
    ## (Intercept)  0.019419000  0.001236993 -0.0129453531 -0.0127163888
    ## ses          0.001236993  0.008966698  0.0013225142 -0.0039723682
    ## female      -0.012945353  0.001322514  0.0225540732  0.0005542377
    ## private     -0.012716389 -0.003972368  0.0005542377  0.0236585058

The preferred version, which is also the default of the sandwich
function, is HC3 where diagonal is: $\frac{u^{2}}{(1 - h)^2}$.

    ### for HC3, divided by (1-ht)^2
    newu <- u^2/(1-ht)^2    
    dd <- diag(newu, nrow = n)
    (vcov.HC3 <- br %*% (t(X) %*% dd %*% X) %*% br)

    ##              (Intercept)          ses       female      private
    ## (Intercept)  0.019429912  0.001237875 -0.012952671 -0.012723458
    ## ses          0.001237875  0.008973645  0.001323592 -0.003975570
    ## female      -0.012952671  0.001323592  0.022566575  0.000554389
    ## private     -0.012723458 -0.003975570  0.000554389  0.023671943

    vcovHC(m1, type = 'HC3') #same

    ##              (Intercept)          ses       female      private
    ## (Intercept)  0.019429912  0.001237875 -0.012952671 -0.012723458
    ## ses          0.001237875  0.008973645  0.001323592 -0.003975570
    ## female      -0.012952671  0.001323592  0.022566575  0.000554389
    ## private     -0.012723458 -0.003975570  0.000554389  0.023671943

These are shown on p. 4 of this sandwich package manual
<https://cran.r-project.org/web/packages/sandwich/vignettes/sandwich.pdf>.

## Cluster Robust

Now, another flavor is to have a cluster robust variance covariance
matrix.

Cluster
VCV(*B̂* = (*X*<sup>*T*</sup>*X*)<sup>−1</sup>*Ω̂*(*X*<sup>*T*</sup>*X*)<sup>−1</sup>
where

$\hat{\Omega} = \sum\_{g=1}^G  X^T\_g \hat{u}\_g \hat{u}\_g^T   X\_g$

    head(hsb)

    ##   school minrty     sx    ses   mach   meanses sector        cses private female
    ## 1   1224     No Female -1.528  5.876 -0.434383 Public -1.09361702       0      1
    ## 2   1224     No Female -0.588 19.708 -0.434383 Public -0.15361702       0      1
    ## 3   1224     No   Male -0.528 20.349 -0.434383 Public -0.09361702       0      0
    ## 4   1224     No   Male -0.668  8.781 -0.434383 Public -0.23361702       0      0
    ## 5   1224     No   Male -0.158 17.898 -0.434383 Public  0.27638298       0      0
    ## 6   1224     No   Male  0.022  4.583 -0.434383 Public  0.45638298       0      0

    cdata <- data.frame(cluster = hsb$school, r = resid(m1))
    (m <- length(table(cdata$cluster))) #number of clusters

    ## [1] 160

    n <- nobs(m1)
    k <- m1$rank
    dfa <- (m/(m-1))  * ((n-1)/(n-k))
    gs <- names(table(cdata$cluster))
    u <- matrix(NA, nrow = m, ncol = k) #clusters x rank:: creating a new u matrix

The u matrix is now a *m* × *k* matrix. It is the transpose of the
residuals multiplied by the design matrix PER cluster.

    ### do this per cluster
    for(i in 1:m){
      u[i,] <- t(cdata$r[cdata$cluster == gs[i]]) %*% X[hsb$school == gs[i], 1:k]
    } #this is now a 160 x 4 matrix
    #4 x 160 multipled by 160 x 4 = 4 by 4 matrix

    (mt <- crossprod(u)) #which is the same as t(u) %*% u

    ##            [,1]      [,2]      [,3]     [,4]
    ## [1,] 1127414.04 -32276.29 524452.13 599712.7
    ## [2,]  -32276.29 261299.92 -42889.74  10432.7
    ## [3,]  524452.13 -42889.74 404206.13 249132.6
    ## [4,]  599712.74  10432.70 249132.57 599712.7

    (clvc <- br %*% mt %*% br * dfa) #same:: manually computed

    ##              (Intercept)          ses       female     private
    ## (Intercept)  0.055118621  0.003473873 -0.027093581 -0.03596669
    ## ses          0.003473873  0.015449763  0.001932605 -0.01272128
    ## female      -0.027093581  0.001932605  0.052099373 -0.01256699
    ## private     -0.035966689 -0.012721285 -0.012566991  0.09446861

    vcovCL(m1, cluster = hsb$school) #same:: using the sandwich package

    ##              (Intercept)          ses       female     private
    ## (Intercept)  0.055118621  0.003473873 -0.027093581 -0.03596669
    ## ses          0.003473873  0.015449763  0.001932605 -0.01272128
    ## female      -0.027093581  0.001932605  0.052099373 -0.01256699
    ## private     -0.035966689 -0.012721285 -0.012566991  0.09446861

    coeftest(m1, vcovCL(m1, cluster = hsb$school))

    ## 
    ## t test of coefficients:
    ## 
    ##             Estimate Std. Error t value  Pr(>|t|)    
    ## (Intercept) 12.52071    0.23477 53.3310 < 2.2e-16 ***
    ## ses          2.88413    0.12430 23.2035 < 2.2e-16 ***
    ## female      -1.40354    0.22825 -6.1490 8.213e-10 ***
    ## private      1.96315    0.30736  6.3872 1.795e-10 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

    coeftest(m1, clvc)

    ## 
    ## t test of coefficients:
    ## 
    ##             Estimate Std. Error t value  Pr(>|t|)    
    ## (Intercept) 12.52071    0.23477 53.3310 < 2.2e-16 ***
    ## ses          2.88413    0.12430 23.2035 < 2.2e-16 ***
    ## female      -1.40354    0.22825 -6.1490 8.213e-10 ***
    ## private      1.96315    0.30736  6.3872 1.795e-10 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

    summ(m1, cluster = 'school', robust= "HC1", digits = 5) #using jtools

<table class="table table-striped table-hover table-condensed table-responsive" style="color: black; width: auto !important; margin-left: auto; margin-right: auto;">
<tbody>
<tr>
<td style="text-align:left;font-weight: bold;">
Observations
</td>
<td style="text-align:right;">
7185
</td>
</tr>
<tr>
<td style="text-align:left;font-weight: bold;">
Dependent variable
</td>
<td style="text-align:right;">
mach
</td>
</tr>
<tr>
<td style="text-align:left;font-weight: bold;">
Type
</td>
<td style="text-align:right;">
OLS linear regression
</td>
</tr>
</tbody>
</table>
<table class="table table-striped table-hover table-condensed table-responsive" style="color: black; width: auto !important; margin-left: auto; margin-right: auto;">
<tbody>
<tr>
<td style="text-align:left;font-weight: bold;">
F(3,7181)
</td>
<td style="text-align:right;">
454.39243
</td>
</tr>
<tr>
<td style="text-align:left;font-weight: bold;">
R²
</td>
<td style="text-align:right;">
0.15954
</td>
</tr>
<tr>
<td style="text-align:left;font-weight: bold;">
Adj. R²
</td>
<td style="text-align:right;">
0.15919
</td>
</tr>
</tbody>
</table>
<table class="table table-striped table-hover table-condensed table-responsive" style="color: black; width: auto !important; margin-left: auto; margin-right: auto;border-bottom: 0;">
<thead>
<tr>
<th style="text-align:left;">
</th>
<th style="text-align:right;">
Est.
</th>
<th style="text-align:right;">
S.E.
</th>
<th style="text-align:right;">
t val.
</th>
<th style="text-align:right;">
p
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:left;font-weight: bold;">
(Intercept)
</td>
<td style="text-align:right;">
12.52071
</td>
<td style="text-align:right;">
0.23477
</td>
<td style="text-align:right;">
53.33103
</td>
<td style="text-align:right;">
0.00000
</td>
</tr>
<tr>
<td style="text-align:left;font-weight: bold;">
ses
</td>
<td style="text-align:right;">
2.88413
</td>
<td style="text-align:right;">
0.12430
</td>
<td style="text-align:right;">
23.20352
</td>
<td style="text-align:right;">
0.00000
</td>
</tr>
<tr>
<td style="text-align:left;font-weight: bold;">
female
</td>
<td style="text-align:right;">
-1.40354
</td>
<td style="text-align:right;">
0.22825
</td>
<td style="text-align:right;">
-6.14905
</td>
<td style="text-align:right;">
0.00000
</td>
</tr>
<tr>
<td style="text-align:left;font-weight: bold;">
private
</td>
<td style="text-align:right;">
1.96315
</td>
<td style="text-align:right;">
0.30736
</td>
<td style="text-align:right;">
6.38719
</td>
<td style="text-align:right;">
0.00000
</td>
</tr>
</tbody>
<tfoot>
<tr>
<td style="padding: 0; " colspan="100%">
<sup></sup> Standard errors: Cluster-robust, type = HC1
</td>
</tr>
</tfoot>
</table>

Compare the standard errors of the cluster robust version with the
standard version below for the private coefficient (school level). This
is .15 vs .30.

    summ(m1)

<table class="table table-striped table-hover table-condensed table-responsive" style="color: black; width: auto !important; margin-left: auto; margin-right: auto;">
<tbody>
<tr>
<td style="text-align:left;font-weight: bold;">
Observations
</td>
<td style="text-align:right;">
7185
</td>
</tr>
<tr>
<td style="text-align:left;font-weight: bold;">
Dependent variable
</td>
<td style="text-align:right;">
mach
</td>
</tr>
<tr>
<td style="text-align:left;font-weight: bold;">
Type
</td>
<td style="text-align:right;">
OLS linear regression
</td>
</tr>
</tbody>
</table>
<table class="table table-striped table-hover table-condensed table-responsive" style="color: black; width: auto !important; margin-left: auto; margin-right: auto;">
<tbody>
<tr>
<td style="text-align:left;font-weight: bold;">
F(3,7181)
</td>
<td style="text-align:right;">
454.39
</td>
</tr>
<tr>
<td style="text-align:left;font-weight: bold;">
R²
</td>
<td style="text-align:right;">
0.16
</td>
</tr>
<tr>
<td style="text-align:left;font-weight: bold;">
Adj. R²
</td>
<td style="text-align:right;">
0.16
</td>
</tr>
</tbody>
</table>
<table class="table table-striped table-hover table-condensed table-responsive" style="color: black; width: auto !important; margin-left: auto; margin-right: auto;border-bottom: 0;">
<thead>
<tr>
<th style="text-align:left;">
</th>
<th style="text-align:right;">
Est.
</th>
<th style="text-align:right;">
S.E.
</th>
<th style="text-align:right;">
t val.
</th>
<th style="text-align:right;">
p
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:left;font-weight: bold;">
(Intercept)
</td>
<td style="text-align:right;">
12.52
</td>
<td style="text-align:right;">
0.13
</td>
<td style="text-align:right;">
95.69
</td>
<td style="text-align:right;">
0.00
</td>
</tr>
<tr>
<td style="text-align:left;font-weight: bold;">
ses
</td>
<td style="text-align:right;">
2.88
</td>
<td style="text-align:right;">
0.10
</td>
<td style="text-align:right;">
29.59
</td>
<td style="text-align:right;">
0.00
</td>
</tr>
<tr>
<td style="text-align:left;font-weight: bold;">
female
</td>
<td style="text-align:right;">
-1.40
</td>
<td style="text-align:right;">
0.15
</td>
<td style="text-align:right;">
-9.39
</td>
<td style="text-align:right;">
0.00
</td>
</tr>
<tr>
<td style="text-align:left;font-weight: bold;">
private
</td>
<td style="text-align:right;">
1.96
</td>
<td style="text-align:right;">
0.15
</td>
<td style="text-align:right;">
12.95
</td>
<td style="text-align:right;">
0.00
</td>
</tr>
</tbody>
<tfoot>
<tr>
<td style="padding: 0; " colspan="100%">
<sup></sup> Standard errors: OLS
</td>
</tr>
</tfoot>
</table>

What about multiway clustering? Cameron, Gelbach, and Miller (2011)
provide a simple way of computing the multiway vcv matrix. Given two
sets of clusters (e.g., firm by year clusters), compute the vcv using
firm as the cluster (A). Then compute the vcv with year as the cluster
(B) and the vcv with an interaction of firm x year as the cluster (C).
Finally, the multiway vcv is A + B - C.

\*\* multiwaycov \*\* is not available anymore for some reason?

    # library(multiwayvcov)
    # #data(package = 'multiwayvcov')
    # data("petersen") #this is in the multiwayvcov package-- this has been deprecated already in favor of the sandwich package
    # dim(petersen)
    # summary(petersen)
    # length(table(petersen$firmid))
    # length(table(petersen$year))
    # 
    # head(petersen)
    # test <- petersen
    # 
    # library(ggplot2)
    # ggplot(petersen, aes(x = year, y = y, group = firmid)) + geom_point(alpha = .05) + geom_line(alpha = .05)
    # 
    # fm <- lm(y ~ x, data=test)
    # 
    # summary(fm)
    # coeftest(fm, vcovCL(fm, cluster = test$firmid)) #A
    # coeftest(fm, vcovCL(fm, cluster = test$year)) #B
    # 
    # #doing this using multiway clustering
    # coeftest(fm, vcovCL(fm, cluster = test[,c('firmid','year')]))
    # #automatic
    # lfe::felm(y ~ x | 0 | 0 | year + firmid, data = test) %>% summary #same result
    # 
    # #### doing this multiway manually
    # 
    # #test$int <- paste0(test$firmid, test$year) #cross of firm and year
    # test$int <- interaction(test$firmid, test$year)
    # length(table(test$int))
    # 
    # M1 <- vcovCL(fm, cluster = test$firmid)
    # M2 <- vcovCL(fm, cluster = test$year)
    # M3 <- vcovCL(fm, cluster = test$int)
    # M1 + M2 - M3 #add first 2, subtract the third: p 336, Cameron
    # vcovCL(fm, cluster = test[,c('year','firmid')]) #same results!
