---
title: Instrumental variables within an SEM framework
author: Francis Huang
date: '2018-08-17'
categories:
  - instrumental variable
  - evaluation
  - SEM
  - causality
tags:
  - instrumental variable
  - R
  - causality
header:
  caption: ''
  image: ''
output: 
  html_document:
    keep_md: true
---



Earlier this year, I wrote an article on using [instrumental variables](https://openpublishing.library.umass.edu/pare/article/id/1661/) (IV) to analyze data from randomized experiments with imperfect compliance (read the manuscript for full details; link updated; it's open access). In the article, I described the steps of IV estimation and the logic behind it.

The sample code using two stage least squares regression (the correct analysis) is shown below (see article for specifics):


``` r
library(ivreg)
dat <- read.csv('https://raw.githubusercontent.com/flh3/pubdata/refs/heads/main/IV/ivexample.csv')
head(dat)
```

```
  assign takeup y
1      0      0 0
2      0      0 0
3      0      0 0
4      0      0 0
5      0      0 0
6      0      0 0
```

``` r
tail(dat)
```

```
    assign takeup  y
195      1      1  9
196      1      1 10
197      1      1 10
198      1      1 12
199      1      1 11
200      1      1  9
```

``` r
summary(dat)
```

```
     assign        takeup            y         
 Min.   :0.0   Min.   :0.000   Min.   : 0.000  
 1st Qu.:0.0   1st Qu.:0.000   1st Qu.: 0.000  
 Median :0.5   Median :0.000   Median : 0.000  
 Mean   :0.5   Mean   :0.435   Mean   : 4.375  
 3rd Qu.:1.0   3rd Qu.:1.000   3rd Qu.:10.000  
 Max.   :1.0   Max.   :1.000   Max.   :13.000  
```

``` r
iv1 <- ivreg(y ~ takeup, ~assign, data = dat)
summary(iv1) 
```

```

Call:
ivreg(formula = y ~ takeup | assign, data = dat)

Residuals:
      Min        1Q    Median        3Q       Max 
-3.065942 -0.065942  0.006522  0.006522  2.934058 

Coefficients:
             Estimate Std. Error t value Pr(>|t|)    
(Intercept) -0.006522   0.085095  -0.077    0.939    
takeup      10.072464   0.153269  65.718   <2e-16 ***

Diagnostic tests:
                 df1 df2 statistic p-value    
Weak instruments   1 198   185.933  <2e-16 ***
Wu-Hausman         1 197     0.018   0.892    
Sargan             0  NA        NA      NA    
---
Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

Residual standard error: 0.7478 on 198 degrees of freedom
Multiple R-Squared: 0.9782,	Adjusted R-squared: 0.9781 
Wald test:  4319 on 1 and 198 DF,  p-value: < 2.2e-16 
```

The treatment on treated (TOT) effect is **10.072 (SE = 0.153)**.

However, I indicated that:

> Although conceptually, the model is a full mediation
> model, the effect is not estimated using path analysis 
> or structural equation modeling (SEM) as is commonly
> done in education or psychology (i.e., the indirect path
> is not path a x path b). 

Using SEM, the results do not match.


``` r
library(lavaan)

#incorrect
t1 <- '
y ~ takeup
takeup ~ assign'

res1 <- lavaan::sem(model = t1, data = dat)
summary(res1)
```

```
lavaan 0.6-19 ended normally after 1 iteration

  Estimator                                         ML
  Optimization method                           NLMINB
  Number of model parameters                         4

  Number of observations                           200

Model Test User Model:
                                                      
  Test statistic                                 0.019
  Degrees of freedom                                 1
  P-value (Chi-square)                           0.891

Parameter Estimates:

  Standard errors                             Standard
  Information                                 Expected
  Information saturated (h1) model          Structured

Regressions:
                   Estimate  Std.Err  z-value  P(>|z|)
  y ~                                                 
    takeup           10.057    0.106   94.774    0.000
  takeup ~                                            
    assign            0.690    0.050   13.704    0.000

Variances:
                   Estimate  Std.Err  z-value  P(>|z|)
   .y                 0.554    0.055   10.000    0.000
   .takeup            0.127    0.013   10.000    0.000
```

Although the path model looks correct, the estimates are off. The TOT (IV) effect is **NOT** 0.69 $\times$ 10.057 which is 6.939 (vs. 10.072). The difference as well is  that $y$ is not regressed on the *actual/observed* takeup values but the *predicted* takeup values (read the article).

Later on, I came across this post by [Paul Allison](https://statisticalhorizons.com/iv-in-sem) who had a solution to get the correct estimate. The solution just involved correlating the error terms of the outcome and the actual takeup values.


``` r
### correct
t2 <- '
y ~ takeup
takeup ~ assign
y ~~ takeup'

res2 <- lavaan::sem(model = t2, data = dat)
summary(res2)
```

```
lavaan 0.6-19 ended normally after 13 iterations

  Estimator                                         ML
  Optimization method                           NLMINB
  Number of model parameters                         5

  Number of observations                           200

Model Test User Model:
                                                      
  Test statistic                                 0.000
  Degrees of freedom                                 0

Parameter Estimates:

  Standard errors                             Standard
  Information                                 Expected
  Information saturated (h1) model          Structured

Regressions:
                   Estimate  Std.Err  z-value  P(>|z|)
  y ~                                                 
    takeup           10.072    0.153   66.049    0.000
  takeup ~                                            
    assign            0.690    0.050   13.704    0.000

Covariances:
                   Estimate  Std.Err  z-value  P(>|z|)
 .y ~~                                                
   .takeup           -0.004    0.027   -0.137    0.891

Variances:
                   Estimate  Std.Err  z-value  P(>|z|)
   .y                 0.554    0.055    9.998    0.000
   .takeup            0.127    0.013   10.000    0.000
```
The point estimate and the standard errors of the takeup variable now match: **10.072 (SE = 0.153)**. However, note that this is still not the same as testing the indirect effect of path (a) $\times$ path(b).

- END
