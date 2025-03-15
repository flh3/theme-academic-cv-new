---
title: Using Plausible Values with Multilevel Models Using R (update)
author: Francis Huang
date: '2024-06-08'
slug: pv3
categories:
  - wemix
  - plausible values 
tags:
  - large scale assessments
header:
  caption: '' 
  image: ''
  preview: yes
---




This is an update to: 

Huang, F. (2024). Using plausible values when fitting multilevel models with large-scale assessment data using R. [*Large-scale Assessments in Education.*](https://largescaleassessmentsineducation.springeropen.com/articles/10.1186/s40536-024-00192-0)

This is an update to `mixPV`, load it using this function:

``` r
source("https://raw.githubusercontent.com/flh3/pubdata/main/mixPV/mixPVv2.R")
```

The function has been updated to be able to use parallel processing or multiple cores of your computer (to make computation faster).

Load in the dataset.


``` r
data(pisa2012, package = 'MLMusingR') 
```

The usual `mixPV` function can be used as normal. This is done to establish the baseline time it takes to complete this:


``` r
st <- Sys.time()
m1a <- mixPV(pv1math + pv2math + pv3math + pv4math + pv5math ~ 
  st29q03 + sc14q02 + st04q01 + escs + (1|schoolid), 
  weights = c('w_fstuwt', 'w_fschwt'), 
  data = pisa2012)
```

```
Analyzing plausible value: pv1math 
Analyzing plausible value: pv2math 
Analyzing plausible value: pv3math 
Analyzing plausible value: pv4math 
Analyzing plausible value: pv5math 
```

``` r
Sys.time() - st
```

```
Time difference of 18.4217 secs
```

To turn on the use of multicores, add `mc = TRUE`:


``` r
st <- Sys.time()
m1 <- mixPV(pv1math + pv2math + pv3math + pv4math + pv5math ~
  st29q03 + sc14q02 + st04q01 + escs + (1|schoolid), 
  weights = c('w_fstuwt', 'w_fschwt'), 
  data = pisa2012, mc = TRUE)
```

```
Attempting to use 15 cores. Progress will not be displayed. Please wait.
```

``` r
Sys.time() - st
```

```
Time difference of 13.95518 secs
```

This is being done on a Chromebook (so there are only four cores). One core is left so the computer can still be used for other processes. Even with three cores, the time to complete is only a fraction of when using serial processing. 

By default, the degrees of freedom (dof) will be the Barnard Rubin dof.

``` r
summary(m1a)
```

```
Results of multilevel analyses with 5 plausible values.
Number of observations: 3136 

Estimates for random effects: 
                     estimate std.error statistic   df Pr(>t)    
schoolid.(Intercept)  1397.96    327.26      4.27 2289 <2e-16 ***
Residual              5295.23    158.43     33.42 1414 <2e-16 ***
---
Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

Estimates for fixed effects: 
                         estimate std.error statistic   df Pr(>t)    
(Intercept)                489.51      8.41     58.22  228 <2e-16 ***
st29q03Agree               -11.29      5.99     -1.88  445 0.0602 .  
st29q03Disagree            -19.53      6.05     -3.23  112 0.0016 ** 
st29q03Strongly disagree   -39.95      7.02     -5.69  299 <2e-16 ***
sc14q02Very little         -22.93     16.69     -1.37  601 0.1700    
sc14q02To some extent      -17.61     11.77     -1.50  190 0.1362    
sc14q02A lot               -30.05      7.75     -3.88  283 0.0001 ***
st04q01Male                  8.40      3.10      2.71  163 0.0075 ** 
escs                        25.88      2.11     12.29 1387 <2e-16 ***
---
Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

You can compare this to the old Rubin default dof:


``` r
summary(m1a, dfadj = FALSE)
```

```
Results of multilevel analyses with 5 plausible values.
Number of observations: 3136 

Estimates for random effects: 
                     estimate std.error statistic   df Pr(>t)    
schoolid.(Intercept)  1397.96    327.26      4.27 9027 <2e-16 ***
Residual              5295.23    158.43     33.42 2666 <2e-16 ***
---
Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

Estimates for fixed effects: 
                         estimate std.error statistic   df Pr(>t)    
(Intercept)                489.51      8.41     58.22  248 <2e-16 ***
st29q03Agree               -11.29      5.99     -1.88  527 0.0601 .  
st29q03Disagree            -19.53      6.05     -3.23  118 0.0016 ** 
st29q03Strongly disagree   -39.95      7.02     -5.69  335 <2e-16 ***
sc14q02Very little         -22.93     16.69     -1.37  759 0.1699    
sc14q02To some extent      -17.61     11.77     -1.50  204 0.1361    
sc14q02A lot               -30.05      7.75     -3.88  315 0.0001 ***
st04q01Male                  8.40      3.10      2.71  174 0.0075 ** 
escs                        25.88      2.11     12.29 2578 <2e-16 ***
---
Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```


--- END



