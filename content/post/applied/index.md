---
title: 🎉 Applied example for alternatives to logistic regression
summary: 
date: 2019-10-27
authors:
  - admin
tags:
  - logistic regression
---

## Introduction

Logistic regression is often used to analyze experiments with binary
outcomes (e.g., pass vs fail) and binary predictors (e.g., treatment vs
control). Although appropriate, there are other possible models that can
be run that may provide easier to interpret results.

In addition, some of these models may be quicker to run. Some may say
that this point is moot given the availability of computing power today
but if you’ve ever tried to run a hierarchical generalized linear model
with a logit link function and a binary outcome, you know that when
using R (using `glmer` or `nlme`) this may take quite a long time (and
cross your fingers that you don’t have convergence issues).

The following code replicates the example (see the manuscript for
details) in the
[article](https://www.tandfonline.com/eprint/YS723ZYEIB2CPWKBEMPZ/full?target=10.1080/00220973.2019.1699769):

> Huang, F. (2019). Alternatives to logistic regression models with
> binary outcomes. Journal of Experimental Education. doi:
> 10.1080/00220973.2019.1699769

Data are based on the article of Huang and Cornell (2015). Using an
online survey, investigators tested for the presence of the
question-order effect. Based on a random number generated when the
students took the survey, they were placed in either the treatment (n =
1037) or control (n = 963) condition. Students in the treatment
condition were asked four specific types bullying questions (i.e.,
verbal, physical, social, cyber) and then were asked a general bullying
question (“I have been bullied in the past year”). Students in the
control condition were asked the general bullying question first and
then the specific bullying questions. We hypothesized that students who
were asked the specific bullying questions first would report overall
higher bullying vs the control group.

## 1. Examining cross tabs

Load in the required packages:

    library(dplyr) #just for the pipe, %>%
    library(logbin) #to run a log binomial model with a function
    library(summarytools) #for nicer crosstabs
    library(jtools) #for easier exp, confints, and adjusted standard errors

Load and examine the data frame:

    #specify the location on website
    dat <- url("https://github.com/flh3/pubdata/raw/refs/heads/main/JXE_Alt_Logistic/jxe.rdata") 
    load(dat) #load in data from the website
    summary(jxe) #name of the data.frame

         abully           tord            female          gl     
     Min.   :0.000   Min.   :0.0000   Min.   :0.0000   9th :446  
     1st Qu.:0.000   1st Qu.:0.0000   1st Qu.:0.0000   10th:530  
     Median :0.000   Median :1.0000   Median :1.0000   11th:517  
     Mean   :0.126   Mean   :0.5185   Mean   :0.5005   12th:507  
     3rd Qu.:0.000   3rd Qu.:1.0000   3rd Qu.:1.0000             
     Max.   :1.000   Max.   :1.0000   Max.   :1.0000             
     race    
     w:1278  
     b: 287  
     h: 170  
     a:  79  
     o: 186  
             

    head(jxe)

         abully tord female   gl race
    6910      0    0      1 10th    w
    8394      0    0      0  9th    w
    7293      0    1      1  9th    w
    8491      0    1      0 10th    w
    4374      1    1      0 10th    w
    1594      0    1      1 10th    b

    dim(jxe)

    [1] 2000    5

Review some crosstabs. All computations are based on the table below.
`tord` is the treatment variable. `abully` indicates if the respondent
had been bullied.

    library(janitor)
    jxe |> tabyl(abully, tord) |>
      adorn_percentages(denominator = 'col') |>
      adorn_pct_formatting(digits = 2) |> 
      adorn_ns(position = 'front') 

     abully            0            1
          0 863 (89.62%) 885 (85.34%)
          1 100 (10.38%) 152 (14.66%)

    (14.66 / 85.34) / (10.38 / 89.62) #OR = 1.48 

    [1] 1.483163

    14.66 - 10.38 #risk difference: 4.28

    [1] 4.28

    14.66 / 10.38 #risk ratio: 1.41

    [1] 1.412331

## 2. Models without covariates

For comparability, see how the results above map on to the first set of
regressions without covariates:

    tab6.log1 <- glm(abully ~ tord, data = jxe, family = binomial)
    summ(tab6.log1, exp = T)$coef %>% round(3)

    ##             exp(Est.)  2.5% 97.5%  z val.     p
    ## (Intercept)     0.116 0.094 0.143 -20.404 0.000
    ## tord            1.482 1.132 1.940   2.865 0.004

    tab6.lpm1 <- glm(abully ~ tord, data = jxe) #risk difference
    summ(tab6.lpm1, confint = T, robust = 'HC3')$coef %>% round(3)

    ##              Est.  2.5% 97.5% t val.     p
    ## (Intercept) 0.104 0.085 0.123 10.553 0.000
    ## tord        0.043 0.014 0.072  2.896 0.004

    tab6.poi1 <- glm(abully ~ tord, data = jxe, family = poisson) #risk ratio
    summ(tab6.poi1, robust = 'HC3')$coef %>% round(3)

    ##               Est.  S.E.  z val.     p
    ## (Intercept) -2.265 0.095 -23.900 0.000
    ## tord         0.345 0.121   2.852 0.004

If you want to run a log-binomial model

    logb <- update(tab6.lpm1, family = binomial(link = 'log'))
    #logb <- logbin(abully ~ tord, data = jxe)
    summ(logb, exp = T, digits = 3, robust = 'HC3')$coef %>% round(3)

                exp(Est.)  2.5% 97.5%  z val.     p
    (Intercept)     0.104 0.086 0.125 -23.900 0.000
    tord            1.412 1.114 1.789   2.852 0.004

## 3. Models with covariates

Compare these to the results in Table 6 in the article with the
covariates:

    tab6.log2 <- glm(abully ~ tord + female + gl + race, data = jxe, family = binomial)
    summ(tab6.log2, exp = T)$coef %>% round(3)

                exp(Est.)  2.5% 97.5%  z val.     p
    (Intercept)     0.149 0.105 0.211 -10.706 0.000
    tord            1.487 1.134 1.951   2.866 0.004
    female          1.184 0.906 1.547   1.239 0.215
    gl10th          0.749 0.525 1.068  -1.597 0.110
    gl11th          0.633 0.438 0.914  -2.439 0.015
    gl12th          0.515 0.348 0.761  -3.333 0.001
    raceb           0.646 0.411 1.015  -1.895 0.058
    raceh           1.227 0.780 1.929   0.885 0.376
    racea           1.512 0.826 2.769   1.340 0.180
    raceo           1.165 0.747 1.815   0.674 0.501

    tab6.lpm2 <- update(tab6.log2, family = gaussian)
    summ(tab6.lpm2, confint = T, robust = 'HC3')$coef %>% round(3)

                  Est.   2.5%  97.5% t val.     p
    (Intercept)  0.138  0.098  0.178  6.707 0.000
    tord         0.042  0.013  0.071  2.873 0.004
    female       0.018 -0.011  0.047  1.231 0.218
    gl10th      -0.038 -0.083  0.008 -1.631 0.103
    gl11th      -0.055 -0.100 -0.011 -2.440 0.015
    gl12th      -0.074 -0.117 -0.030 -3.331 0.001
    raceb       -0.040 -0.077 -0.003 -2.109 0.035
    raceh        0.024 -0.033  0.081  0.825 0.409
    racea        0.051 -0.035  0.138  1.161 0.246
    raceo        0.017 -0.037  0.071  0.625 0.532

    tab6.poi2 <- update(tab6.log2, family = poisson)
    summ(tab6.poi2, exp = T, robust = 'HC3')$coef %>% round(3)

                exp(Est.)  2.5% 97.5%  z val.     p
    (Intercept)     0.128 0.096 0.172 -13.815 0.000
    tord            1.411 1.114 1.787   2.852 0.004
    female          1.157 0.917 1.458   1.232 0.218
    gl10th          0.785 0.581 1.060  -1.581 0.114
    gl11th          0.678 0.494 0.929  -2.421 0.015
    gl12th          0.563 0.401 0.793  -3.295 0.001
    raceb           0.678 0.449 1.024  -1.845 0.065
    raceh           1.190 0.811 1.746   0.888 0.374
    racea           1.417 0.858 2.339   1.363 0.173
    raceo           1.140 0.781 1.664   0.678 0.498

    tab6.logbin <- update(tab6.log2, family = binomial(link = 'log'))
    ## using logbinomial, adding `em` to speed up the estimation
    ## tab6.logbin <- logbin(abully ~ tord + female + gl + race, 
    ##              data = jxe, method = 'em')
    summ(tab6.logbin, exp = T, confint = T, robust = 'HC3')$coef %>% round(3)

                exp(Est.)  2.5% 97.5%  z val.     p
    (Intercept)     0.128 0.096 0.171 -13.869 0.000
    tord            1.412 1.115 1.788   2.866 0.004
    female          1.165 0.924 1.468   1.291 0.197
    gl10th          0.780 0.578 1.052  -1.628 0.104
    gl11th          0.672 0.491 0.921  -2.470 0.013
    gl12th          0.565 0.402 0.794  -3.288 0.001
    raceb           0.674 0.446 1.019  -1.872 0.061
    raceh           1.187 0.809 1.741   0.876 0.381
    racea           1.422 0.863 2.342   1.382 0.167
    raceo           1.142 0.782 1.666   0.687 0.492

The pattern of results are similar. The Poisson, log-binomial, and
linear probability models however may provide results that are easier to
understand (especially if communicating results to a lay audience who do
not understand odds ratios).

**References**

Huang, F., & Cornell, D. (2015). Order and definitional effects in
bullying surveys: Results from an experimental study. *Psychological
Assessment, 27,* 1484-1493. doi: <http://dx.doi.org/10.1037/pas0000149>

– END