---
title: Plausible Values as Predictors
author: admin
date: 2025-06-05
tags: 
  - large scale assessments
  - multilevel models
  - plausible values
header:
  caption: ''
  image: ''
  preview: yes
draft: false
---

Although the `mixPV` function was introduced as a way to analyze large
scale assessments using multiple plausible values (PV), the function
only works if the plausible values are used as the outcome (i.e., it is
the Y variable or on the left hand side [LHS] of the equation).
However, there are times when the PV is the predictor of interest. This
still has to be analyzed properly (i.e., just don’t average all the
values).

Shown below are the steps (using a kind of nonsensical example) how to
do this manually. The `mixPV` function is still needed in order to
properly pool the results though.

We’ll ask the research question if SES (`escs`) and math performance
(`math`; five PVs) can predict math interest (e.g., look forward to
lessons). I know, the directionality of this is unclear but this is for
illustrative purposes only.

    library(MLMusingR)
    data(pisa2012)

Using the dataset, we’ll need to convert it into a tall dataset and then
convert it into a list object. We’ll use the `tidyr` package:

    library(tidyr)
    tall <- pivot_longer(pisa2012, pv1math:pv5math, names_to = 'pv',
                         values_to = 'math')

We’ll turn the categorical outcome into a continuous one for this
example:

    tall$mathinterest <- 4 - as.numeric(tall$st29q03) #outcome of interest
    # Maths interest- Look forward to lessons
    table(tall$mathinterest, tall$st29q03) #just checking

       
        Strongly agree Agree Disagree Strongly disagree  N/A Invalid Missing
      0              0     0        0              2405    0       0       0
      1              0     0     6155                 0    0       0       0
      2              0  5185        0                 0    0       0       0
      3           1935     0        0                 0    0       0       0

    ## make into a list to be analyzed separately
    dat <- split(tall, tall$pv)

Once we have our data stored as a list object, we can use the `mix`
function in the `WeMix` package to analyze each dataset (there are five,
one for each PV). I just standardize (z score) the math ability as well:

    library(WeMix)
    WeMix v4.0.3

    m1 <- \(x) { #change this function to what you are interested in
      mix(mathinterest ~ scale(math) + escs + (1|schoolid), 
            weights = c('w_fstuwt', 'w_fschwt'),
            data = x)
    }
    res <- lapply(dat, m1)

The results are stored in the `res` object as a list. This still needs
to be pooled properly. To do that, we have to make the output something
that `mixPV` can understand and pool the results:

    class(res) <- 'mixPV' #need this to pool results
    summary(res)

    Results of multilevel analyses with 5 plausible values.
    Number of observations: 3136 

    Estimates for random effects: 
                         estimate std.error statistic   df Pr(>t)    
    schoolid.(Intercept)   0.0942    0.0198    4.7544 3097 <2e-16 ***
    Residual               0.7143    0.0200   35.7462 2837 <2e-16 ***
    ---
    Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

    Estimates for fixed effects: 
                estimate std.error statistic   df Pr(>t)    
    (Intercept)   1.3995    0.0478   29.2881 3118 <2e-16 ***
    scale(math)   0.1468    0.0235    6.2461  136 <2e-16 ***
    escs         -0.0149    0.0229   -0.6499 2315   0.52    
    ---
    Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

– END

