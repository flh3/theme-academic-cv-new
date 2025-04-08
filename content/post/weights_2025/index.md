---
title: Weights with Multilevel Models
author: Francis Huang
date: '2025-03-21' 
tags:
  - large scale assessments
header:
  caption: ''
  image: ''
  preview: yes
---


This is an applied example regarding the use of weights in multilevel
models when using large scale assessments. This is using the Germany
TIMSS dataset. This accompanies the article:


> Atasever, U., Huang, F., & Rutkowski, L. (2025). Reassessing weights
> in large-scale assessments and multilevel models. Large-scale
> Assessments in Education. doi: 10.1186/s40536-025-00245-y


### 1. Read in the data and load the required libraries

Download the data from the TIMSS website.

    stu <- rio::import("c:/data/timss/deu/asgdeum7.sav")
    sch <- rio::import("c:/data/timss/deu/acgdeum7.sav")

    library(sjmisc)
    library(dplyr)
    library(WeMix)
    library(jtools)
    library(modelsummary)

    names(stu) <- tolower(names(stu))
    names(sch) <- tolower(names(sch))

    # select variables

    stu2 <- select(stu, books = asbg04,
                   likemath = asbm02e, gender = itsex, totwgt,
                   starts_with("asmmat"), idschool, idclass)
    sch2 <- select(sch, idschool, econdis =  acbg03a, schwgt, 
                   empsucc = acbgeas, resmath = acbgmrs)

Recode the variables:

    sch2$econdis <- rio::factorize(sch2$econdis)
    stu2$gender <- rio::factorize(stu2$gender)
    stu2$books <- rio::factorize(stu2$books)
    comb <- left_join(stu2, sch2, by = 'idschool')
    n_distinct(comb$idschool)

    [1] 203

    # mice::md.pattern(comb, rotate.names = T)
    tmp <- select(comb, idschool, starts_with("asm"), gender, books,
            econdis, empsucc, totwgt, schwgt) %>%
      tidyr::drop_na()
    n_distinct(tmp$idschool)

    [1] 168

### 2. Are weights informative?

    psych::describe(select(tmp, starts_with("asm")))

             vars    n   mean    sd median trimmed   mad    min    max
    asmmat01    1 2458 523.35 69.99 525.28  525.18 71.08 243.21 748.23
    asmmat02    2 2458 524.34 69.89 528.06  526.12 71.85 263.79 776.36
    asmmat03    3 2458 524.57 69.76 527.25  526.23 71.65 258.19 752.39
    asmmat04    4 2458 523.91 70.10 526.93  525.45 71.03 235.78 755.51
    asmmat05    5 2458 524.10 70.09 526.28  525.58 70.84 230.72 747.18
              range  skew kurtosis   se
    asmmat01 505.01 -0.26     0.18 1.41
    asmmat02 512.56 -0.25     0.02 1.41
    asmmat03 494.21 -0.26     0.01 1.41
    asmmat04 519.73 -0.23     0.10 1.41
    asmmat05 516.46 -0.23     0.03 1.41

Perform the weights tests with five different plausible values:

    wtest1 <- lm(asmmat01 ~ gender + books + econdis + empsucc,
             data = tmp) #just using one pv
    wtest2 <- update(wtest1, asmmat02 ~ .)
    wtest3 <- update(wtest1, asmmat03 ~ .)
    wtest4 <- update(wtest1, asmmat04 ~ .)
    wtest5 <- update(wtest1, asmmat05 ~ .)

    weights_tests(wtest1, weights = 'totwgt', data = tmp)

    DuMouchel-Duncan test of model change with weights

    F(10,2438) = 2.925
    p = 0.001

    Lower p values indicate greater influence of the weights.

    Standard errors: OLS
    -----------------------------------------------------------------
                                         Est.    S.E.   t val.      p
    -------------------------------- -------- ------- -------- ------
    (Intercept)                        370.38   40.48     9.15   0.00
    genderMale                           1.52    8.92     0.17   0.86
    totwgt                               0.31    0.19     1.63   0.10
    booksEnough to fill one             25.98   16.67     1.56   0.12
    shelf (11–25 books)                                              
    booksEnough to fill one             55.47   16.35     3.39   0.00
    bookcase (26–100 books)                                          
    booksEnough to fill two             54.33   17.55     3.09   0.00
    bookcases (101–200 books)                                        
    booksEnough to fill three or        53.19   18.12     2.94   0.00
    more bookcases (more than                                        
    200)                                                             
    econdis11 to 25%                   -24.95   10.95    -2.28   0.02
    econdis26 to 50%                   -49.24   16.23    -3.03   0.00
    econdisMore than 50%               -11.25   19.56    -0.58   0.57
    empsucc                             14.41    3.60     4.00   0.00
    genderMale:totwgt                    0.07    0.04     1.66   0.10
    totwgt:booksEnough to fill           0.00    0.08     0.06   0.95
    one shelf (11–25 books)                                          
    totwgt:booksEnough to fill           0.00    0.08     0.03   0.97
    one bookcase (26–100 books)                                      
    totwgt:booksEnough to fill           0.06    0.08     0.67   0.50
    two bookcases (101–200                                           
    books)                                                           
    totwgt:booksEnough to fill           0.08    0.08     1.01   0.31
    three or more bookcases (more                                    
    than 200)                                                        
    totwgt:econdis11 to 25%              0.08    0.05     1.69   0.09
    totwgt:econdis26 to 50%              0.15    0.08     1.85   0.06
    totwgt:econdisMore than             -0.17    0.09    -1.89   0.06
    50%                                                              
    totwgt:empsucc                      -0.05    0.02    -2.69   0.01
    -----------------------------------------------------------------

    ---
    Pfeffermann-Sverchkov test of sample weight ignorability 

    Residual correlation = -0.03, p = 0.10
    Squared residual correlation = 0.02, p = 0.29
    Cubed residual correlation = -0.02, p = 0.30

    A significant correlation may indicate biased estimates
    in the unweighted model.

    weights_tests(wtest2, weights = 'totwgt', data = tmp)

    DuMouchel-Duncan test of model change with weights

    F(10,2438) = 2.872
    p = 0.001

    Lower p values indicate greater influence of the weights.

    Standard errors: OLS
    -----------------------------------------------------------------
                                         Est.    S.E.   t val.      p
    -------------------------------- -------- ------- -------- ------
    (Intercept)                        364.38   40.44     9.01   0.00
    genderMale                           2.70    8.91     0.30   0.76
    totwgt                               0.34    0.19     1.78   0.08
    booksEnough to fill one             22.18   16.66     1.33   0.18
    shelf (11–25 books)                                              
    booksEnough to fill one             51.84   16.34     3.17   0.00
    bookcase (26–100 books)                                          
    booksEnough to fill two             54.67   17.54     3.12   0.00
    bookcases (101–200 books)                                        
    booksEnough to fill three or        46.72   18.10     2.58   0.01
    more bookcases (more than                                        
    200)                                                             
    econdis11 to 25%                   -24.36   10.94    -2.23   0.03
    econdis26 to 50%                   -42.46   16.21    -2.62   0.01
    econdisMore than 50%               -10.02   19.54    -0.51   0.61
    empsucc                             15.50    3.60     4.31   0.00
    genderMale:totwgt                    0.06    0.04     1.46   0.14
    totwgt:booksEnough to fill           0.01    0.08     0.13   0.90
    one shelf (11–25 books)                                          
    totwgt:booksEnough to fill           0.01    0.08     0.15   0.88
    one bookcase (26–100 books)                                      
    totwgt:booksEnough to fill           0.07    0.08     0.82   0.41
    two bookcases (101–200                                           
    books)                                                           
    totwgt:booksEnough to fill           0.10    0.08     1.17   0.24
    three or more bookcases (more                                    
    than 200)                                                        
    totwgt:econdis11 to 25%              0.07    0.05     1.39   0.16
    totwgt:econdis26 to 50%              0.11    0.08     1.35   0.18
    totwgt:econdisMore than             -0.16    0.09    -1.83   0.07
    50%                                                              
    totwgt:empsucc                      -0.05    0.02    -2.91   0.00
    -----------------------------------------------------------------

    ---
    Pfeffermann-Sverchkov test of sample weight ignorability 

    Residual correlation = -0.04, p = 0.03
    Squared residual correlation = -0.01, p = 0.76
    Cubed residual correlation = -0.03, p = 0.08

    A significant correlation may indicate biased estimates
    in the unweighted model.

    weights_tests(wtest3, weights = 'totwgt', data = tmp)

    DuMouchel-Duncan test of model change with weights

    F(10,2438) = 2.082
    p = 0.023

    Lower p values indicate greater influence of the weights.

    Standard errors: OLS
    -----------------------------------------------------------------
                                         Est.    S.E.   t val.      p
    -------------------------------- -------- ------- -------- ------
    (Intercept)                        396.02   40.63     9.75   0.00
    genderMale                           1.42    8.96     0.16   0.87
    totwgt                               0.28    0.19     1.43   0.15
    booksEnough to fill one             25.60   16.74     1.53   0.13
    shelf (11–25 books)                                              
    booksEnough to fill one             52.74   16.41     3.21   0.00
    bookcase (26–100 books)                                          
    booksEnough to fill two             62.58   17.62     3.55   0.00
    bookcases (101–200 books)                                        
    booksEnough to fill three or        52.06   18.19     2.86   0.00
    more bookcases (more than                                        
    200)                                                             
    econdis11 to 25%                   -30.04   10.99    -2.73   0.01
    econdis26 to 50%                   -49.00   16.29    -3.01   0.00
    econdisMore than 50%               -27.90   19.63    -1.42   0.16
    empsucc                             11.96    3.61     3.31   0.00
    genderMale:totwgt                    0.05    0.04     1.32   0.19
    totwgt:booksEnough to fill          -0.01    0.08    -0.17   0.87
    one shelf (11–25 books)                                          
    totwgt:booksEnough to fill          -0.00    0.08    -0.04   0.97
    one bookcase (26–100 books)                                      
    totwgt:booksEnough to fill           0.02    0.08     0.20   0.84
    two bookcases (101–200                                           
    books)                                                           
    totwgt:booksEnough to fill           0.08    0.08     0.91   0.36
    three or more bookcases (more                                    
    than 200)                                                        
    totwgt:econdis11 to 25%              0.09    0.05     1.90   0.06
    totwgt:econdis26 to 50%              0.13    0.08     1.60   0.11
    totwgt:econdisMore than             -0.10    0.09    -1.16   0.25
    50%                                                              
    totwgt:empsucc                      -0.04    0.02    -2.31   0.02
    -----------------------------------------------------------------

    ---
    Pfeffermann-Sverchkov test of sample weight ignorability 

    Residual correlation = -0.02, p = 0.29
    Squared residual correlation = 0.00, p = 0.96
    Cubed residual correlation = -0.01, p = 0.61

    A significant correlation may indicate biased estimates
    in the unweighted model.

    weights_tests(wtest4, weights = 'totwgt', data = tmp)

    DuMouchel-Duncan test of model change with weights

    F(10,2438) = 2.721
    p = 0.003

    Lower p values indicate greater influence of the weights.

    Standard errors: OLS
    -----------------------------------------------------------------
                                         Est.    S.E.   t val.      p
    -------------------------------- -------- ------- -------- ------
    (Intercept)                        335.39   40.68     8.25   0.00
    genderMale                          11.42    8.97     1.27   0.20
    totwgt                               0.48    0.19     2.48   0.01
    booksEnough to fill one             38.12   16.75     2.28   0.02
    shelf (11–25 books)                                              
    booksEnough to fill one             60.18   16.43     3.66   0.00
    bookcase (26–100 books)                                          
    booksEnough to fill two             72.27   17.64     4.10   0.00
    bookcases (101–200 books)                                        
    booksEnough to fill three or        60.78   18.21     3.34   0.00
    more bookcases (more than                                        
    200)                                                             
    econdis11 to 25%                   -17.07   11.01    -1.55   0.12
    econdis26 to 50%                   -45.64   16.31    -2.80   0.01
    econdisMore than 50%                 3.33   19.66     0.17   0.87
    empsucc                             15.92    3.62     4.40   0.00
    genderMale:totwgt                    0.02    0.04     0.42   0.67
    totwgt:booksEnough to fill          -0.05    0.08    -0.65   0.52
    one shelf (11–25 books)                                          
    totwgt:booksEnough to fill          -0.03    0.08    -0.42   0.67
    one bookcase (26–100 books)                                      
    totwgt:booksEnough to fill          -0.01    0.08    -0.16   0.88
    two bookcases (101–200                                           
    books)                                                           
    totwgt:booksEnough to fill           0.05    0.08     0.64   0.52
    three or more bookcases (more                                    
    than 200)                                                        
    totwgt:econdis11 to 25%              0.04    0.05     0.86   0.39
    totwgt:econdis26 to 50%              0.13    0.08     1.65   0.10
    totwgt:econdisMore than             -0.22    0.09    -2.45   0.01
    50%                                                              
    totwgt:empsucc                      -0.05    0.02    -3.09   0.00
    -----------------------------------------------------------------

    ---
    Pfeffermann-Sverchkov test of sample weight ignorability 

    Residual correlation = -0.03, p = 0.20
    Squared residual correlation = 0.02, p = 0.32
    Cubed residual correlation = -0.02, p = 0.39

    A significant correlation may indicate biased estimates
    in the unweighted model.

    weights_tests(wtest5, weights = 'totwgt', data = tmp)

    DuMouchel-Duncan test of model change with weights

    F(10,2438) = 3.038
    p = 0.001

    Lower p values indicate greater influence of the weights.

    Standard errors: OLS
    -----------------------------------------------------------------
                                         Est.    S.E.   t val.      p
    -------------------------------- -------- ------- -------- ------
    (Intercept)                        380.75   40.75     9.34   0.00
    genderMale                           1.95    8.98     0.22   0.83
    totwgt                               0.30    0.20     1.53   0.13
    booksEnough to fill one             19.09   16.79     1.14   0.26
    shelf (11–25 books)                                              
    booksEnough to fill one             39.02   16.46     2.37   0.02
    bookcase (26–100 books)                                          
    booksEnough to fill two             47.47   17.67     2.69   0.01
    bookcases (101–200 books)                                        
    booksEnough to fill three or        39.76   18.24     2.18   0.03
    more bookcases (more than                                        
    200)                                                             
    econdis11 to 25%                   -30.75   11.03    -2.79   0.01
    econdis26 to 50%                   -41.44   16.34    -2.54   0.01
    econdisMore than 50%               -22.65   19.69    -1.15   0.25
    empsucc                             15.01    3.62     4.14   0.00
    genderMale:totwgt                    0.07    0.04     1.62   0.10
    totwgt:booksEnough to fill           0.01    0.08     0.17   0.87
    one shelf (11–25 books)                                          
    totwgt:booksEnough to fill           0.05    0.08     0.67   0.51
    one bookcase (26–100 books)                                      
    totwgt:booksEnough to fill           0.08    0.08     0.95   0.34
    two bookcases (101–200                                           
    books)                                                           
    totwgt:booksEnough to fill           0.13    0.08     1.56   0.12
    three or more bookcases (more                                    
    than 200)                                                        
    totwgt:econdis11 to 25%              0.10    0.05     2.05   0.04
    totwgt:econdis26 to 50%              0.10    0.08     1.17   0.24
    totwgt:econdisMore than             -0.11    0.09    -1.27   0.21
    50%                                                              
    totwgt:empsucc                      -0.05    0.02    -2.87   0.00
    -----------------------------------------------------------------

    ---
    Pfeffermann-Sverchkov test of sample weight ignorability 

    Residual correlation = -0.04, p = 0.04
    Squared residual correlation = 0.05, p = 0.03
    Cubed residual correlation = -0.04, p = 0.08

    A significant correlation may indicate biased estimates
    in the unweighted model.

In this case, the DD test was statistically significant for all PVs: DD:
&lt;.001 &lt;.001 .02 .003 .001. For the PS, only two were statistically
significant: ns .03 ns ns .04.

NOTE: in this case, the DD test was stat significant but the PS was not.
In this case, it is not about using one test over the other. Just use
both.

Doing the DD manually is straightforward:

    wtest6 <- lm(asmmat01 ~ (gender + books + econdis + empsucc) *
                   totwgt, data = tmp)
    anova(wtest1, wtest6)

    Analysis of Variance Table

    Model 1: asmmat01 ~ gender + books + econdis + empsucc
    Model 2: asmmat01 ~ (gender + books + econdis + empsucc) * totwgt
      Res.Df     RSS Df Sum of Sq      F   Pr(>F)   
    1   2448 9203366                                
    2   2438 9094268 10    109097 2.9247 0.001191 **
    ---
    Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

Compare F and p value: it is the same as above

Performing this manually for the PS test:

    wtest7 <- lm(resid(wtest1) ~ totwgt, data = tmp)
    summary(wtest7)


    Call:
    lm(formula = resid(wtest1) ~ totwgt, data = tmp)

    Residuals:
         Min       1Q   Median       3Q      Max 
    -250.622  -38.008    2.289   41.425  202.831 

    Coefficients:
                Estimate Std. Error t value Pr(>|t|)
    (Intercept)  6.67434    4.41896   1.510    0.131
    totwgt      -0.03220    0.02047  -1.573    0.116

    Residual standard error: 61.18 on 2456 degrees of freedom
    Multiple R-squared:  0.001006,  Adjusted R-squared:  0.0005997 
    F-statistic: 2.474 on 1 and 2456 DF,  p-value: 0.1159

    wtest8 <- lm(I(resid(wtest1)^2) ~ totwgt, data = tmp)
    summary(wtest8)


    Call:
    lm(formula = I(resid(wtest1)^2) ~ totwgt, data = tmp)

    Residuals:
       Min     1Q Median     3Q    Max 
     -4148  -3400  -2110   1262  58530 

    Coefficients:
                Estimate Std. Error t value Pr(>|t|)    
    (Intercept) 3328.596    397.420   8.376   <2e-16 ***
    totwgt         2.005      1.841   1.089    0.276    
    ---
    Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

    Residual standard error: 5503 on 2456 degrees of freedom
    Multiple R-squared:  0.0004828, Adjusted R-squared:  7.586e-05 
    F-statistic: 1.186 on 1 and 2456 DF,  p-value: 0.2762

This test shows that the weights do not make a difference for the first
PV. However, overall (for all pvs and using the both tests), this shows
that the weights can make a difference. Compare the manual output to the
output using the function with the first plausible value.

### 3. Compute necessary weights

    comb$one <- 1 #weight of one
    comb$nwt <- comb$totwgt / mean(comb$totwgt) #normalized weight
    comb$pwt1 <- comb$totwgt / comb$schwgt #conditional weight; SAS, Mplus, etc will use this

Can compute both the `cluster` and `ecluster` (effective sample size)
weights using Mplus terminology. See this paper:
<https://statmodel.com/download/Scaling3.pdf>.

    library(MLMusingR) #for mixPV and wscale functions
    comb$clustw <- wscale(cluster = 'idschool', comb, 'pwt1')
    comb$eclust <- wscale(cluster = 'idschool', comb, 'pwt1', type = 'ecluster')

Can do this manually too *this is not run*:

    ## cluster weights (mPlus)
    comb$ns <- as.numeric(ave(comb$idschool, comb$idschool, FUN = length)) #how many in cluster (numerator)
    comb$swt <- ave(comb$pwt1, comb$idschool, FUN = sum) #sum of wij (denominator)
    comb$clustw <- with(comb, pwt1 * (ns / swt)) #wij x adjustment

    comb <- comb %>% 
      group_by(idschool) %>%
      mutate(ns = n(),
             swt = sum(pwt1),
             clustw = pwt1 * (ns / swt)
            )

    ecluster (effective sample size)
    comb$ess <- sum(comb$pwt1)^2 / sum(comb$pwt1^2)
    comb <- comb %>% group_by(idschool) %>%
      mutate(ess = sum(pwt1)^2 / sum(pwt1^2), #effective sample size
             eclust = pwt1 * (ess / sum(pwt1)))

    range(comb$clustw) ##both 1

    [1] 1 1

    range(comb$eclust)

    [1] 1 1

With this example using TIMSS, there is *no* variation within school–
which means that weights at level-1 are all just 1.0.

    comb %>% group_by(idschool) %>%
      summarise(vr = var(pwt1)) %>% filter(vr > 0)

    # A tibble: 0 × 2
    # ℹ 2 variables: idschool <dbl>, vr <dbl>

Getting descriptives at the student level:

    library(tableone)
    CreateTableOne(data = select(tmp, gender, books, asmmat01))

                                                               
                                                                Overall       
      n                                                           2458        
      gender (%)                                                              
         Female                                                   1260 (51.3) 
         Male                                                     1198 (48.7) 
         Omitted or invalid                                          0 ( 0.0) 
      books (%)                                                               
         None or very few (0–10 books)                             274 (11.1) 
         Enough to fill one shelf (11–25 books)                    577 (23.5) 
         Enough to fill one bookcase (26–100 books)                780 (31.7) 
         Enough to fill two bookcases (101–200 books)              429 (17.5) 
         Enough to fill three or more bookcases (more than 200)    398 (16.2) 
         Omitted or invalid                                          0 ( 0.0) 
      asmmat01 (mean (SD))                                      523.35 (69.99)

And at the school level:

    CreateTableOne(data = select(tmp, idschool, econdis, empsucc) %>%
                     filter(!duplicated(idschool)), vars = c('econdis', 'empsucc'))

                           
                            Overall     
      n                      168        
      econdis (%)                       
         0 to 10%             42 (25.0) 
         11 to 25%            63 (37.5) 
         26 to 50%            36 (21.4) 
         More than 50%        27 (16.1) 
         Omitted or invalid    0 ( 0.0) 
      empsucc (mean (SD))   9.40 (1.65) 

### 4. Fit the models

    ### unconditional
    l2l1 <- mixPV(asmmat01 + asmmat02 + asmmat03 +
                    asmmat04 + asmmat05 ~ (1|idschool),
                weights = c('totwgt', 'schwgt'),
                data = comb, mc = TRUE)

    Attempting to use 15 cores. Progress will not be displayed. Please wait.

    nowgt <- mixPV(asmmat01 + asmmat02 + asmmat03 +
                   asmmat04 + asmmat05 ~  (1|idschool),
                 weights = c('one', 'one'),
                 data = comb, cWeights = TRUE, mc = TRUE)

    Attempting to use 15 cores. Progress will not be displayed. Please wait.

    l2 <- mixPV(asmmat01 + asmmat02 + asmmat03 +
                asmmat04 + asmmat05 ~ (1|idschool),
              weights = c('one', 'schwgt'),
              data = comb, cWeights = TRUE, mc = TRUE)

    Attempting to use 15 cores. Progress will not be displayed. Please wait.

    l1 <- mixPV(asmmat01 + asmmat02 + asmmat03 +
                asmmat04 + asmmat05 ~ (1|idschool),
              weights = c('nwt', 'one'),
              data = comb, cWeights = TRUE, mc = TRUE)

    Attempting to use 15 cores. Progress will not be displayed. Please wait.

    ## to output in modelsummary
    source("https://raw.githubusercontent.com/flh3/pubdata/main/mixPV/wemix_modelsummary.R")

    ## unconditional results
    modelsummary(list("two" = l2l1, "L2" = l2, 
                      "L1" = l1, "no" = nowgt), 
                 stars = TRUE)

<table style="width:97%;">
<colgroup>
<col style="width: 22%" />
<col style="width: 18%" />
<col style="width: 18%" />
<col style="width: 18%" />
<col style="width: 18%" />
</colgroup>
<thead>
<tr class="header">
<th></th>
<th>two</th>
<th>L2</th>
<th>L1</th>
<th>no</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td>(Intercept)</td>
<td>518.167***</td>
<td>518.640***</td>
<td>518.740***</td>
<td>518.841***</td>
</tr>
<tr class="even">
<td></td>
<td>(4.153)</td>
<td>(4.136)</td>
<td>(2.822)</td>
<td>(2.780)</td>
</tr>
<tr class="odd">
<td>idschool.(Intercept)</td>
<td>1683.861***</td>
<td>1496.649**</td>
<td>1280.252***</td>
<td>1234.790***</td>
</tr>
<tr class="even">
<td></td>
<td>(482.320)</td>
<td>(489.666)</td>
<td>(229.657)</td>
<td>(212.052)</td>
</tr>
<tr class="odd">
<td>Residual</td>
<td>3525.220***</td>
<td>3679.265***</td>
<td>3658.870***</td>
<td>3689.888***</td>
</tr>
<tr class="even">
<td></td>
<td>(100.414)</td>
<td>(120.293)</td>
<td>(104.439)</td>
<td>(104.684)</td>
</tr>
<tr class="odd">
<td>Nobs</td>
<td>3437</td>
<td>3437</td>
<td>3437</td>
<td>3437</td>
</tr>
<tr class="even">
<td>N.pv</td>
<td>5</td>
<td>5</td>
<td>5</td>
<td>5</td>
</tr>
<tr class="odd">
<td>AICbar</td>
<td>7850728.97216705</td>
<td>2949163.88327609</td>
<td>38346.5801643249</td>
<td>38368.8602627188</td>
</tr>
<tr class="even">
<td>BICbar</td>
<td>7850747.39922988</td>
<td>2949182.31033892</td>
<td>38365.0072271555</td>
<td>38387.2873255494</td>
</tr>
</tbody><tfoot>
<tr class="odd">
<td colspan="5"><ul>
<li>p &lt; 0.1, * p &lt; 0.05, ** p &lt; 0.01, *** p &lt; 0.001</li>
</ul></td>
</tr>
</tfoot>
&#10;</table>

The *τ*<sub>00</sub> are higher in the ones with the L2 weights or
weights at both levels.

Now to fit the model with predictors:

    l2l1 <- mixPV(asmmat01 + asmmat02 + asmmat03 +
                  asmmat04 + asmmat05 ~ gender + books + econdis + empsucc + (1|idschool),
              weights = c('totwgt', 'schwgt'),
              data = comb, mc = TRUE)

    Attempting to use 15 cores. Progress will not be displayed. Please wait.

    nowgt <- mixPV(asmmat01 + asmmat02 + asmmat03 +
                   asmmat04 + asmmat05 ~ gender + books + econdis  + empsucc + (1|idschool),
              weights = c('one', 'one'),
              data = comb, cWeights = TRUE, mc = TRUE)

    Attempting to use 15 cores. Progress will not be displayed. Please wait.

    l2 <- mixPV(asmmat01 + asmmat02 + asmmat03 +
                asmmat04 + asmmat05 ~ gender + books + econdis  + empsucc + (1|idschool),
              weights = c('one', 'schwgt'),
              data = comb, cWeights = TRUE, mc = TRUE)

    Attempting to use 15 cores. Progress will not be displayed. Please wait.

    l1 <- mixPV(asmmat01 + asmmat02 + asmmat03 +
                asmmat04 + asmmat05 ~ gender + books + econdis + empsucc + (1|idschool),
              weights = c('nwt', 'one'),
              data = comb, cWeights = TRUE, mc = TRUE)

    Attempting to use 15 cores. Progress will not be displayed. Please wait.

    modelsummary(list("two" = l2l1, "L2" = l2, 
                      "L1" = l1, "no" = nowgt), 
                 stars = TRUE)

<table style="width:98%;">
<colgroup>
<col style="width: 43%" />
<col style="width: 13%" />
<col style="width: 13%" />
<col style="width: 13%" />
<col style="width: 13%" />
</colgroup>
<thead>
<tr class="header">
<th></th>
<th>two</th>
<th>L2</th>
<th>L1</th>
<th>no</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td>(Intercept)</td>
<td>478.566***</td>
<td>481.008***</td>
<td>448.600***</td>
<td>446.009***</td>
</tr>
<tr class="even">
<td></td>
<td>(31.695)</td>
<td>(30.156)</td>
<td>(19.742)</td>
<td>(18.711)</td>
</tr>
<tr class="odd">
<td>genderMale</td>
<td>16.152***</td>
<td>14.851***</td>
<td>16.077***</td>
<td>14.751***</td>
</tr>
<tr class="even">
<td></td>
<td>(2.923)</td>
<td>(3.133)</td>
<td>(2.870)</td>
<td>(2.773)</td>
</tr>
<tr class="odd">
<td>booksEnough to fill one shelf (11–25 books)</td>
<td>20.330***</td>
<td>21.566***</td>
<td>21.510***</td>
<td>21.982***</td>
</tr>
<tr class="even">
<td></td>
<td>(4.898)</td>
<td>(5.631)</td>
<td>(4.941)</td>
<td>(5.020)</td>
</tr>
<tr class="odd">
<td>booksEnough to fill one bookcase (26–100 books)</td>
<td>46.237***</td>
<td>47.454***</td>
<td>48.163***</td>
<td>49.006***</td>
</tr>
<tr class="even">
<td></td>
<td>(4.869)</td>
<td>(5.556)</td>
<td>(4.902)</td>
<td>(5.031)</td>
</tr>
<tr class="odd">
<td>booksEnough to fill two bookcases (101–200 books)</td>
<td>58.724***</td>
<td>59.215***</td>
<td>61.302***</td>
<td>61.647***</td>
</tr>
<tr class="even">
<td></td>
<td>(5.317)</td>
<td>(5.949)</td>
<td>(5.340)</td>
<td>(5.423)</td>
</tr>
<tr class="odd">
<td>booksEnough to fill three or more bookcases (more than 200)</td>
<td>62.767***</td>
<td>63.504***</td>
<td>65.213***</td>
<td>64.565***</td>
</tr>
<tr class="even">
<td></td>
<td>(5.619)</td>
<td>(6.476)</td>
<td>(5.674)</td>
<td>(5.941)</td>
</tr>
<tr class="odd">
<td>econdis11 to 25%</td>
<td>-5.587</td>
<td>-5.722</td>
<td>-6.303</td>
<td>-6.936</td>
</tr>
<tr class="even">
<td></td>
<td>(6.256)</td>
<td>(6.227)</td>
<td>(5.123)</td>
<td>(4.986)</td>
</tr>
<tr class="odd">
<td>econdis26 to 50%</td>
<td>-20.962**</td>
<td>-21.240**</td>
<td>-18.255**</td>
<td>-18.652**</td>
</tr>
<tr class="even">
<td></td>
<td>(7.798)</td>
<td>(7.479)</td>
<td>(6.223)</td>
<td>(5.966)</td>
</tr>
<tr class="odd">
<td>econdisMore than 50%</td>
<td>-63.154**</td>
<td>-64.856**</td>
<td>-48.280***</td>
<td>-47.310***</td>
</tr>
<tr class="even">
<td></td>
<td>(22.172)</td>
<td>(22.259)</td>
<td>(12.042)</td>
<td>(11.020)</td>
</tr>
<tr class="odd">
<td>empsucc</td>
<td>1.039</td>
<td>0.779</td>
<td>3.989*</td>
<td>4.330**</td>
</tr>
<tr class="even">
<td></td>
<td>(2.996)</td>
<td>(2.914)</td>
<td>(1.768)</td>
<td>(1.662)</td>
</tr>
<tr class="odd">
<td>idschool.(Intercept)</td>
<td>856.482***</td>
<td>648.425**</td>
<td>527.251***</td>
<td>455.856***</td>
</tr>
<tr class="even">
<td></td>
<td>(216.065)</td>
<td>(208.650)</td>
<td>(125.477)</td>
<td>(104.879)</td>
</tr>
<tr class="odd">
<td>Residual</td>
<td>3149.349***</td>
<td>3304.061***</td>
<td>3293.601***</td>
<td>3352.888***</td>
</tr>
<tr class="even">
<td></td>
<td>(108.213)</td>
<td>(145.552)</td>
<td>(113.330)</td>
<td>(114.145)</td>
</tr>
<tr class="odd">
<td>Nobs</td>
<td>2458</td>
<td>2458</td>
<td>2458</td>
<td>2458</td>
</tr>
<tr class="even">
<td>N.pv</td>
<td>5</td>
<td>5</td>
<td>5</td>
<td>5</td>
</tr>
<tr class="odd">
<td>AICbar</td>
<td>5580849.91142946</td>
<td>2135564.71856396</td>
<td>27237.0112561975</td>
<td>27132.3408273488</td>
</tr>
<tr class="even">
<td>BICbar</td>
<td>5580919.59666894</td>
<td>2135634.40380344</td>
<td>27306.696495679</td>
<td>27202.0260668304</td>
</tr>
</tbody><tfoot>
<tr class="odd">
<td colspan="5"><ul>
<li>p &lt; 0.1, * p &lt; 0.05, ** p &lt; 0.01, *** p &lt; 0.001</li>
</ul></td>
</tr>
</tfoot>
&#10;</table>

Both the models with L2 only or weights at both levels are most similar.
Compare this as well to the simulation results in the paper. `empsucc`
is not statistically significant while in the models with no weight or
level-1 weight only, they are (probably incorrect).

