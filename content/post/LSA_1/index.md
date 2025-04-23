---
title: Using Plausible Values with Multilevel Models Using R
author: Francis Huang
date: '2024-01-28'
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


{{% callout note %}}
The `mixPV` function has been updated. You can read more about it [here](../LSA2/)
{{% /callout %}}

Syntax to accompany the article: 

Huang, F. (2024). Using plausible values when fitting multilevel models with large-scale assessment data using R. [*Large-scale Assessments in Education.*](https://largescaleassessmentsineducation.springeropen.com/articles/10.1186/s40536-024-00192-0)

When fitting multilevel models using large scale assessments such as PISA or TIMSS, it is important to account for:

1) the use of weights at different levels and 
2) the presence of multiple plausible values.  

I am often asked how do you run this analysis in R. The following example shows how this can be done (read the article for more details). The use of plausible values in inherently a missing data problem-- no one child in these LSAs takes the whole battery of tests (if any did, a child would need more than 10 hours to complete the assessment-- which is not feasible). Respondents only complete *some* of the tests-- not the whole thing. See Rutkowski et al (2010) for a [guide](https://psycnet.apa.org/record/2010-12529-005) on this. Note: there are other ways of doing this too (such as using the `EdSurvey` package [Bailey et al.]).

## 1. Load in packages and the `mixPV` functions

The `mixPV` functions are loaded in using the `source` function:


``` r
library(WeMix) #for mixed models with weights at different levels
library(modelsummary) #for output tables
library(broom) #needed for output tables
source("https://raw.githubusercontent.com/flh3/pubdata/main/mixPV/mixPV.R")
```

Note: this function is just a wrapper that uses the `mix` function in the `WeMix` package (Bailey et al.). The `mix` function does all the hard work when fitting the models!

## 2. Load in the dataset (`pisa2012`). 

The dataset is available in the `MLMusingR` package (v0.3.2).


``` r
## Import/load the data (I like using rio::import)
# pisa2012 <- rio::import("https://github.com/flh3/pubdata/raw/main/mixPV/pisa2012.rds")
data(pisa2012, package = 'MLMusingR') #data is in the MLMusingR package

## Inspect the data
## install.packages("lme4", type = "source") #if there are issues with Matrix
head(pisa2012)
```

```
  pv1math pv2math pv3math pv4math pv5math  escs schoolid           st29q03
1     365     362     348     348     400 -0.60  0000001             Agree
2     416     422     411     422     393  0.89  0000001          Disagree
3     410     371     422     387     424 -1.99  0000001    Strongly agree
4     290     333     316     317     352 -0.35  0000001             Agree
5     383     423     415     369     380  0.63  0000001          Disagree
6     398     397     384     378     489  0.62  0000001 Strongly disagree
  st04q01 w_fstuwt w_fschwt    sc14q02  pwt1 noise1
1  Female      766     83.7 Not at all  9.16 -1.207
2    Male      809     83.7 Not at all  9.67  0.277
3    Male      846     83.7 Not at all 10.11  1.084
4    Male      809     83.7 Not at all  9.67 -2.346
5    Male      809     83.7 Not at all  9.67  0.429
6  Female      766     83.7 Not at all  9.16  0.506
```

``` r
summary(pisa2012)
```

```
    pv1math       pv2math       pv3math       pv4math       pv5math   
 Min.   :225   Min.   :206   Min.   :225   Min.   :190   Min.   :189  
 1st Qu.:421   1st Qu.:420   1st Qu.:421   1st Qu.:422   1st Qu.:422  
 Median :479   Median :481   Median :480   Median :482   Median :480  
 Mean   :484   Mean   :484   Mean   :485   Mean   :485   Mean   :485  
 3rd Qu.:545   3rd Qu.:545   3rd Qu.:546   3rd Qu.:546   3rd Qu.:544  
 Max.   :822   Max.   :808   Max.   :784   Max.   :776   Max.   :779  
                                                                      
      escs         schoolid                      st29q03        st04q01    
 Min.   :-3.80   Length:3136        Strongly agree   : 387   Female :1570  
 1st Qu.:-0.48   Class :character   Agree            :1037   Male   :1566  
 Median : 0.30   Mode  :character   Disagree         :1231   N/A    :   0  
 Mean   : 0.20                      Strongly disagree: 481   Invalid:   0  
 3rd Qu.: 0.93                      N/A              :   0   Missing:   0  
 Max.   : 3.12                      Invalid          :   0                 
                                    Missing          :   0                 
    w_fstuwt       w_fschwt              sc14q02          pwt1     
 Min.   : 134   Min.   :  22   Not at all    :2276   Min.   : 1.2  
 1st Qu.: 560   1st Qu.:  49   Very little   : 525   1st Qu.: 4.7  
 Median : 665   Median :  81   To some extent: 272   Median : 8.9  
 Mean   : 713   Mean   : 144   A lot         :  63   Mean   : 9.5  
 3rd Qu.: 781   3rd Qu.: 149   N/A           :   0   3rd Qu.:13.6  
 Max.   :2598   Max.   :1943   Invalid       :   0   Max.   :37.0  
                               Missing       :   0                 
     noise1     
 Min.   :-3.40  
 1st Qu.:-0.66  
 Median : 0.02  
 Mean   : 0.01  
 3rd Qu.: 0.67  
 Max.   : 3.20  
                
```

``` r
dim(pisa2012)
```

```
[1] 3136   14
```

Read the article for a description of the predictors or use `?pisa2012` for the codebook. NOTE: do not use this dataset on its own for running any kind of substantive analysis. 

## 3. The model can be fit using the `mix` function in the WeMix package.

However, the example below just uses one plausible value (pv). I just show this to illustrate how using all the pvs can have slightly different results compared to just using one pv. Take note: when running these analysis-- do NOT just use one pv-- that is incorrect (despite what you may have read in some other peer-reviewed manuscript) for a variety of reasons. Read the documentation of the LSA.


``` r
## just using one plausible value
nopv <- mix(pv1math ~
 st29q03 + sc14q02 + st04q01 + escs + (1|schoolid), 
 weights = c('w_fstuwt', 'w_fschwt'), 
 data = pisa2012)
summary(nopv)
```

```
Call:
mix(formula = pv1math ~ st29q03 + sc14q02 + st04q01 + escs + 
    (1 | schoolid), data = pisa2012, weights = c("w_fstuwt", 
    "w_fschwt"))

Variance terms:
 Level    Group        Name Variance Std. Error Std.Dev.
     2 schoolid (Intercept)     1414        328     37.6
     1 Residual                 5265        152     72.6
Groups:
 Level    Group n size mean wgt sum wgt
     2 schoolid    157      192   30161
     1      Obs   3136      713 2236615

Fixed Effects:
                         Estimate Std. Error t value
(Intercept)                486.80       7.78   62.59
st29q03Agree               -11.11       5.70   -1.95
st29q03Disagree            -19.26       5.46   -3.53
st29q03Strongly disagree   -41.54       6.86   -6.05
sc14q02Very little         -21.34      17.06   -1.25
sc14q02To some extent      -11.78      12.82   -0.92
sc14q02A lot               -26.91       7.66   -3.51
st04q01Male                  9.51       2.99    3.18
escs                        25.57       2.12   12.07

lnl= -12789991.99 
Intraclass Correlation= 0.212 
```

``` r
## can also do it this way (using conditional weights)
# nopv1 <- mix(pv1math ~
#  st29q03 + sc14q02 + st04q01 + escs + (1|schoolid), 
#  weights = c('pwt1', 'w_fschwt'), 
#  data = pisa2012, cWeights = TRUE)
# summary(nopv1)
```

## 4. Fit the model using several plausible values

The multiple plausible values are explicitly specified in the formula. A basic random intercepts model can be fit using:


``` r
## using 5 plausible values
m0 <- mixPV(pv1math + pv2math + pv3math + pv4math + pv5math ~
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
summary(m0)
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

``` r
# summary_all(m0) #shows all results with multiple PVs
```

A random slope model can also be fit (adding a random slope for `escs`):


``` r
m1 <- mixPV(pv1math + pv2math + pv3math + pv4math + pv5math ~
 st29q03 + sc14q02 + st04q01 + escs + (escs|schoolid), 
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
## a boundary(singular) fit message suggests that the 
## variance components estimated are not different from zero
## the results however do show variation
summary(m1)
```

```
Results of multilevel analyses with 5 plausible values.
Number of observations: 3136 

Estimates for random effects: 
                     estimate std.error statistic     df Pr(>t)    
schoolid.(Intercept)  1380.68    325.42      4.24 2151.8 <2e-16 ***
schoolid.escs          324.39     63.89      5.08  350.5 <2e-16 ***
Residual              5021.04    106.04     47.35   81.6 <2e-16 ***
---
Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

Estimates for fixed effects: 
                         estimate std.error statistic   df Pr(>t)    
(Intercept)                486.44      8.27     58.84  245 <2e-16 ***
st29q03Agree               -10.54      5.89     -1.79  370 0.0746 .  
st29q03Disagree            -17.40      6.08     -2.86  108 0.0050 ** 
st29q03Strongly disagree   -36.88      7.05     -5.23  250 <2e-16 ***
sc14q02Very little         -23.18     15.78     -1.47  706 0.1422    
sc14q02To some extent      -13.75     11.88     -1.16  480 0.2478    
sc14q02A lot               -35.74      8.59     -4.16  258 <2e-16 ***
st04q01Male                  8.88      3.09      2.88  192 0.0045 ** 
escs                        27.12      2.44     11.11 1412 <2e-16 ***
---
Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

``` r
#summary_all(m1)
```

Differences between the two models can be tested using a likelihood ratio test: is the full model an improvement over the reduced model (w/random slope vs random intercepts only?). See Grund et al. [(2023)]( https://doi.org/10.1037/met0000556) article in *Psych Methods* about different ways an LRT can be performed when using imputed data.


``` r
lrtPV(m1, m0) #full vs reduced
```

```
     F df1  df2   r     pv
1 98.3   2 2.65 441 0.0033
```

Just testing this with random noise too: `noise1` (random variable that is not associated with the outcome)-- differences should not be statistically significant (i.e., adding the variable does not improve the model).


``` r
m2 <- mixPV(pv1math + pv2math + pv3math + pv4math + pv5math ~
 st29q03 + sc14q02 + st04q01 + escs + noise1 + (1|schoolid), 
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
summary(m2) #ns
```

```
Results of multilevel analyses with 5 plausible values.
Number of observations: 3136 

Estimates for random effects: 
                     estimate std.error statistic   df Pr(>t)    
schoolid.(Intercept)  1394.17    325.01      4.29 8527 <2e-16 ***
Residual              5295.00    158.46     33.41 2716 <2e-16 ***
---
Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

Estimates for fixed effects: 
                         estimate std.error statistic   df Pr(>t)    
(Intercept)               489.496     8.397    58.294  247 <2e-16 ***
st29q03Agree              -11.303     5.998    -1.884  526 0.0601 .  
st29q03Disagree           -19.562     6.060    -3.228  118 0.0016 ** 
st29q03Strongly disagree  -39.961     7.022    -5.691  335 <2e-16 ***
sc14q02Very little        -22.883    16.710    -1.369  777 0.1713    
sc14q02To some extent     -17.504    11.784    -1.485  204 0.1390    
sc14q02A lot              -30.078     7.702    -3.905  306 0.0001 ***
st04q01Male                 8.418     3.101     2.715  170 0.0073 ** 
escs                       25.895     2.114    12.249 2627 <2e-16 ***
noise1                      0.590     1.397     0.422 1941 0.6729    
---
Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

``` r
lrtPV(m2, m0) #ns too
```

```
     F df1  df2    r    pv
1 3.03   1 4.23 34.8 0.153
```

## 5. Showing results side by side

I like building up models over several phases. This just illustrates how you can do that. This output should still be fixed up (see article) and read the `modelsummary` documentation. 


``` r
modelsummary(list("ri" = m0, 'rs' = m1),
             stars = TRUE, output = 'gt')
```

<!--html_preserve--><div id="zfrqlobnbp" style="padding-left:0px;padding-right:0px;padding-top:10px;padding-bottom:10px;overflow-x:auto;overflow-y:auto;width:auto;height:auto;">
<style>#zfrqlobnbp table {
  font-family: system-ui, 'Segoe UI', Roboto, Helvetica, Arial, sans-serif, 'Apple Color Emoji', 'Segoe UI Emoji', 'Segoe UI Symbol', 'Noto Color Emoji';
  -webkit-font-smoothing: antialiased;
  -moz-osx-font-smoothing: grayscale;
}

#zfrqlobnbp thead, #zfrqlobnbp tbody, #zfrqlobnbp tfoot, #zfrqlobnbp tr, #zfrqlobnbp td, #zfrqlobnbp th {
  border-style: none;
}

#zfrqlobnbp p {
  margin: 0;
  padding: 0;
}

#zfrqlobnbp .gt_table {
  display: table;
  border-collapse: collapse;
  line-height: normal;
  margin-left: auto;
  margin-right: auto;
  color: #333333;
  font-size: 16px;
  font-weight: normal;
  font-style: normal;
  background-color: #FFFFFF;
  width: auto;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #A8A8A8;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #A8A8A8;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
}

#zfrqlobnbp .gt_caption {
  padding-top: 4px;
  padding-bottom: 4px;
}

#zfrqlobnbp .gt_title {
  color: #333333;
  font-size: 125%;
  font-weight: initial;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-color: #FFFFFF;
  border-bottom-width: 0;
}

#zfrqlobnbp .gt_subtitle {
  color: #333333;
  font-size: 85%;
  font-weight: initial;
  padding-top: 3px;
  padding-bottom: 5px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-color: #FFFFFF;
  border-top-width: 0;
}

#zfrqlobnbp .gt_heading {
  background-color: #FFFFFF;
  text-align: center;
  border-bottom-color: #FFFFFF;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#zfrqlobnbp .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#zfrqlobnbp .gt_col_headings {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#zfrqlobnbp .gt_col_heading {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 6px;
  padding-left: 5px;
  padding-right: 5px;
  overflow-x: hidden;
}

#zfrqlobnbp .gt_column_spanner_outer {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  padding-top: 0;
  padding-bottom: 0;
  padding-left: 4px;
  padding-right: 4px;
}

#zfrqlobnbp .gt_column_spanner_outer:first-child {
  padding-left: 0;
}

#zfrqlobnbp .gt_column_spanner_outer:last-child {
  padding-right: 0;
}

#zfrqlobnbp .gt_column_spanner {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 5px;
  overflow-x: hidden;
  display: inline-block;
  width: 100%;
}

#zfrqlobnbp .gt_spanner_row {
  border-bottom-style: hidden;
}

#zfrqlobnbp .gt_group_heading {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  text-align: left;
}

#zfrqlobnbp .gt_empty_group_heading {
  padding: 0.5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: middle;
}

#zfrqlobnbp .gt_from_md > :first-child {
  margin-top: 0;
}

#zfrqlobnbp .gt_from_md > :last-child {
  margin-bottom: 0;
}

#zfrqlobnbp .gt_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  margin: 10px;
  border-top-style: solid;
  border-top-width: 1px;
  border-top-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  overflow-x: hidden;
}

#zfrqlobnbp .gt_stub {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 5px;
  padding-right: 5px;
}

#zfrqlobnbp .gt_stub_row_group {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 5px;
  padding-right: 5px;
  vertical-align: top;
}

#zfrqlobnbp .gt_row_group_first td {
  border-top-width: 2px;
}

#zfrqlobnbp .gt_row_group_first th {
  border-top-width: 2px;
}

#zfrqlobnbp .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#zfrqlobnbp .gt_first_summary_row {
  border-top-style: solid;
  border-top-color: #D3D3D3;
}

#zfrqlobnbp .gt_first_summary_row.thick {
  border-top-width: 2px;
}

#zfrqlobnbp .gt_last_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#zfrqlobnbp .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#zfrqlobnbp .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}

#zfrqlobnbp .gt_last_grand_summary_row_top {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: double;
  border-bottom-width: 6px;
  border-bottom-color: #D3D3D3;
}

#zfrqlobnbp .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}

#zfrqlobnbp .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#zfrqlobnbp .gt_footnotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}

#zfrqlobnbp .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}

#zfrqlobnbp .gt_sourcenotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}

#zfrqlobnbp .gt_sourcenote {
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}

#zfrqlobnbp .gt_left {
  text-align: left;
}

#zfrqlobnbp .gt_center {
  text-align: center;
}

#zfrqlobnbp .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}

#zfrqlobnbp .gt_font_normal {
  font-weight: normal;
}

#zfrqlobnbp .gt_font_bold {
  font-weight: bold;
}

#zfrqlobnbp .gt_font_italic {
  font-style: italic;
}

#zfrqlobnbp .gt_super {
  font-size: 65%;
}

#zfrqlobnbp .gt_footnote_marks {
  font-size: 75%;
  vertical-align: 0.4em;
  position: initial;
}

#zfrqlobnbp .gt_asterisk {
  font-size: 100%;
  vertical-align: 0;
}

#zfrqlobnbp .gt_indent_1 {
  text-indent: 5px;
}

#zfrqlobnbp .gt_indent_2 {
  text-indent: 10px;
}

#zfrqlobnbp .gt_indent_3 {
  text-indent: 15px;
}

#zfrqlobnbp .gt_indent_4 {
  text-indent: 20px;
}

#zfrqlobnbp .gt_indent_5 {
  text-indent: 25px;
}

#zfrqlobnbp .katex-display {
  display: inline-flex !important;
  margin-bottom: 0.75em !important;
}

#zfrqlobnbp div.Reactable > div.rt-table > div.rt-thead > div.rt-tr.rt-tr-group-header > div.rt-th-group:after {
  height: 0px !important;
}
</style>
<table class="gt_table" data-quarto-disable-processing="false" data-quarto-bootstrap="false">
  <thead>
    <tr class="gt_col_headings">
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1" scope="col" id=" "> </th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="ri">ri</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="rs">rs</th>
    </tr>
  </thead>
  <tbody class="gt_table_body">
    <tr><td headers=" " class="gt_row gt_left">(Intercept)</td>
<td headers="ri" class="gt_row gt_center">489.507***</td>
<td headers="rs" class="gt_row gt_center">486.441***</td></tr>
    <tr><td headers=" " class="gt_row gt_left"></td>
<td headers="ri" class="gt_row gt_center">(8.408)</td>
<td headers="rs" class="gt_row gt_center">(8.268)</td></tr>
    <tr><td headers=" " class="gt_row gt_left">st29q03Agree</td>
<td headers="ri" class="gt_row gt_center">-11.286+</td>
<td headers="rs" class="gt_row gt_center">-10.536+</td></tr>
    <tr><td headers=" " class="gt_row gt_left"></td>
<td headers="ri" class="gt_row gt_center">(5.989)</td>
<td headers="rs" class="gt_row gt_center">(5.894)</td></tr>
    <tr><td headers=" " class="gt_row gt_left">st29q03Disagree</td>
<td headers="ri" class="gt_row gt_center">-19.534**</td>
<td headers="rs" class="gt_row gt_center">-17.404**</td></tr>
    <tr><td headers=" " class="gt_row gt_left"></td>
<td headers="ri" class="gt_row gt_center">(6.050)</td>
<td headers="rs" class="gt_row gt_center">(6.079)</td></tr>
    <tr><td headers=" " class="gt_row gt_left">st29q03Strongly disagree</td>
<td headers="ri" class="gt_row gt_center">-39.949***</td>
<td headers="rs" class="gt_row gt_center">-36.881***</td></tr>
    <tr><td headers=" " class="gt_row gt_left"></td>
<td headers="ri" class="gt_row gt_center">(7.025)</td>
<td headers="rs" class="gt_row gt_center">(7.050)</td></tr>
    <tr><td headers=" " class="gt_row gt_left">sc14q02Very little</td>
<td headers="ri" class="gt_row gt_center">-22.927</td>
<td headers="rs" class="gt_row gt_center">-23.178</td></tr>
    <tr><td headers=" " class="gt_row gt_left"></td>
<td headers="ri" class="gt_row gt_center">(16.690)</td>
<td headers="rs" class="gt_row gt_center">(15.777)</td></tr>
    <tr><td headers=" " class="gt_row gt_left">sc14q02To some extent</td>
<td headers="ri" class="gt_row gt_center">-17.605</td>
<td headers="rs" class="gt_row gt_center">-13.748</td></tr>
    <tr><td headers=" " class="gt_row gt_left"></td>
<td headers="ri" class="gt_row gt_center">(11.766)</td>
<td headers="rs" class="gt_row gt_center">(11.881)</td></tr>
    <tr><td headers=" " class="gt_row gt_left">sc14q02A lot</td>
<td headers="ri" class="gt_row gt_center">-30.050***</td>
<td headers="rs" class="gt_row gt_center">-35.744***</td></tr>
    <tr><td headers=" " class="gt_row gt_left"></td>
<td headers="ri" class="gt_row gt_center">(7.748)</td>
<td headers="rs" class="gt_row gt_center">(8.593)</td></tr>
    <tr><td headers=" " class="gt_row gt_left">st04q01Male</td>
<td headers="ri" class="gt_row gt_center">8.395**</td>
<td headers="rs" class="gt_row gt_center">8.881**</td></tr>
    <tr><td headers=" " class="gt_row gt_left"></td>
<td headers="ri" class="gt_row gt_center">(3.103)</td>
<td headers="rs" class="gt_row gt_center">(3.088)</td></tr>
    <tr><td headers=" " class="gt_row gt_left">escs</td>
<td headers="ri" class="gt_row gt_center">25.883***</td>
<td headers="rs" class="gt_row gt_center">27.123***</td></tr>
    <tr><td headers=" " class="gt_row gt_left"></td>
<td headers="ri" class="gt_row gt_center">(2.105)</td>
<td headers="rs" class="gt_row gt_center">(2.441)</td></tr>
    <tr><td headers=" " class="gt_row gt_left">schoolid.(Intercept)</td>
<td headers="ri" class="gt_row gt_center">1397.963***</td>
<td headers="rs" class="gt_row gt_center">1380.681***</td></tr>
    <tr><td headers=" " class="gt_row gt_left"></td>
<td headers="ri" class="gt_row gt_center">(327.262)</td>
<td headers="rs" class="gt_row gt_center">(325.420)</td></tr>
    <tr><td headers=" " class="gt_row gt_left">Residual</td>
<td headers="ri" class="gt_row gt_center">5295.229***</td>
<td headers="rs" class="gt_row gt_center">5021.039***</td></tr>
    <tr><td headers=" " class="gt_row gt_left"></td>
<td headers="ri" class="gt_row gt_center">(158.428)</td>
<td headers="rs" class="gt_row gt_center">(106.040)</td></tr>
    <tr><td headers=" " class="gt_row gt_left">schoolid.escs</td>
<td headers="ri" class="gt_row gt_center"></td>
<td headers="rs" class="gt_row gt_center">324.388***</td></tr>
    <tr><td headers=" " class="gt_row gt_left" style="border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: #000000;"></td>
<td headers="ri" class="gt_row gt_center" style="border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: #000000;"></td>
<td headers="rs" class="gt_row gt_center" style="border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: #000000;">(63.893)</td></tr>
    <tr><td headers=" " class="gt_row gt_left">Nobs</td>
<td headers="ri" class="gt_row gt_center">3136</td>
<td headers="rs" class="gt_row gt_center">3136</td></tr>
    <tr><td headers=" " class="gt_row gt_left">N.pv</td>
<td headers="ri" class="gt_row gt_center">5</td>
<td headers="rs" class="gt_row gt_center">5</td></tr>
    <tr><td headers=" " class="gt_row gt_left">AICbar</td>
<td headers="ri" class="gt_row gt_center">25592462.6532503</td>
<td headers="rs" class="gt_row gt_center">25504198.0201192</td></tr>
    <tr><td headers=" " class="gt_row gt_left">BICbar</td>
<td headers="ri" class="gt_row gt_center">25592529.2109875</td>
<td headers="rs" class="gt_row gt_center">25504276.6792632</td></tr>
  </tbody>
  <tfoot class="gt_sourcenotes">
    <tr>
      <td class="gt_sourcenote" colspan="3">+ p &lt; 0.1, * p &lt; 0.05, ** p &lt; 0.01, *** p &lt; 0.001</td>
    </tr>
  </tfoot>
  
</table>
</div><!--/html_preserve-->

NOTE: After this initial post, I received a request on how the `mix` output can be used with the `modelsummary` package. To get that to work (at least for two level models), use this:

```
source("https://raw.githubusercontent.com/flh3/pubdata/main/mixPV/wemix_modelsummary.R")
```

NOTE: that `mix` function does not show the p-values (like `lmer`). The stars (in `modelsummary`) -- if just using the `mix` function (not `mivPV`) are based on p-values using a z statistic (assuming infinite degrees of freedom) which is likely not correct for higher-level predictors (as is done when using Mplus) -- so be warned about that. 

## 6. Comparing results using the `EdSurvey` package 

NOTE: This can take a while to run if downloading the datasets using the function.

The article also shows the results when using SAS proc glimmix.

```
library(EdSurvey)
# downloadPISA(years = 2012, root = "C:/", cache = TRUE)

pisa12 <- readPISA(path = "C:/EdSurveyData/PISA/2012/", 
                   countries = c("usa"))
dat <- EdSurvey::getData(data = pisa12, 
 varnames = c('st04q01', 'escs', 'sc14q02', 'st29q03',
 'math', 'w_fstuwt', 'w_fschwt', 'schoolid'),
 omittedLevels = TRUE, addAttributes = TRUE)

m1 <- mixed.sdf(formula = math ~ st29q03 + sc14q02 +
  st04q01 + escs + (1|schoolid), 
  weightVar=c("w_fstuwt", "w_fschwt"), 
  data = dat, weightTransformation = FALSE)
summary(m1)

# > summary(m1)
# Call:
#   mixed.sdf(formula = math ~ st29q03 + sc14q02 + st04q01 + escs + 
#               (1 | schoolid), data = dat, weightVars = c("w_fstuwt", "w_fschwt"), 
#             weightTransformation = FALSE)
# 
# Formula: math ~ st29q03 + sc14q02 + st04q01 + escs + (1 |
#                                                         schoolid)
# 
# Plausible Values: 5
# Number of Groups:
#   Level    Group n size mean wgt sum wgt
# 2 schoolid    157    192.1   30161
# 1      Obs   3136    713.2 2236615
# 
# Variance terms:
#   Level    Group        Name Variance Std. Error Std.Dev.
# 2 schoolid (Intercept)     1398      327.5    37.39
# 1 Residual                 5295      152.5    72.77
# 
# Fixed Effects:
#   Estimate Std. Error t value
# (Intercept)               489.507      8.408  58.218
# st29q03Agree              -11.286      5.989  -1.884
# st29q03Disagree           -19.534      6.050  -3.229
# st29q03Strongly disagree  -39.949      7.025  -5.687
# sc14q02Very little        -22.927     16.690  -1.374
# sc14q02To some extent     -17.605     11.766  -1.496
# sc14q02A lot              -30.050      7.748  -3.879
# st04q01Male                 8.395      3.103   2.706
# escs                       25.883      2.105  12.293
# 
# Intraclass Correlation= 0.209

m2 <- mixed.sdf(formula = math ~ st29q03 + sc14q02 +
   st04q01 + escs + (escs|schoolid), 
   weightVar = c("w_fstuwt", "w_fschwt"), 
   data = dat, weightTransformation = FALSE)
summary(m2)

# > summary(m2)
# Call:
#   mixed.sdf(formula = math ~ st29q03 + sc14q02 + st04q01 + escs + 
#   (escs | schoolid), data = dat, weightVars = c("w_fstuwt", 
#  "w_fschwt"), weightTransformation = FALSE)
# 
# Formula: math ~ st29q03 + sc14q02 + st04q01 + escs + (escs |
#                                                         schoolid)
# 
# Plausible Values: 5
# Number of Groups:
#   Level    Group n size mean wgt sum wgt
# 2 schoolid    157    192.1   30161
# 1      Obs   3136    713.2 2236615
# 
# Variance terms:
#   Level    Group        Name Variance Std. Error Std.Dev. Corr1
# 2 schoolid (Intercept)   1380.7     303.89    37.16      
# 2 schoolid        escs    324.4      63.53    18.01  0.22
# 1 Residual               5021.0     137.82    70.86      
# 
# Fixed Effects:
#   Estimate Std. Error t value
# (Intercept)               486.441      8.268  58.836
# st29q03Agree              -10.536      5.894  -1.788
# st29q03Disagree           -17.404      6.079  -2.863
# st29q03Strongly disagree  -36.881      7.050  -5.231
# sc14q02Very little        -23.178     15.777  -1.469
# sc14q02To some extent     -13.748     11.881  -1.157
# sc14q02A lot              -35.744      8.593  -4.160
# st04q01Male                 8.881      3.088   2.876
# escs                       27.123      2.441  11.113
# 
# Intraclass Correlation= 0.253
```


--- END



