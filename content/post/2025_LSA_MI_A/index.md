---
title: Working with missing data in large-scale assessments (without plausible  values)
author: admin
date: 2025-04-17
tags: 
  - large scale assessments
  - multilevel models
  - missing data
header:
  caption: ''
  image: ''
  preview: yes
draft: true
---

This is the syntax for accounting for missing data/imputing data with
large scale assessments (without **plausible values**). This is Appendix
A and accompanies the article:

> Huang, F., & Keller, B. (2025). Working with missing data in
> large-scale assessments. *Large-scale Assessments in Education*. doi:
> 10.1186/s40536-025-00248-9

The syntax for fitting the models with plausible values can be found
[here](https://francish.net/post/2025_lsa_mi/). The article is open
access but you can get the updated, corrected version
[here](The%20article%20is%20open%20access%20but%20you%20can%20get%20the%20updated,%20corrected%20version%20here%20(as%20of%202025.04.16).)
(as of 2025.04.16).

The following example is if multiple plausible values are *not* going to
be used (e.g., some scale as the outcome, belonging, etc.). **For this
example, only one plausible value is going to be used (only as an
example for a continuous variable).** Researchers may be interested in
predicting some outcome like bullying or school belonging that does not
use plausible values. If users really want to use a variable that has
plausible value as an outcome, **ALL** of the values should be used
properly. See full article for the syntax for multiple imputation using
all the plausible values. To learn more about plausible values, see [here](https://francish.net/post/pv3/).

### Load in the required packages

    library(dplyr) #for data management
    library(rblimp) #for imputing
    library(WeMix) #for fitting models
    library(doParallel) #for parallel computing
    library(modelsummary) #to output models side by side
    library(tidyr) #to convert to tall format
    library(MLMusingR) #for the mixPV function
	
### 1. LOAD DATA

The following example uses the Belgian 2018 PISA dataset.

A dataset is saved into the `comb` object (this has 90% complete data):

    # source("https://raw.githubusercontent.com/flh3/pubdata/main/mixPV/mixPVv2.R") #if MLMusingR is not installed
    comb <- rio::import("https://github.com/flh3/pubdata/raw/refs/heads/main/miscdata/belgium.rds",
     trust = TRUE)

Afterwards, the model of interest is fit:

    ## using dataset as-is
    l1a <- mix(pv1math ~ gender + escs + immig2 + (1|cntschid) +
      stubeha + lackstaff,
      weights = c('w_fstuwt', 'w_schgrnrabwt'),
      data = comb)

    Warning in mix(pv1math ~ gender + escs + immig2 + (1 | cntschid) + stubeha +
    : There were 799 rows with missing data. These have been removed.

    summary(l1a) # minimal missing data

    Call:
    mix(formula = pv1math ~ gender + escs + immig2 + (1 | cntschid) + 
        stubeha + lackstaff, data = comb, weights = c("w_fstuwt", 
        "w_schgrnrabwt"))

    Variance terms:
     Level    Group        Name Variance Std. Error Std.Dev.
         2 cntschid (Intercept)     2548      275.9    50.48
         1 Residual                 4463      112.3    66.81
    Groups:
     Level    Group n size mean wgt sum wgt
         2 cntschid    270    5.791    1564
         1      Obs   7676   13.817  106058

    Fixed Effects:
                            Estimate Std. Error t value
    (Intercept)              499.148     10.483  47.617
    genderFemale             -20.906      1.899 -11.008
    escs                      18.054      1.216  14.845
    immig2Second-Generation  -20.033      3.492  -5.737
    immig2First-Generation   -25.541      3.759  -6.795
    stubeha                  -35.424      5.414  -6.543
    lackstaffVery little      31.221     12.900   2.420
    lackstaffTo some extent   11.141     13.851   0.804
    lackstaffA lot            19.328     15.533   1.244

    lnl= -598666.14 
    Intraclass Correlation= 0.3634 

A second dataset- `wmiss` is loaded which has additional missing values
in the `escs` variable:

    # this dataset has more missing data
    wmiss <- rio::import("https://github.com/flh3/pubdata/raw/refs/heads/main/miscdata/belgiumwmiss.rds",
     trust = TRUE)
    MLMusingR::nmiss(wmiss) # how much missing data?

    Percent missing per variable:
          pv1math       pv2math       pv3math       pv4math       pv5math 
       0.00000000    0.00000000    0.00000000    0.00000000    0.00000000 
          pv6math       pv7math       pv8math       pv9math      pv10math 
       0.00000000    0.00000000    0.00000000    0.00000000    0.00000000 
          stubeha     teachbeha        gender          escs        immig2 
       0.03445428    0.03445428    0.00000000    0.12235988    0.02595870 
        lackstaff      w_fstuwt w_schgrnrabwt      cntschid 
       0.05699115    0.00000000    0.00000000    0.00000000 

    Percent complete cases: 0.8154572 
    (Minimum) number to impute: 18 

    l1b <- mix(pv1math ~ gender + escs + immig2 + (1|cntschid) +
     stubeha + lackstaff,
     weights = c('w_fstuwt', 'w_schgrnrabwt'),
     data = wmiss)

    Warning in mix(pv1math ~ gender + escs + immig2 + (1 | cntschid) + stubeha +
    : There were 1564 rows with missing data. These have been removed.

    summary(l1b)

    Call:
    mix(formula = pv1math ~ gender + escs + immig2 + (1 | cntschid) + 
        stubeha + lackstaff, data = wmiss, weights = c("w_fstuwt", 
        "w_schgrnrabwt"))

    Variance terms:
     Level    Group        Name Variance Std. Error Std.Dev.
         2 cntschid (Intercept)     1701     200.06    41.24
         1 Residual                 3671      80.92    60.59
    Groups:
     Level    Group n size mean wgt sum wgt
         2 cntschid    268    5.729    1535
         1      Obs   6911   13.768   95153

    Fixed Effects:
                            Estimate Std. Error t value
    (Intercept)              524.990      7.739  67.835
    genderFemale             -17.952      1.778 -10.098
    escs                      16.946      1.248  13.580
    immig2Second-Generation  -18.519      3.288  -5.632
    immig2First-Generation   -22.359      3.587  -6.233
    stubeha                  -29.170      3.876  -7.526
    lackstaffVery little      11.851     10.241   1.157
    lackstaffTo some extent   -2.132      9.863  -0.216
    lackstaffA lot             2.408     11.286   0.213

    lnl= -527776.77 
    Intraclass Correlation= 0.3167 

### 2. IMPUTING

Prior to imputing the data, we need to get our data ready.

    ns <- nrow(wmiss) #how many observations 
    m <- 20 #number of imputations

{{% callout note %}}
UPDATE: rblimp used to require all numeric variables only (at the time of acceptance of the manuscript). However, this has been updated (in version 0.2.7) and there is no need to convert factors into numeric variables anymore. We can use the `wmiss` dataset as-is and this is used in the imputation.
{{% /callout %}}

We are now ready to impute 20 datasets. We do not need to do this per
plausible value. 

{{% callout note %}}
Remember, you have to download and install Blimp separately (which performs the multiple imputations). You can get it here <https://www.appliedmissingdata.com/blimp>.
{{% /callout %}}

The `rblimp` package also needs to be installed using:
`remotes::install_github('blimp-stats/rblimp')`

    mymodel2 <- rblimp(
      data = wmiss, #this is different from the manuscript
      nominal = 'gender immig2 lackstaff',
      # ordinal = '',
      clusterid = 'cntschid',
      fixed = 'gender w_fstuwt', 
      model = 'pv1math ~ gender escs immig2 
               stubeha lackstaff w_fstuwt', 
      options = 'labels',
      seed = 1234,
      nimps = m
    )

This takes some time. A lot of output is printed (not shown). We can
check the PSR values.

    mymodel2@psr %>% tail(1) %>% max(., na.rm = TRUE)

    [1] 1.048

This seems fine (value is &lt; 1.1). We can then proceed to create the
stacked dataset (called `tmp`).

    tmp <- as.mitml(mymodel2) |>
      do.call(rbind, args = _)

We create an imputation counter:

    tmp$.implist <- rep(1:m, each = ns)

{{% callout note %}}
UPDATE: The original manuscript required copying the variable attributes of factors back into the numeric variables. This is not needed anymore with the latest version of `rblimp`.
{{% /callout %}}

We can check how many observations there are per imputation (we have 20
imputations):

    table(tmp$.implist)


       1    2    3    4    5    6    7    8    9   10   11   12   13   14   15 
    8475 8475 8475 8475 8475 8475 8475 8475 8475 8475 8475 8475 8475 8475 8475 
      16   17   18   19   20 
    8475 8475 8475 8475 8475 

Afterwards, we can create a list with the 20 datasets for final
analysis:

    alld2 <- split(tmp, tmp$.implist)

### 3. FITTING THE MODELS

We will use parallel processing here to speed up the computation:

First, write out our function. This is where you will modify the model
to fit what the substantive (or analytic) model of interest is:

    fnc <- function(x) {
      mix(pv1math ~ gender + escs + immig2 + stubeha +
          lackstaff  + (1|cntschid),
      weights = c('w_fstuwt', 'w_schgrnrabwt'),
      data = x)
    }

Here, `x` is a placeholder.

We use the following statements to fit the `fnc` function above to our
imputed datasets:

    ## fitting models using multiple cores
    st <- Sys.time() #optional
    cl <- makeCluster(detectCores() - 1) #use all cores less one so the computer can still be used for other things...
    registerDoParallel(cl)
    clusterExport(cl, list('fnc', 'mix')) #need to specify the functiosn to use
    mires <- parLapply(cl, alld2, fun = fnc)
    Sys.time() - st #optional

    Time difference of 49.27839 secs

    stopCluster(cl)

Afterwards, we need to pool results. This is done using the `summary`
function in the `mixPV` function. We need to have the output as a
`mivPV` class (although this was fit with `mix`), to “fool” the
`summary` function to combine the results using Rubin’s rules:

    class(mires) <- 'mixPV'
    summary(mires) #this does the pooling 

    Results of multilevel analyses with 20 plausible values.
    Number of observations: 8475 

    Estimates for random effects: 
                         estimate std.error statistic   df Pr(>t)    
    cntschid.(Intercept)  2721.34    318.77      8.54 1992 <2e-16 ***
    Residual              4423.45    103.34     42.81 7676 <2e-16 ***
    ---
    Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

    Estimates for fixed effects: 
                            estimate std.error statistic   df Pr(>t)    
    (Intercept)              496.620    10.838    45.821 1205 <2e-16 ***
    genderFemale             -19.312     1.848   -10.450 8055 <2e-16 ***
    escs                      20.044     1.277    15.701  507 <2e-16 ***
    immig2Second-Generation  -19.260     3.244    -5.937 3847 <2e-16 ***
    immig2First-Generation   -24.198     3.492    -6.929 6472 <2e-16 ***
    stubeha                  -35.947     5.250    -6.847 4331 <2e-16 ***
    lackstaffVery little      30.873    13.690     2.255  708  0.024 *  
    lackstaffTo some extent    8.356    14.465     0.578 1006  0.564    
    lackstaffA lot            17.722    17.473     1.014  495  0.311    
    ---
    Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

We can compare results side by side using `modelsummary`. Need a
function so `mix` objects will display too (that’s why the `source`
function is used below):

    source("https://raw.githubusercontent.com/flh3/pubdata/refs/heads/main/mixPV/wemix_modelsummary.R")
    modelsummary(list('90% comp' = l1a, '82% comp' = l1b, 'MI' = mires), stars = TRUE)

<table style="width:98%;">
<colgroup>
<col style="width: 29%" />
<col style="width: 22%" />
<col style="width: 22%" />
<col style="width: 21%" />
</colgroup>
<thead>
<tr class="header">
<th></th>
<th>90% comp</th>
<th>82% comp</th>
<th>MI</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td>(Intercept)</td>
<td>499.148***</td>
<td>524.990***</td>
<td>496.620***</td>
</tr>
<tr class="even">
<td></td>
<td>(10.483)</td>
<td>(7.739)</td>
<td>(10.838)</td>
</tr>
<tr class="odd">
<td>genderFemale</td>
<td>-20.906***</td>
<td>-17.952***</td>
<td>-19.312***</td>
</tr>
<tr class="even">
<td></td>
<td>(1.899)</td>
<td>(1.778)</td>
<td>(1.848)</td>
</tr>
<tr class="odd">
<td>escs</td>
<td>18.054***</td>
<td>16.946***</td>
<td>20.044***</td>
</tr>
<tr class="even">
<td></td>
<td>(1.216)</td>
<td>(1.248)</td>
<td>(1.277)</td>
</tr>
<tr class="odd">
<td>immig2Second-Generation</td>
<td>-20.033***</td>
<td>-18.519***</td>
<td>-19.260***</td>
</tr>
<tr class="even">
<td></td>
<td>(3.492)</td>
<td>(3.288)</td>
<td>(3.244)</td>
</tr>
<tr class="odd">
<td>immig2First-Generation</td>
<td>-25.541***</td>
<td>-22.359***</td>
<td>-24.198***</td>
</tr>
<tr class="even">
<td></td>
<td>(3.759)</td>
<td>(3.587)</td>
<td>(3.492)</td>
</tr>
<tr class="odd">
<td>stubeha</td>
<td>-35.424***</td>
<td>-29.170***</td>
<td>-35.947***</td>
</tr>
<tr class="even">
<td></td>
<td>(5.414)</td>
<td>(3.876)</td>
<td>(5.250)</td>
</tr>
<tr class="odd">
<td>lackstaffVery little</td>
<td>31.221*</td>
<td>11.851</td>
<td>30.873*</td>
</tr>
<tr class="even">
<td></td>
<td>(12.900)</td>
<td>(10.241)</td>
<td>(13.690)</td>
</tr>
<tr class="odd">
<td>lackstaffTo some extent</td>
<td>11.141</td>
<td>-2.132</td>
<td>8.356</td>
</tr>
<tr class="even">
<td></td>
<td>(13.851)</td>
<td>(9.863)</td>
<td>(14.465)</td>
</tr>
<tr class="odd">
<td>lackstaffA lot</td>
<td>19.328</td>
<td>2.408</td>
<td>17.722</td>
</tr>
<tr class="even">
<td></td>
<td>(15.533)</td>
<td>(11.286)</td>
<td>(17.473)</td>
</tr>
<tr class="odd">
<td>cntschid.(Intercept)</td>
<td>2547.812***</td>
<td>1701.117***</td>
<td>2721.342***</td>
</tr>
<tr class="even">
<td></td>
<td>(275.856)</td>
<td>(200.061)</td>
<td>(318.770)</td>
</tr>
<tr class="odd">
<td>Residual</td>
<td>4463.001***</td>
<td>3670.943***</td>
<td>4423.448***</td>
</tr>
<tr class="even">
<td></td>
<td>(112.308)</td>
<td>(80.921)</td>
<td>(103.337)</td>
</tr>
<tr class="odd">
<td>Nobs</td>
<td>7676</td>
<td>6911</td>
<td>8475</td>
</tr>
<tr class="even">
<td>lnl</td>
<td>-598666.141908678</td>
<td>-527776.771623781</td>
<td></td>
</tr>
<tr class="odd">
<td>N.grps</td>
<td>270</td>
<td>268</td>
<td></td>
</tr>
<tr class="even">
<td>icc.x</td>
<td>0.363411731755597</td>
<td>0.31666002745425</td>
<td></td>
</tr>
<tr class="odd">
<td>N.pv</td>
<td></td>
<td></td>
<td>20</td>
</tr>
<tr class="even">
<td>AICbar</td>
<td></td>
<td></td>
<td>1331427.46293097</td>
</tr>
<tr class="odd">
<td>BICbar</td>
<td></td>
<td></td>
<td>1331504.95656622</td>
</tr>
</tbody><tfoot>
<tr class="even">
<td colspan="4"><ul>
<li>p &lt; 0.1, * p &lt; 0.05, ** p &lt; 0.01, *** p &lt; 0.001</li>
</ul></td>
</tr>
</tfoot>
&#10;</table>

Results of the first and third models are similar. Model 2, with
additional missing data in `escs` have different results– for the
level-2 coefficients (see `lackstaff` results).

-- END
