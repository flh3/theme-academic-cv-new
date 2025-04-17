---
title: Working with missing data in large-scale assessments
author: admin
date: 2025-04-16
tags: 
  - large scale assessments
  - multilevel models
  - missing data
header:
  caption: ''
  image: ''
  preview: yes
---

This is the syntax for accounting for missing data/imputing data with
large scale assessments (with **plausible values**). This accompanies
the article:

> Huang, F., & Keller, B. (2025). Working with missing data in
> large-scale assessments. *Large-scale Assessments in Education*. doi:
> 10.1186/s40536-025-00248-9

The article is open access but you can get the updated, corrected version [here](MIwithLSA.pdf) (as of 2025.04.16).

There is a slight adjustment (for the original article) when it comes to
parallel processing (an additional package needs to be loaded). This is
using the Belgian PISA dataset. Read the article as it explains the steps.

## 1. Read in the data and load the required libraries

Download the data from the TIMSS website.

    comb <- rio::import("https://github.com/flh3/pubdata/raw/refs/heads/main/miscdata/belgium.rds") #combined original dataset
    wmiss <- rio::import("https://github.com/flh3/pubdata/raw/refs/heads/main/miscdata/belgiumwmiss.rds") #dataset with additional missing data

    library(MLMusingR) # for mixPV and summary for pooling
    library(WeMix) # for mix
    library(tidyr) # to convert from wide to long
    library(dplyr) # data management
    library(rblimp) #for creating the imputations

## 2. Fit the models

Done using the complete (90%) and missing info (82%) datasets. Done for
comparison purposes (how do the results differ when we impute the data?)

    l1a <- mixPV(pv1math + pv2math + pv3math + pv4math +
      pv5math + pv6math + pv7math + pv8math + #note correction, pv8
      pv9math + pv10math ~ gender + escs + immig2 +
      stubeha + lackstaff + (1|cntschid),
      weights = c('w_fstuwt', 'w_schgrnrabwt'),
      data = comb, mc = TRUE) 
	# can add the option mc = TRUE to make this faster (multi core)
    l1b <- mixPV(pv1math + pv2math + pv3math + pv4math +
      pv5math + pv6math + pv7math + pv8math + #note correction, pv8
      pv9math + pv10math ~ gender + escs + immig2 +
      stubeha + lackstaff + (1|cntschid),
      weights = c('w_fstuwt', 'w_schgrnrabwt'),
      data = wmiss, mc = TRUE) 

## 3. Investigate missingness

    MLMusingR::nmiss(wmiss) 

    Percent missing per variable:
          pv1math       pv2math       pv3math       pv4math       pv5math       pv6math       pv7math 
       0.00000000    0.00000000    0.00000000    0.00000000    0.00000000    0.00000000    0.00000000 
          pv8math       pv9math      pv10math       stubeha     teachbeha        gender          escs 
       0.00000000    0.00000000    0.00000000    0.03445428    0.03445428    0.00000000    0.12235988 
           immig2     lackstaff      w_fstuwt w_schgrnrabwt      cntschid 
       0.02595870    0.05699115    0.00000000    0.00000000    0.00000000 

    Percent complete cases: 0.8154572 
    (Minimum) number to impute: 18 

Perform some necessary data management:

    tall <- pivot_longer(wmiss, pv1math:pv10math, values_to = 'math')
    m <- 20 #number of datasets to impute
    ns <- nrow(wmiss) #count how many observations there are
    tall$.pv <- as.numeric(gsub("[^0-9]", "", tall$name)) #extract the 
	  # numeric value 
    nopv <- length(table(as.character(tall$.pv))) #the number of 
	  # plausible values
    tall_numeric <- mutate(tall,
      across(everything(), as.numeric) #blimp will only work with 
	  # numeric data
    ) 

## 4. Impute the data

{{% callout note %}}
Remember, you have to download and install Blimp separately (which performs the multiple imputations). You can get it here <https://www.appliedmissingdata.com/blimp>.
{{% /callout %}}


Then, you will have to install rblimp: 

    # install.packages('remotes')
    remotes::install_github('blimp-stats/rblimp')

    # set_blimp("/mnt/c/Blimp/blimp") ### NOTE:: I need this for linux
    mymodel <- rblimp(
      data = tall_numeric,
      nominal = 'gender immig2 lackstaff',
      # ordinal = '',
      clusterid = 'cntschid',
      fixed = 'gender w_fstuwt', 
      model = 'math ~ gender escs immig2 
               stubeha lackstaff w_fstuwt', 
      options = 'labels',
      seed = 1234,
      nimps = ) |> 
      by_group('.pv') 

Again, read the article which explains the different options. This will take a while. Suggest using 2 in the `nimps` option to start
and see if everything works properly first. The `by_group` option is
important when there are plausible values.

After imputing, inspect the rhat values (see article)

    mymodel |> sapply(\(x) tail(x@psr, 1) |> max(na.rm = TRUE))

         1      2      3      4      5      6      7      8      9     10 
    1.0480 1.0394 1.0401 1.0959 1.0312 1.0609 1.0489 1.0337 1.0401 1.0610 

    # lapply(mymodel, psr)

## 5. Extract the data

And perform some data management.

    impdat <- lapply(mymodel, as.mitml) |>
      unlist(recursive = FALSE) |>
      do.call(rbind, args = _)

    impdat$.implist <- rep(1:(m * nopv), each = ns)
    ## Return factor attributes to the data
    table(impdat$lackstaff) #no attributes


         1      2      3      4 
    250958 720793 576658 146591 

    attributes(impdat$lackstaff) <- attributes(comb$lackstaff)
    table(impdat$lackstaff) #now with attributes

        Not at all    Very little To some extent          A lot 
            250958         720793         576658         146591 

    ## do this for the other variables that should be categorical
    attributes(impdat$gender) <- attributes(comb$gender)
    attributes(impdat$immig2) <- attributes(comb$immig2)

    ## Create a list for analysis
    alldat <- split(impdat, impdat$.implist)

## 6. Analyze the data

Specify the model:

    modfit <- function(x) {
      mix(math ~ gender + escs + immig2 + stubeha +
      lackstaff + (1|cntschid),
      weights = c('w_fstuwt', 'w_schgrnrabwt'),
      data = x)
    }

Fit the model, two ways:

Method 1: Serial operation (slow)

    # allres <- lapply(alldat, FUN = modfit)

Method 2: Parallel computation (much faster but still requires around 5
minutes)

    library(parallel)
    library(doParallel) ### NEED THIS!
    cl <- makeCluster(detectCores() - 1)
    registerDoParallel(cl)
    clusterExport(cl, list('modfit', 'mix'))
    allres <- parLapply(cl, alldat, fun = modfit)
    stopCluster(cl)

## 7 Pool results

Need to change the class for the `summary` function to work:

    class(allres) <- 'mixPV'
    summary(allres)

    Results of multilevel analyses with 200 plausible values.
    Number of observations: 8475 

    Estimates for random effects: 
                         estimate std.error statistic   df Pr(>t)    
    cntschid.(Intercept)  2878.60    377.84      7.62 1782 <2e-16 ***
    Residual              4353.26    163.94     26.55  454 <2e-16 ***
    ---
    Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

    Estimates for fixed effects: 
                            estimate std.error statistic   df Pr(>t)    
    (Intercept)               496.88     10.74     46.28 4252 <2e-16 ***
    genderFemale              -19.50      2.31     -8.42  950 <2e-16 ***
    escs                       18.88      1.72     10.95  485 <2e-16 ***
    immig2Second-Generation   -22.31      3.62     -6.16 1747 <2e-16 ***
    immig2First-Generation    -26.35      4.60     -5.73 1330 <2e-16 ***
    stubeha                   -37.68      5.54     -6.81 5192 <2e-16 ***
    lackstaffVery little       28.12     13.97      2.01 4055  0.044 *  
    lackstaffTo some extent     9.23     14.20      0.65 4573  0.516    
    lackstaffA lot             21.61     17.98      1.20 2498  0.229    
    ---
    Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

Comparing results side by side:

    source("https://raw.githubusercontent.com/flh3/pubdata/refs/heads/main/mixPV/wemix_modelsummary.R")
    library(modelsummary)
    modelsummary(list('90% comp' = l1a, '82% comp' = l1b, 'MI' = allres), stars = TRUE)

<table style="width:98%;">
<colgroup>
<col style="width: 30%" />
<col style="width: 22%" />
<col style="width: 22%" />
<col style="width: 22%" />
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
<td>499.886***</td>
<td>520.934***</td>
<td>496.875***</td>
</tr>
<tr class="even">
<td></td>
<td>(10.312)</td>
<td>(9.069)</td>
<td>(10.737)</td>
</tr>
<tr class="odd">
<td>genderFemale</td>
<td>-20.890***</td>
<td>-18.840***</td>
<td>-19.495***</td>
</tr>
<tr class="even">
<td></td>
<td>(2.428)</td>
<td>(2.458)</td>
<td>(2.315)</td>
</tr>
<tr class="odd">
<td>escs</td>
<td>17.674***</td>
<td>16.991***</td>
<td>18.878***</td>
</tr>
<tr class="even">
<td></td>
<td>(1.510)</td>
<td>(1.625)</td>
<td>(1.724)</td>
</tr>
<tr class="odd">
<td>immig2Second-Generation</td>
<td>-22.650***</td>
<td>-21.346***</td>
<td>-22.306***</td>
</tr>
<tr class="even">
<td></td>
<td>(3.872)</td>
<td>(4.086)</td>
<td>(3.622)</td>
</tr>
<tr class="odd">
<td>immig2First-Generation</td>
<td>-27.046***</td>
<td>-24.955***</td>
<td>-26.349***</td>
</tr>
<tr class="even">
<td></td>
<td>(4.877)</td>
<td>(4.972)</td>
<td>(4.598)</td>
</tr>
<tr class="odd">
<td>stubeha</td>
<td>-36.355***</td>
<td>-32.648***</td>
<td>-37.677***</td>
</tr>
<tr class="even">
<td></td>
<td>(5.522)</td>
<td>(4.961)</td>
<td>(5.536)</td>
</tr>
<tr class="odd">
<td>lackstaffVery little</td>
<td>27.385*</td>
<td>12.016</td>
<td>28.116*</td>
</tr>
<tr class="even">
<td></td>
<td>(13.242)</td>
<td>(12.071)</td>
<td>(13.967)</td>
</tr>
<tr class="odd">
<td>lackstaffTo some extent</td>
<td>9.347</td>
<td>-2.166</td>
<td>9.227</td>
</tr>
<tr class="even">
<td></td>
<td>(13.758)</td>
<td>(11.475)</td>
<td>(14.198)</td>
</tr>
<tr class="odd">
<td>lackstaffA lot</td>
<td>22.942</td>
<td>8.459</td>
<td>21.613</td>
</tr>
<tr class="even">
<td></td>
<td>(15.653)</td>
<td>(14.019)</td>
<td>(17.975)</td>
</tr>
<tr class="odd">
<td>cntschid.(Intercept)</td>
<td>2714.714***</td>
<td>2137.460***</td>
<td>2878.599***</td>
</tr>
<tr class="even">
<td></td>
<td>(362.636)</td>
<td>(370.142)</td>
<td>(377.836)</td>
</tr>
<tr class="odd">
<td>Residual</td>
<td>4385.549***</td>
<td>3885.879***</td>
<td>4353.262***</td>
</tr>
<tr class="even">
<td></td>
<td>(179.614)</td>
<td>(165.660)</td>
<td>(163.942)</td>
</tr>
<tr class="odd">
<td>Nobs</td>
<td>7676</td>
<td>6911</td>
<td>8475</td>
</tr>
<tr class="even">
<td>N.pv</td>
<td>10</td>
<td>10</td>
<td>200</td>
</tr>
<tr class="odd">
<td>AICbar</td>
<td>1195567.29545046</td>
<td>1061170.157846</td>
<td>1329601.33171994</td>
</tr>
<tr class="even">
<td>BICbar</td>
<td>1195643.69984289</td>
<td>1061245.40741186</td>
<td>1329678.8253552</td>
</tr>
</tbody><tfoot>
<tr class="odd">
<td colspan="4"><ul>
<li>p &lt; 0.1, * p &lt; 0.05, ** p &lt; 0.01, *** p &lt; 0.001</li>
</ul></td>
</tr>
</tfoot>
&#10;</table>

END
