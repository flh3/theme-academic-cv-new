---
title: "Selecting the proper weights in LSAs with multilevel models"
author: "Francis Huang"
date: 2024-09-01
output:
  html_document:
    df_print: paged
categories:
- large scale datasets
- weights
tags:
- large scale assessments
- weights
header:
  caption: ''
  image: ''
  preview: yes
slug: lsawt
---



# Which weights to use with multilevel models?

A common question with the use of large-scale assessments (LSAs) is related to the use of weights. Another issue is how to specify these weights properly. 

Software such as SAS and Mplus, when specifying weights at two levels, require the use of conditional weights at level 1 if the level-2 weight is specified (or you can just use the level-2 weights alone; see Mang et al., 2021, see bottom part of this post). 

To illustrate this using R (with the `mixPV` function):

## 1. Load in the needed functions and data

First, load in the `mixPV` function. Load in the `pisa2012` dataset as well.


``` r
library(modelsummary)
source("https://raw.githubusercontent.com/flh3/pubdata/main/mixPV/mixPVv2.R")
data(pisa2012, package = 'MLMusingR') 
```

By default, the `WeMix` package uses conditional weights = FALSE or `cWeights = FALSE` (no need to specify). This makes it easier so users don't have to compute the conditional weights (conditional on the school being selected) and can just use the total weight supplied in many datasets. You have to make sure which type of weight you are using. NOTE: the `mc = TRUE` option is used to shorten computing time (turning on the use of multiple cores). The following just uses the raw weights at both levels:


``` r
m1a <- mixPV(pv1math + pv2math + pv3math + pv4math + pv5math ~ 
               sc14q02 + st04q01 + escs + (1|schoolid), 
             weights = c('w_fstuwt', 'w_fschwt'), mc = TRUE,
             data = pisa2012)
```

```
Attempting to use 15 cores. Progress will not be displayed. Please wait.
```

However, if you use software such as Mplus or SAS, you need to specify the conditional weights. To compute the conditional weights (if you are specifying weights at both levels), it is the total weight divided by the school weight. 


``` r
pisa2012$cweight <- pisa2012$w_fstuwt / pisa2012$w_fschwt
```

We can then use this when we fit the model and we include `cWeights = TRUE`. We can compare the output in a while.


``` r
m1b <- mixPV(pv1math + pv2math + pv3math + pv4math + pv5math ~ 
               sc14q02 + st04q01 + escs + (1|schoolid), 
             weights = c('cweight', 'w_fschwt'), mc = TRUE,
             cWeight = TRUE,
             data = pisa2012)
```

```
Attempting to use 15 cores. Progress will not be displayed. Please wait.
```

I will include the example if we just use the conditional weight (as you would with other software packages) but DO NOT turn on the `cWeights` option.


``` r
m1c <- mixPV(pv1math + pv2math + pv3math + pv4math + pv5math ~ 
               sc14q02 + st04q01 + escs + (1|schoolid), 
             weights = c('cweight', 'w_fschwt'), mc = TRUE,
             #cWeight = TRUE,  ## for illustrative purposes only
             data = pisa2012)
```

```
Attempting to use 15 cores. Progress will not be displayed. Please wait.
```

We can compare results side-by-side.


``` r
modelsummary(list("default" = m1a, "cweights" = m1b, "wrong" = m1c),
  stars = TRUE, gof_map = NA, output = 'gt')
```

<!--html_preserve--><div id="fjhjfswqvu" style="padding-left:0px;padding-right:0px;padding-top:10px;padding-bottom:10px;overflow-x:auto;overflow-y:auto;width:auto;height:auto;">
<style>#fjhjfswqvu table {
  font-family: system-ui, 'Segoe UI', Roboto, Helvetica, Arial, sans-serif, 'Apple Color Emoji', 'Segoe UI Emoji', 'Segoe UI Symbol', 'Noto Color Emoji';
  -webkit-font-smoothing: antialiased;
  -moz-osx-font-smoothing: grayscale;
}

#fjhjfswqvu thead, #fjhjfswqvu tbody, #fjhjfswqvu tfoot, #fjhjfswqvu tr, #fjhjfswqvu td, #fjhjfswqvu th {
  border-style: none;
}

#fjhjfswqvu p {
  margin: 0;
  padding: 0;
}

#fjhjfswqvu .gt_table {
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

#fjhjfswqvu .gt_caption {
  padding-top: 4px;
  padding-bottom: 4px;
}

#fjhjfswqvu .gt_title {
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

#fjhjfswqvu .gt_subtitle {
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

#fjhjfswqvu .gt_heading {
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

#fjhjfswqvu .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#fjhjfswqvu .gt_col_headings {
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

#fjhjfswqvu .gt_col_heading {
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

#fjhjfswqvu .gt_column_spanner_outer {
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

#fjhjfswqvu .gt_column_spanner_outer:first-child {
  padding-left: 0;
}

#fjhjfswqvu .gt_column_spanner_outer:last-child {
  padding-right: 0;
}

#fjhjfswqvu .gt_column_spanner {
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

#fjhjfswqvu .gt_spanner_row {
  border-bottom-style: hidden;
}

#fjhjfswqvu .gt_group_heading {
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

#fjhjfswqvu .gt_empty_group_heading {
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

#fjhjfswqvu .gt_from_md > :first-child {
  margin-top: 0;
}

#fjhjfswqvu .gt_from_md > :last-child {
  margin-bottom: 0;
}

#fjhjfswqvu .gt_row {
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

#fjhjfswqvu .gt_stub {
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

#fjhjfswqvu .gt_stub_row_group {
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

#fjhjfswqvu .gt_row_group_first td {
  border-top-width: 2px;
}

#fjhjfswqvu .gt_row_group_first th {
  border-top-width: 2px;
}

#fjhjfswqvu .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#fjhjfswqvu .gt_first_summary_row {
  border-top-style: solid;
  border-top-color: #D3D3D3;
}

#fjhjfswqvu .gt_first_summary_row.thick {
  border-top-width: 2px;
}

#fjhjfswqvu .gt_last_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#fjhjfswqvu .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#fjhjfswqvu .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}

#fjhjfswqvu .gt_last_grand_summary_row_top {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: double;
  border-bottom-width: 6px;
  border-bottom-color: #D3D3D3;
}

#fjhjfswqvu .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}

#fjhjfswqvu .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#fjhjfswqvu .gt_footnotes {
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

#fjhjfswqvu .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}

#fjhjfswqvu .gt_sourcenotes {
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

#fjhjfswqvu .gt_sourcenote {
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}

#fjhjfswqvu .gt_left {
  text-align: left;
}

#fjhjfswqvu .gt_center {
  text-align: center;
}

#fjhjfswqvu .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}

#fjhjfswqvu .gt_font_normal {
  font-weight: normal;
}

#fjhjfswqvu .gt_font_bold {
  font-weight: bold;
}

#fjhjfswqvu .gt_font_italic {
  font-style: italic;
}

#fjhjfswqvu .gt_super {
  font-size: 65%;
}

#fjhjfswqvu .gt_footnote_marks {
  font-size: 75%;
  vertical-align: 0.4em;
  position: initial;
}

#fjhjfswqvu .gt_asterisk {
  font-size: 100%;
  vertical-align: 0;
}

#fjhjfswqvu .gt_indent_1 {
  text-indent: 5px;
}

#fjhjfswqvu .gt_indent_2 {
  text-indent: 10px;
}

#fjhjfswqvu .gt_indent_3 {
  text-indent: 15px;
}

#fjhjfswqvu .gt_indent_4 {
  text-indent: 20px;
}

#fjhjfswqvu .gt_indent_5 {
  text-indent: 25px;
}

#fjhjfswqvu .katex-display {
  display: inline-flex !important;
  margin-bottom: 0.75em !important;
}

#fjhjfswqvu div.Reactable > div.rt-table > div.rt-thead > div.rt-tr.rt-tr-group-header > div.rt-th-group:after {
  height: 0px !important;
}
</style>
<table class="gt_table" data-quarto-disable-processing="false" data-quarto-bootstrap="false">
  <thead>
    <tr class="gt_col_headings">
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1" scope="col" id=" "> </th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="default">default</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="cweights">cweights</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="wrong">wrong</th>
    </tr>
  </thead>
  <tbody class="gt_table_body">
    <tr><td headers=" " class="gt_row gt_left">(Intercept)</td>
<td headers="default" class="gt_row gt_center">470.498***</td>
<td headers="cweights" class="gt_row gt_center">470.498***</td>
<td headers="wrong" class="gt_row gt_center">479.535***</td></tr>
    <tr><td headers=" " class="gt_row gt_left"></td>
<td headers="default" class="gt_row gt_center">(7.103)</td>
<td headers="cweights" class="gt_row gt_center">(7.103)</td>
<td headers="wrong" class="gt_row gt_center">(4.876)</td></tr>
    <tr><td headers=" " class="gt_row gt_left">sc14q02Very little</td>
<td headers="default" class="gt_row gt_center">-20.910</td>
<td headers="cweights" class="gt_row gt_center">-20.910</td>
<td headers="wrong" class="gt_row gt_center">-8.561</td></tr>
    <tr><td headers=" " class="gt_row gt_left"></td>
<td headers="default" class="gt_row gt_center">(15.710)</td>
<td headers="cweights" class="gt_row gt_center">(15.710)</td>
<td headers="wrong" class="gt_row gt_center">(6.421)</td></tr>
    <tr><td headers=" " class="gt_row gt_left">sc14q02To some extent</td>
<td headers="default" class="gt_row gt_center">-16.131</td>
<td headers="cweights" class="gt_row gt_center">-16.131</td>
<td headers="wrong" class="gt_row gt_center">-27.209**</td></tr>
    <tr><td headers=" " class="gt_row gt_left"></td>
<td headers="default" class="gt_row gt_center">(11.823)</td>
<td headers="cweights" class="gt_row gt_center">(11.823)</td>
<td headers="wrong" class="gt_row gt_center">(8.611)</td></tr>
    <tr><td headers=" " class="gt_row gt_left">sc14q02A lot</td>
<td headers="default" class="gt_row gt_center">-25.224**</td>
<td headers="cweights" class="gt_row gt_center">-25.224**</td>
<td headers="wrong" class="gt_row gt_center">-23.765***</td></tr>
    <tr><td headers=" " class="gt_row gt_left"></td>
<td headers="default" class="gt_row gt_center">(7.614)</td>
<td headers="cweights" class="gt_row gt_center">(7.614)</td>
<td headers="wrong" class="gt_row gt_center">(6.635)</td></tr>
    <tr><td headers=" " class="gt_row gt_left">st04q01Male</td>
<td headers="default" class="gt_row gt_center">9.529**</td>
<td headers="cweights" class="gt_row gt_center">9.529**</td>
<td headers="wrong" class="gt_row gt_center">10.784**</td></tr>
    <tr><td headers=" " class="gt_row gt_left"></td>
<td headers="default" class="gt_row gt_center">(3.097)</td>
<td headers="cweights" class="gt_row gt_center">(3.097)</td>
<td headers="wrong" class="gt_row gt_center">(4.163)</td></tr>
    <tr><td headers=" " class="gt_row gt_left">escs</td>
<td headers="default" class="gt_row gt_center">26.300***</td>
<td headers="cweights" class="gt_row gt_center">26.300***</td>
<td headers="wrong" class="gt_row gt_center">35.847***</td></tr>
    <tr><td headers=" " class="gt_row gt_left"></td>
<td headers="default" class="gt_row gt_center">(2.210)</td>
<td headers="cweights" class="gt_row gt_center">(2.210)</td>
<td headers="wrong" class="gt_row gt_center">(2.158)</td></tr>
    <tr><td headers=" " class="gt_row gt_left">schoolid.(Intercept)</td>
<td headers="default" class="gt_row gt_center">1344.166***</td>
<td headers="cweights" class="gt_row gt_center">1344.166***</td>
<td headers="wrong" class="gt_row gt_center">0.000</td></tr>
    <tr><td headers=" " class="gt_row gt_left"></td>
<td headers="default" class="gt_row gt_center">(300.241)</td>
<td headers="cweights" class="gt_row gt_center">(300.241)</td>
<td headers="wrong" class="gt_row gt_center">(0.000)</td></tr>
    <tr><td headers=" " class="gt_row gt_left">Residual</td>
<td headers="default" class="gt_row gt_center">5418.299***</td>
<td headers="cweights" class="gt_row gt_center">5418.299***</td>
<td headers="wrong" class="gt_row gt_center">6701.321***</td></tr>
    <tr><td headers=" " class="gt_row gt_left"></td>
<td headers="default" class="gt_row gt_center">(160.102)</td>
<td headers="cweights" class="gt_row gt_center">(160.102)</td>
<td headers="wrong" class="gt_row gt_center">(210.501)</td></tr>
  </tbody>
  <tfoot class="gt_sourcenotes">
    <tr>
      <td class="gt_sourcenote" colspan="4">+ p &lt; 0.1, * p &lt; 0.05, ** p &lt; 0.01, *** p &lt; 0.001</td>
    </tr>
  </tfoot>
  
</table>
</div><!--/html_preserve-->

The first two models have identical results. The third model (which is incorrectly specified) results differ and the level-2 variance here is zero. 

## 2. What about scaling level-1 weights?

Asparouhov (2005) suggested scaling the weights at level 1. The following formulas (and explanations) can be seen at the Mplus website at https://statmodel.com/download/Scaling3.pdf.

The unscaled weight is $w^*_{ij}=w_{ij}$.

What Mplus calls the cluster weight has scaled weights that add up to the cluster sample size. Carle (2009) refers to this as Method A. It is computed using:

$w^*_{ij}=w_{ij}\frac{n_j}{\Sigma_i w_{ij}}$

Here, $n_j$ is the number of sample units in cluster $j$. This is the default scaling method in Mplus and has generally been shown to perform well (see Asparouhov, 2006).

Another scaling approach is referred to as the ecluster method which is the effective cluster sample size method. This is referred to as Method B by Carle (2009) (though the formula in his appendix seems to differ slightly):

This is similar to the cluster method except that the numerator is $n^*_j$ instead and is:

$w^*_{ij}=w_{ij}\frac{n^*_j}{\Sigma_i w_{ij}}$.

The numerator is computed using:

$n^*_{j}=\frac{(\Sigma_i w_{ij})^2}{\Sigma_i w^2_{ij}}$.


To compute the scaled weights, we can use the `wscale` function (note the options). The cluster variable, dataset, conditional cluster weight,
and type of scaling need to be specified:


``` r
pisa2012$cl_weight <- wscale(cluster = 'schoolid', data = pisa2012, 
                             wt = 'cweight', type = 'cluster')
pisa2012$es_weight <- wscale(cluster = 'schoolid', data = pisa2012, 
                             wt = 'cweight', type = 'ecluster')
```

After I wrote the function above, I found a function that does the scaling. The results are the same:

```
xx <- datawizard::rescale_weights(pisa2012, "schoolid", "w_fstuwt")
```

We can compare results to those of Mplus. This is using cluster weights:


``` r
ex <- mixPV(pv1math + pv2math + pv3math + pv4math + pv5math ~ 
              1 + (1|schoolid), 
            weights = c('cl_weight', 'w_fschwt'), mc = TRUE,
            cWeight = TRUE,
            data = pisa2012)
```

```
Attempting to use 15 cores. Progress will not be displayed. Please wait.
```

``` r
summary(ex)
```

```
Results of multilevel analyses with 5 plausible values.
Number of observations: 3136 

Estimates for random effects: 
                     estimate std.error statistic     df Pr(>t)    
schoolid.(Intercept)  1825.03    436.26      4.18 2719.0 <2e-16 ***
Residual              5726.58    233.29     24.55   90.6 <2e-16 ***
---
Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

Estimates for fixed effects: 
            estimate std.error statistic   df Pr(>t)    
(Intercept)   473.39      7.03     67.33 1777 <2e-16 ***
---
Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

Comparing to Mplus output shows similar results (SEs might differ slightly):

```
                Estimate       S.E.  Est./S.E.    P-Value

Within Level

Variances
MATH            5726.580    234.000     24.473      0.000

Between Level

Means
MATH             473.390      7.387     64.088      0.000

Variances
MATH            1825.021    464.365      3.930      0.000
```

This is using the ecluster option:


``` r
ex2 <- mixPV(pv1math + pv2math + pv3math + pv4math + pv5math ~ 
               1 + (1|schoolid), 
             weights = c('es_weight', 'w_fschwt'), mc = TRUE,
             cWeight = TRUE,
             data = pisa2012)
```

```
Attempting to use 15 cores. Progress will not be displayed. Please wait.
```

``` r
summary(ex2)
```

```
Results of multilevel analyses with 5 plausible values.
Number of observations: 3136 

Estimates for random effects: 
                     estimate std.error statistic     df Pr(>t)    
schoolid.(Intercept)  1821.40    434.62      4.19 2714.0 <2e-16 ***
Residual              5727.56    233.36     24.54   90.2 <2e-16 ***
---
Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

Estimates for fixed effects: 
            estimate std.error statistic   df Pr(>t)    
(Intercept)   473.40      7.02     67.39 1776 <2e-16 ***
---
Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

```
                Estimate       S.E.  Est./S.E.    P-Value

Within Level

Variances
MATH            5727.560    234.056     24.471      0.000

Between Level

Means
MATH             473.403      7.379     64.156      0.000

Variances
MATH            1821.389    462.665      3.937      0.000
```

This is unscaled:


``` r
ex3 <- mixPV(pv1math + pv2math + pv3math + pv4math + pv5math ~ 
               1 + (1|schoolid), 
             weights = c('cweight', 'w_fschwt'), mc = TRUE,
             cWeight = TRUE,
             data = pisa2012)
```

```
Attempting to use 15 cores. Progress will not be displayed. Please wait.
```

``` r
summary(ex3)
```

```
Results of multilevel analyses with 5 plausible values.
Number of observations: 3136 

Estimates for random effects: 
                     estimate std.error statistic   df Pr(>t)    
schoolid.(Intercept)  2126.22    503.39      4.22 3099 <2e-16 ***
Residual              5895.85    175.05     33.68  876 <2e-16 ***
---
Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

Estimates for fixed effects: 
            estimate std.error statistic   df Pr(>t)    
(Intercept)   472.35      7.33     64.44 1470 <2e-16 ***
---
Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```


```
                 Estimate       S.E.  Est./S.E.    P-Value
Within Level

Variances
MATH            5895.849    174.758     33.737      0.000

Between Level

Means
MATH             472.349      7.750     60.950      0.000

Variances
MATH            2126.477    535.991      3.967      0.000
```

Results differ based on the scaling option and results are all similar to those generated by Mplus.

## 3. What about just using level-2 weights?

A simpler option may just be to use level-2 weights (as suggested by Mang et al., 2021 as well as our own simulation results [under review]). To do so, when using R, we need to need to set the weight at level 1 to 1 *and* use conditional weights.


``` r
pisa2012$one <- 1 #level-1 weight
m1d <- mixPV(pv1math + pv2math + pv3math + pv4math + pv5math ~ 
               sc14q02 + st04q01 + escs + (1|schoolid), 
             weights = c('one', 'w_fschwt'), mc = T,
             cWeight = TRUE,
             data = pisa2012)
```

```
Attempting to use 15 cores. Progress will not be displayed. Please wait.
```

For comparison, I will show results also using the scaled weights:


``` r
m1e <- mixPV(pv1math + pv2math + pv3math + pv4math + pv5math ~ 
               sc14q02 + st04q01 + escs + (1|schoolid), 
             weights = c('cl_weight', 'w_fschwt'), mc = TRUE,
             cWeight = TRUE,
             data = pisa2012)
```

```
Attempting to use 15 cores. Progress will not be displayed. Please wait.
```

``` r
m1f<- mixPV(pv1math + pv2math + pv3math + pv4math + pv5math ~ 
              sc14q02 + st04q01 + escs + (1|schoolid), 
            weights = c('es_weight', 'w_fschwt'), mc = TRUE,
            cWeight = TRUE,
            data = pisa2012)
```

```
Attempting to use 15 cores. Progress will not be displayed. Please wait.
```

``` r
modelsummary(list("l2only" = m1d, 'cluster' = m1e, "ecluster" = m1f), 
             stars = TRUE, gof_map = NA, output = 'gt')
```

<!--html_preserve--><div id="pjojxhhbnr" style="padding-left:0px;padding-right:0px;padding-top:10px;padding-bottom:10px;overflow-x:auto;overflow-y:auto;width:auto;height:auto;">
<style>#pjojxhhbnr table {
  font-family: system-ui, 'Segoe UI', Roboto, Helvetica, Arial, sans-serif, 'Apple Color Emoji', 'Segoe UI Emoji', 'Segoe UI Symbol', 'Noto Color Emoji';
  -webkit-font-smoothing: antialiased;
  -moz-osx-font-smoothing: grayscale;
}

#pjojxhhbnr thead, #pjojxhhbnr tbody, #pjojxhhbnr tfoot, #pjojxhhbnr tr, #pjojxhhbnr td, #pjojxhhbnr th {
  border-style: none;
}

#pjojxhhbnr p {
  margin: 0;
  padding: 0;
}

#pjojxhhbnr .gt_table {
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

#pjojxhhbnr .gt_caption {
  padding-top: 4px;
  padding-bottom: 4px;
}

#pjojxhhbnr .gt_title {
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

#pjojxhhbnr .gt_subtitle {
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

#pjojxhhbnr .gt_heading {
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

#pjojxhhbnr .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#pjojxhhbnr .gt_col_headings {
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

#pjojxhhbnr .gt_col_heading {
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

#pjojxhhbnr .gt_column_spanner_outer {
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

#pjojxhhbnr .gt_column_spanner_outer:first-child {
  padding-left: 0;
}

#pjojxhhbnr .gt_column_spanner_outer:last-child {
  padding-right: 0;
}

#pjojxhhbnr .gt_column_spanner {
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

#pjojxhhbnr .gt_spanner_row {
  border-bottom-style: hidden;
}

#pjojxhhbnr .gt_group_heading {
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

#pjojxhhbnr .gt_empty_group_heading {
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

#pjojxhhbnr .gt_from_md > :first-child {
  margin-top: 0;
}

#pjojxhhbnr .gt_from_md > :last-child {
  margin-bottom: 0;
}

#pjojxhhbnr .gt_row {
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

#pjojxhhbnr .gt_stub {
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

#pjojxhhbnr .gt_stub_row_group {
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

#pjojxhhbnr .gt_row_group_first td {
  border-top-width: 2px;
}

#pjojxhhbnr .gt_row_group_first th {
  border-top-width: 2px;
}

#pjojxhhbnr .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#pjojxhhbnr .gt_first_summary_row {
  border-top-style: solid;
  border-top-color: #D3D3D3;
}

#pjojxhhbnr .gt_first_summary_row.thick {
  border-top-width: 2px;
}

#pjojxhhbnr .gt_last_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#pjojxhhbnr .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#pjojxhhbnr .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}

#pjojxhhbnr .gt_last_grand_summary_row_top {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: double;
  border-bottom-width: 6px;
  border-bottom-color: #D3D3D3;
}

#pjojxhhbnr .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}

#pjojxhhbnr .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#pjojxhhbnr .gt_footnotes {
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

#pjojxhhbnr .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}

#pjojxhhbnr .gt_sourcenotes {
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

#pjojxhhbnr .gt_sourcenote {
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}

#pjojxhhbnr .gt_left {
  text-align: left;
}

#pjojxhhbnr .gt_center {
  text-align: center;
}

#pjojxhhbnr .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}

#pjojxhhbnr .gt_font_normal {
  font-weight: normal;
}

#pjojxhhbnr .gt_font_bold {
  font-weight: bold;
}

#pjojxhhbnr .gt_font_italic {
  font-style: italic;
}

#pjojxhhbnr .gt_super {
  font-size: 65%;
}

#pjojxhhbnr .gt_footnote_marks {
  font-size: 75%;
  vertical-align: 0.4em;
  position: initial;
}

#pjojxhhbnr .gt_asterisk {
  font-size: 100%;
  vertical-align: 0;
}

#pjojxhhbnr .gt_indent_1 {
  text-indent: 5px;
}

#pjojxhhbnr .gt_indent_2 {
  text-indent: 10px;
}

#pjojxhhbnr .gt_indent_3 {
  text-indent: 15px;
}

#pjojxhhbnr .gt_indent_4 {
  text-indent: 20px;
}

#pjojxhhbnr .gt_indent_5 {
  text-indent: 25px;
}

#pjojxhhbnr .katex-display {
  display: inline-flex !important;
  margin-bottom: 0.75em !important;
}

#pjojxhhbnr div.Reactable > div.rt-table > div.rt-thead > div.rt-tr.rt-tr-group-header > div.rt-th-group:after {
  height: 0px !important;
}
</style>
<table class="gt_table" data-quarto-disable-processing="false" data-quarto-bootstrap="false">
  <thead>
    <tr class="gt_col_headings">
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1" scope="col" id=" "> </th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="l2only">l2only</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="cluster">cluster</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="ecluster">ecluster</th>
    </tr>
  </thead>
  <tbody class="gt_table_body">
    <tr><td headers=" " class="gt_row gt_left">(Intercept)</td>
<td headers="l2only" class="gt_row gt_center">469.276***</td>
<td headers="cluster" class="gt_row gt_center">469.241***</td>
<td headers="ecluster" class="gt_row gt_center">469.233***</td></tr>
    <tr><td headers=" " class="gt_row gt_left"></td>
<td headers="l2only" class="gt_row gt_center">(7.049)</td>
<td headers="cluster" class="gt_row gt_center">(7.088)</td>
<td headers="ecluster" class="gt_row gt_center">(7.082)</td></tr>
    <tr><td headers=" " class="gt_row gt_left">sc14q02Very little</td>
<td headers="l2only" class="gt_row gt_center">-18.928</td>
<td headers="cluster" class="gt_row gt_center">-18.795</td>
<td headers="ecluster" class="gt_row gt_center">-18.800</td></tr>
    <tr><td headers=" " class="gt_row gt_left"></td>
<td headers="l2only" class="gt_row gt_center">(13.537)</td>
<td headers="cluster" class="gt_row gt_center">(13.624)</td>
<td headers="ecluster" class="gt_row gt_center">(13.614)</td></tr>
    <tr><td headers=" " class="gt_row gt_left">sc14q02To some extent</td>
<td headers="l2only" class="gt_row gt_center">-14.990</td>
<td headers="cluster" class="gt_row gt_center">-15.337</td>
<td headers="ecluster" class="gt_row gt_center">-15.339</td></tr>
    <tr><td headers=" " class="gt_row gt_left"></td>
<td headers="l2only" class="gt_row gt_center">(11.902)</td>
<td headers="cluster" class="gt_row gt_center">(12.117)</td>
<td headers="ecluster" class="gt_row gt_center">(12.111)</td></tr>
    <tr><td headers=" " class="gt_row gt_left">sc14q02A lot</td>
<td headers="l2only" class="gt_row gt_center">-23.536**</td>
<td headers="cluster" class="gt_row gt_center">-23.265**</td>
<td headers="ecluster" class="gt_row gt_center">-23.273**</td></tr>
    <tr><td headers=" " class="gt_row gt_left"></td>
<td headers="l2only" class="gt_row gt_center">(7.952)</td>
<td headers="cluster" class="gt_row gt_center">(8.088)</td>
<td headers="ecluster" class="gt_row gt_center">(8.083)</td></tr>
    <tr><td headers=" " class="gt_row gt_left">st04q01Male</td>
<td headers="l2only" class="gt_row gt_center">12.296**</td>
<td headers="cluster" class="gt_row gt_center">12.051**</td>
<td headers="ecluster" class="gt_row gt_center">12.084**</td></tr>
    <tr><td headers=" " class="gt_row gt_left"></td>
<td headers="l2only" class="gt_row gt_center">(4.320)</td>
<td headers="cluster" class="gt_row gt_center">(4.345)</td>
<td headers="ecluster" class="gt_row gt_center">(4.349)</td></tr>
    <tr><td headers=" " class="gt_row gt_left">escs</td>
<td headers="l2only" class="gt_row gt_center">29.086***</td>
<td headers="cluster" class="gt_row gt_center">29.045***</td>
<td headers="ecluster" class="gt_row gt_center">29.049***</td></tr>
    <tr><td headers=" " class="gt_row gt_left"></td>
<td headers="l2only" class="gt_row gt_center">(2.698)</td>
<td headers="cluster" class="gt_row gt_center">(2.685)</td>
<td headers="ecluster" class="gt_row gt_center">(2.688)</td></tr>
    <tr><td headers=" " class="gt_row gt_left">schoolid.(Intercept)</td>
<td headers="l2only" class="gt_row gt_center">1054.672***</td>
<td headers="cluster" class="gt_row gt_center">1069.977***</td>
<td headers="ecluster" class="gt_row gt_center">1067.115***</td></tr>
    <tr><td headers=" " class="gt_row gt_left"></td>
<td headers="l2only" class="gt_row gt_center">(258.981)</td>
<td headers="cluster" class="gt_row gt_center">(260.180)</td>
<td headers="ecluster" class="gt_row gt_center">(259.201)</td></tr>
    <tr><td headers=" " class="gt_row gt_left">Residual</td>
<td headers="l2only" class="gt_row gt_center">5223.750***</td>
<td headers="cluster" class="gt_row gt_center">5219.380***</td>
<td headers="ecluster" class="gt_row gt_center">5220.383***</td></tr>
    <tr><td headers=" " class="gt_row gt_left"></td>
<td headers="l2only" class="gt_row gt_center">(206.297)</td>
<td headers="cluster" class="gt_row gt_center">(204.390)</td>
<td headers="ecluster" class="gt_row gt_center">(204.518)</td></tr>
  </tbody>
  <tfoot class="gt_sourcenotes">
    <tr>
      <td class="gt_sourcenote" colspan="4">+ p &lt; 0.1, * p &lt; 0.05, ** p &lt; 0.01, *** p &lt; 0.001</td>
    </tr>
  </tfoot>
  
</table>
</div><!--/html_preserve-->

Results are very similar. 

A reason for this is that the level-1 scaled weights are all very close to 1 (or even equal to 1). If there is no variation within cluster, the weights will be 1. 

If we examine the original conditional weights, there is quite some variability:


``` r
range(pisa2012$cweight) #the original conditional weight
```

```
[1]  1.23 37.02
```

However, compared to the scaled weights, they are all very close to 1. Which is why the results shown above are all very similar. 


``` r
range(pisa2012$es_weight)
```

```
[1] 0.764 1.333
```

``` r
range(pisa2012$cl_weight)
```

```
[1] 0.782 1.356
```

``` r
mean(pisa2012$es_weight)
```

```
[1] 0.996
```

``` r
mean(pisa2012$cl_weight)
```

```
[1] 1
```

*Takeaway*: when running multilevel models with weights at different levels, make sure you are specifying the correct weights. If the residual variance at the second level is close to zero, you should probably revisit your syntax.


### References

Asparouhov, T. (2006). General Multi-Level Modeling with Sampling Weights Communications in Statistics - Theory and Methods, 35: 439-460.

Carle, A. (2009). Fitting multilevel models in complex survey data with design weights: Recommendations BMC Medical Research Methodology, 9(49): 1-13.

Mang, J., Kuchenhoff, H., Meinck, S., & Prenzel, M. (2021). Sampling weights in multilevel modelling: An investigation using PISA sampling structures. Large-Scale Assessments in Education, 9(1), 6. https://doi.org/10.1186/s40536-021-00099-0

END





