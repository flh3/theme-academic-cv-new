---
title: 🎉 Applied example for alternatives to logistic regression
summary: 
date: 2019-10-27

# Featured image
# Place an image named `featured.jpg/png` in this page's folder and customize its options here.
image:
  caption: 'Image credit: [**Unsplash**](https://unsplash.com)'

authors:
  - admin

tags:
  - logistic regression
design:
  spacing:
    # Customize the section spacing. Order is top, right, bottom, left.
    padding: ['20px', '0', '20px', '0']
---

<div id="introduction" class="section level2">
<h2>Introduction</h2>
<p>Logistic regression is often used to analyze experiments with binary outcomes (e.g., pass vs fail) and binary predictors (e.g., treatment vs control). Although appropriate, there are other possible models that can be run that may provide easier to interpret results.</p>
<p>In addition, some of these models may be quicker to run. Some may say that this point is moot given the availability of computing power today but if you’ve ever tried to run a hierarchical generalized linear model with a logit link function and a binary outcome, you know that when using R (using <code>glmer</code> or <code>nlme</code>) this may take quite a long time (and cross your fingers that you don’t have convergence issues).</p>
<p>The following code replicates the example (see the manuscript for details) in the <a href="https://www.tandfonline.com/eprint/YS723ZYEIB2CPWKBEMPZ/full?target=10.1080/00220973.2019.1699769">article</a>:</p>
<blockquote>
<p>Huang, F. (2019). Alternatives to logistic regression models with binary outcomes. Journal of Experimental Education. doi: 10.1080/00220973.2019.1699769</p>
</blockquote>
<p>Data are based on the article of Huang and Cornell (2015). Using an online survey, investigators tested for the presence of the question-order effect. Based on a random number generated when the students took the survey, they were placed in either the treatment (n = 1037) or control (n = 963) condition. Students in the treatment condition were asked four specific types bullying questions (i.e., verbal, physical, social, cyber) and then were asked a general bullying question (“I have been bullied in the past year”). Students in the control condition were asked the general bullying question first and then the specific bullying questions. We hypothesized that students who were asked the specific bullying questions first would report overall higher bullying vs the control group.</p>
</div>
<div id="examining-cross-tabs" class="section level2">
<h2>1. Examining cross tabs</h2>
<p>Load in the required packages:</p>
<pre class="r"><code>library(dplyr) #just for the pipe, %&gt;%
library(logbin) #to run a log binomial model with a function
library(summarytools) #for nicer crosstabs
library(jtools) #for easier exp, confints, and adjusted standard errors</code></pre>
<p>Load and examine the data frame:</p>
<pre class="r"><code>#specify the location on website
dat &lt;- url(&quot;http://faculty.missouri.edu/huangf/data/jxe2019/jxe.rdata&quot;) 
load(dat) #load in data from the website
summary(jxe) #name of the data.frame</code></pre>
<pre><code>##      abully           tord            female          gl      race    
##  Min.   :0.000   Min.   :0.0000   Min.   :0.0000   9th :446   w:1278  
##  1st Qu.:0.000   1st Qu.:0.0000   1st Qu.:0.0000   10th:530   b: 287  
##  Median :0.000   Median :1.0000   Median :1.0000   11th:517   h: 170  
##  Mean   :0.126   Mean   :0.5185   Mean   :0.5005   12th:507   a:  79  
##  3rd Qu.:0.000   3rd Qu.:1.0000   3rd Qu.:1.0000              o: 186  
##  Max.   :1.000   Max.   :1.0000   Max.   :1.0000</code></pre>
<pre class="r"><code>head(jxe)</code></pre>
<pre><code>##      abully tord female   gl race
## 6910      0    0      1 10th    w
## 8394      0    0      0  9th    w
## 7293      0    1      1  9th    w
## 8491      0    1      0 10th    w
## 4374      1    1      0 10th    w
## 1594      0    1      1 10th    b</code></pre>
<pre class="r"><code>dim(jxe)</code></pre>
<pre><code>## [1] 2000    5</code></pre>
<p>Review some crosstabs. All computations are based on the table below. <code>tord</code> is the treatment variable. <code>abully</code> indicates if the respondent had been bullied.</p>
<pre class="r"><code>tt &lt;- ctable(jxe$abully, jxe$tord, prop = &#39;c&#39;)
tt</code></pre>
<pre><code>## Cross-Tabulation, Column Proportions  
## abully * tord  
## Data Frame: jxe  
## 
## -------- ------ -------------- --------------- ---------------
##            tord              0               1           Total
##   abully                                                      
##        0          863 ( 89.6%)    885 ( 85.3%)   1748 ( 87.4%)
##        1          100 ( 10.4%)    152 ( 14.7%)    252 ( 12.6%)
##    Total          963 (100.0%)   1037 (100.0%)   2000 (100.0%)
## -------- ------ -------------- --------------- ---------------</code></pre>
<pre class="r"><code>tt$cross_table #crosstabs</code></pre>
<pre><code>##        tord
## abully     0    1 Total
##   0      863  885  1748
##   1      100  152   252
##   Total  963 1037  2000</code></pre>
<pre class="r"><code>tt$proportions #proportions</code></pre>
<pre><code>##               0         1 Total
## 0     0.8961578 0.8534233 0.874
## 1     0.1038422 0.1465767 0.126
## Total 1.0000000 1.0000000 1.000</code></pre>
<pre class="r"><code>(14.66 / 85.34) / (10.38 / 89.62) #OR = 1.48 </code></pre>
<pre><code>## [1] 1.483163</code></pre>
<pre class="r"><code>14.66 - 10.38 #risk difference: 4.28</code></pre>
<pre><code>## [1] 4.28</code></pre>
<pre class="r"><code>14.66 / 10.38 #risk ratio: 1.41</code></pre>
<pre><code>## [1] 1.412331</code></pre>
</div>
<div id="models-without-covariates" class="section level2">
<h2>2. Models without covariates</h2>
<p>For comparability, see how the results above map on to the first set of regressions without covariates:</p>
<pre class="r"><code>tab6.log1 &lt;- glm(abully ~ tord, data = jxe, family = binomial)
summ(tab6.log1, exp = T)$coef %&gt;% round(3)</code></pre>
<pre><code>##             exp(Est.)  2.5% 97.5%  z val.     p
## (Intercept)     0.116 0.094 0.143 -20.404 0.000
## tord            1.482 1.132 1.940   2.865 0.004</code></pre>
<pre class="r"><code>tab6.lpm1 &lt;- glm(abully ~ tord, data = jxe) #risk difference
summ(tab6.lpm1, confint = T, robust = &#39;HC3&#39;)$coef %&gt;% round(3)</code></pre>
<pre><code>##              Est.  2.5% 97.5% t val.     p
## (Intercept) 0.104 0.085 0.123 10.553 0.000
## tord        0.043 0.014 0.072  2.896 0.004</code></pre>
<pre class="r"><code>tab6.poi1 &lt;- glm(abully ~ tord, data = jxe, family = poisson) #risk ratio
summ(tab6.poi1, robust = &#39;HC3&#39;)$coef %&gt;% round(3)</code></pre>
<pre><code>##               Est.  S.E.  z val.     p
## (Intercept) -2.265 0.095 -23.900 0.000
## tord         0.345 0.121   2.852 0.004</code></pre>
<p>If you want to run a log-binomial model</p>
<pre class="r"><code>logb &lt;- update(tab6.lpm1, family = binomial(link = &#39;log&#39;))
#logb &lt;- logbin(abully ~ tord, data = jxe)
summ(logb, exp = T, digits = 3, robust = &#39;HC3&#39;)$coef %&gt;% round(3)</code></pre>
<pre><code>##             exp(Est.)  2.5% 97.5%  z val.     p
## (Intercept)     0.104 0.086 0.125 -23.900 0.000
## tord            1.412 1.114 1.789   2.852 0.004</code></pre>
</div>
<div id="models-with-covariates" class="section level2">
<h2>3. Models with covariates</h2>
<p>Compare these to the results in Table 6 in the article with the covariates:</p>
<pre class="r"><code>tab6.log2 &lt;- glm(abully ~ tord + female + gl + race, data = jxe, family = binomial)
summ(tab6.log2, exp = T)$coef %&gt;% round(3)</code></pre>
<pre><code>##             exp(Est.)  2.5% 97.5%  z val.     p
## (Intercept)     0.149 0.105 0.211 -10.706 0.000
## tord            1.487 1.134 1.951   2.866 0.004
## female          1.184 0.906 1.547   1.239 0.215
## gl10th          0.749 0.525 1.068  -1.597 0.110
## gl11th          0.633 0.438 0.914  -2.439 0.015
## gl12th          0.515 0.348 0.761  -3.333 0.001
## raceb           0.646 0.411 1.015  -1.895 0.058
## raceh           1.227 0.780 1.929   0.885 0.376
## racea           1.512 0.826 2.769   1.340 0.180
## raceo           1.165 0.747 1.815   0.674 0.501</code></pre>
<pre class="r"><code>tab6.lpm2 &lt;- update(tab6.log2, family = gaussian)
summ(tab6.lpm2, confint = T, robust = &#39;HC3&#39;)$coef %&gt;% round(3)</code></pre>
<pre><code>##               Est.   2.5%  97.5% t val.     p
## (Intercept)  0.138  0.098  0.178  6.707 0.000
## tord         0.042  0.013  0.071  2.873 0.004
## female       0.018 -0.011  0.047  1.231 0.218
## gl10th      -0.038 -0.083  0.008 -1.631 0.103
## gl11th      -0.055 -0.100 -0.011 -2.440 0.015
## gl12th      -0.074 -0.117 -0.030 -3.331 0.001
## raceb       -0.040 -0.077 -0.003 -2.109 0.035
## raceh        0.024 -0.033  0.081  0.825 0.409
## racea        0.051 -0.035  0.138  1.161 0.246
## raceo        0.017 -0.037  0.071  0.625 0.532</code></pre>
<pre class="r"><code>tab6.poi2 &lt;- update(tab6.log2, family = poisson)
summ(tab6.poi2, exp = T, robust = &#39;HC3&#39;)$coef %&gt;% round(3)</code></pre>
<pre><code>##             exp(Est.)  2.5% 97.5%  z val.     p
## (Intercept)     0.128 0.096 0.172 -13.815 0.000
## tord            1.411 1.114 1.787   2.852 0.004
## female          1.157 0.917 1.458   1.232 0.218
## gl10th          0.785 0.581 1.060  -1.581 0.114
## gl11th          0.678 0.494 0.929  -2.421 0.015
## gl12th          0.563 0.401 0.793  -3.295 0.001
## raceb           0.678 0.449 1.024  -1.845 0.065
## raceh           1.190 0.811 1.746   0.888 0.374
## racea           1.417 0.858 2.339   1.363 0.173
## raceo           1.140 0.781 1.664   0.678 0.498</code></pre>
<pre class="r"><code>tab6.logbin &lt;- update(tab6.log2, family = binomial(link = &#39;log&#39;))
## using logbinomial, adding `em` to speed up the estimation
## tab6.logbin &lt;- logbin(abully ~ tord + female + gl + race, 
##              data = jxe, method = &#39;em&#39;)
summ(tab6.logbin, exp = T, confint = T, robust = &#39;HC3&#39;)$coef %&gt;% round(3)</code></pre>
<pre><code>##             exp(Est.)  2.5% 97.5%  z val.     p
## (Intercept)     0.128 0.096 0.171 -13.869 0.000
## tord            1.412 1.115 1.788   2.866 0.004
## female          1.165 0.924 1.468   1.291 0.197
## gl10th          0.780 0.578 1.052  -1.628 0.104
## gl11th          0.672 0.491 0.921  -2.470 0.013
## gl12th          0.565 0.402 0.794  -3.288 0.001
## raceb           0.674 0.446 1.019  -1.872 0.061
## raceh           1.187 0.809 1.741   0.876 0.381
## racea           1.422 0.863 2.342   1.382 0.167
## raceo           1.142 0.782 1.666   0.687 0.492</code></pre>
<p>The pattern of results are similar. The Poisson, log-binomial, and linear probability models however may provide results that are easier to understand (especially if communicating results to a lay audience who do not understand odds ratios).</p>
<p><strong>References</strong></p>
<p>Huang, F., &amp; Cornell, D. (2015). Order and definitional effects in bullying surveys: Results from an experimental study. <em>Psychological Assessment, 27,</em> 1484-1493. doi: <a href="http://dx.doi.org/10.1037/pas0000149" class="uri">http://dx.doi.org/10.1037/pas0000149</a></p>
<p>– END</p>
</div>
