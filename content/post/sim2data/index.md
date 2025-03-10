---
title: 🎉 Simulating two level data
summary: How to simulate multilevel data using a Monte Carlo simulation.
date: 2023-10-27

# Featured image
# Place an image named `featured.jpg/png` in this page's folder and customize its options here.
image:
  caption: 'Image credit: [**Unsplash**](https://unsplash.com)'

authors:
  - admin

tags:
  - Multilevel
  - Monte Carlo
  - Simulation
---

{{< toc mobile_only=true is_open=true >}}

<p>Researchers may want to simulate a two-level model (i.e., a hierarchical linear model, a random effects model, etc.). The following code illustrates how to generate the data and compares analytic techniques using MLM and OLS.</p>
<div id="simulate-the-data" class="section level2">
<h2>1. Simulate the data</h2>
<pre class="r"><code>set.seed(1234) #for reproducability
nG &lt;- 20 #number of groups
nJ &lt;- 30 #cluster size
W1 &lt;- 2 #level 2 coeff
X1 &lt;- 3 #level 1 coeff

tmp2 &lt;- rnorm(nG) #generate 20 random numbers, m = 0, sd = 1
l2 &lt;- rep(tmp2, each = nJ) #all units in l2 have the same value
group &lt;- gl(nG, k = nJ) #creating cluster variable
tmp2 &lt;- rnorm(nG) #error term for level 2
err2 &lt;- rep(tmp2, each = nJ) #all units in l2 have the same value

l1 &lt;- rnorm(nG * nJ) #total sample size is nG * nJ
err1 &lt;- rnorm(nG * nJ) #level 1 

#putting it all together
y &lt;- W1 * l2 + X1 * l1 + err2 + err1
dat &lt;- data.frame(y, group, l2, err2,l1, err1)</code></pre>
<p>To vary the intraclass correlation (ICC or <span class="math inline">\(\rho\)</span>), users must specify what the variance of the error terms should be (while taking into account the variance of the variables). There is a difference between the <em>unconditional</em> vs <em>conditional</em> ICC (often, in education, we want to know the unconditional first). Use covariance algebra to figure this out.</p>
<p>For example, for two variables (X and Y):</p>
<p><span class="math inline">\(Var(X + Y) = Var(X) + Var(Y) + 2cov(X, Y)\)</span></p>
<p>In our case, the variables are not related with each other so the last part is 0.</p>
<ul>
<li><p>The level 2 variance (due to l2) should be 4 (in our case, it is 3.912).</p></li>
<li><p>The level 1 variance (due to l1) should be 9 (in our case, it is 9.285).</p></li>
</ul>
<p>The errors both have 1 as the variance. So, in our models, the overall variance of y should be: (4 + 1) + (9 + 1) = 15. For the one simulated run above, the variance of y is 14.341. This is close. The theoretical unconditional ICC should be: 5/15 or .33. In our example, the standard errors turned out to be larger.</p>
</div>
<div id="analyze-the-data" class="section level2">
<h2>2. Analyze the data</h2>
<pre class="r"><code>library(lme4) #to run multilevel models
library(jtools) #to get nicer output
mlm0 &lt;- lmer(y ~ (1|group), data = dat) #unconditional
summ(mlm0) #shows the ICC, close</code></pre>
<pre><code>## MODEL INFO:
## Observations: 600
## Dependent Variable: y
## Type: Mixed effects linear regression 
## 
## MODEL FIT:
## AIC = 3166.12, BIC = 3179.31
## Pseudo-R² (fixed effects) = 0.00
## Pseudo-R² (total) = 0.28 
## 
## FIXED EFFECTS:
##              Est. S.E. t val.    p  
## (Intercept) -1.08 0.47  -2.28 0.03 *
## 
## p values calculated using Kenward-Roger d.f. = 19 
## 
## RANDOM EFFECTS:
##     Group   Parameter Std. Dev.
##     group (Intercept)      2.02
##  Residual                  3.23
## 
## Grouping variables:
##  Group # groups  ICC
##  group       20 0.28</code></pre>
<pre class="r"><code>mlm1 &lt;- lmer(y ~ l2 + l1  + (1|group), data = dat)
ols1 &lt;- lm(y ~ l2 + l1, data = dat)

#export_summs(mlm1, ols1, model.names = c(&#39;MLM&#39;, &#39;OLS&#39;))

stargazer::stargazer(mlm1, ols1, type = &#39;text&#39;, no.space = T, star.cutoffs = c(.05,.01,.001))</code></pre>
<pre><code>## 
## ============================================================
##                               Dependent variable:           
##                     ----------------------------------------
##                                        y                    
##                        linear                OLS            
##                     mixed-effects                           
##                          (1)                 (2)            
## ------------------------------------------------------------
## l2                    1.778***             1.777***         
##                        (0.173)             (0.049)          
## l1                    3.024***             3.047***         
##                        (0.039)             (0.048)          
## Constant              -0.663***           -0.663***         
##                        (0.176)             (0.050)          
## ------------------------------------------------------------
## Observations             600                 600            
## R2                                          0.901           
## Adjusted R2                                 0.900           
## Log Likelihood        -861.194                              
## Akaike Inf. Crit.     1,732.389                             
## Bayesian Inf. Crit.   1,754.373                             
## Residual Std. Error                    1.195 (df = 597)     
## F Statistic                       2,708.287*** (df = 2; 597)
## ============================================================
## Note:                          *p&lt;0.05; **p&lt;0.01; ***p&lt;0.001</code></pre>
<p>We can compare the results using MLM vs. OLS. The standard error for the level 2 variable is much smaller using the OLS model. Here we see why we can get Type I errors so easily (in this case, both are statistically significant though). The coefficients are similar to each other because the variables were generated to both be uncorrelated with each other.</p>
</div>
<div id="other-items-of-interest" class="section level2">
<h2>3. Other items of interest</h2>
<p>To see how these results may differ, readers can check out:</p>
<p>Huang, F. (2018). Multilevel modeling and ordinary least squares: How comparable are they? <em>Journal of Experimental Education, 86</em>, 265-281. <a href="https://doi.org/10.1080/00220973.2016.1277339" class="uri">https://doi.org/10.1080/00220973.2016.1277339</a>.</p>
<p><a href="http://www.tandfonline.com/eprint/WHmbzEjIhPidHtbt7IRk/full" class="uri">http://www.tandfonline.com/eprint/WHmbzEjIhPidHtbt7IRk/full</a></p>
<p>For more info comparing the two approaches:</p>
<ul>
<li><a href="http://faculty.missouri.edu/huangf/data/pubdata/jxe/online%20appendix%20A%20formatted.pdf">http://faculty.missouri.edu/huangf/data/pubdata/jxe/online%20appendix%20A%20formatted.pdf</a></li>
<li><a href="http://faculty.missouri.edu/huangf/data/pubdata/jxe/" class="uri">http://faculty.missouri.edu/huangf/data/pubdata/jxe/</a></li>
</ul>
<p>NOTE: a simpler way to get corrected level 2 standard errors is to use cluster robust standard errors. However, take note, that adjusted standard errors may often still be underestimated when the number of clusters is low (e.g., &lt; 50).</p>
<p>The <code>jtools::summ</code> function makes getting cluster robust standard errors easier! Without having to run other functions before hand.</p>
<pre class="r"><code>summ(ols1, cluster = &#39;group&#39;, robust = T)</code></pre>
<pre><code>## MODEL INFO:
## Observations: 600
## Dependent Variable: y
## Type: OLS linear regression 
## 
## MODEL FIT:
## F(2,597) = 2708.29, p = 0.00
## R² = 0.90
## Adj. R² = 0.90 
## 
## Standard errors: Cluster-robust, type = HC3
##              Est. S.E. t val.    p    
## (Intercept) -0.66 0.20  -3.25 0.00  **
## l2           1.78 0.25   7.23 0.00 ***
## l1           3.05 0.05  65.98 0.00 ***</code></pre>
<p>See:</p>
<p>Huang, F. (2016). Alternatives to multilevel modeling for the analysis of clustered data. <em>Journal of Experimental Education, 84</em>, 175-196. doi: 10.1080/00220973.2014.952397</p>
<p>– END</p>
</div>
