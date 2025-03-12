---
title: 🎉 Comparing coefficients across logistic regression models
summary: 
date: 2020-06-28

authors:
  - admin

tags:
  - logistic regression
---


<p><strong>ROUGH NOTES</strong>: <span style="color: red;"><em>[let me know if you spot any errors– there might be a couple!]</em></span> Often, in randomized control trial where individuals are randomly assigned to treatment and control conditions, covariates are included to improve precision by reducing error and improving statistical power. However, when binary outcomes are used (e.g., patient recovers or not), there are several additional concerns that have gone unnoticed by many applied researchers.</p>
<p>Take a simulated example where our true model data generating process is (to keep things simple, the intercept is zero and the parameters are both set to 1):</p>
<p><span class="math display">\[log(\frac{p}{1 - p}) = 1 + 1 \times (tr) + 1 \times (x2)\]</span></p>
<pre class="r"><code>library(summarytools)
library(stargazer)

pp &lt;- function(x) exp(x) / (1 + exp(x)) #convert logit to prob
set.seed(2468)
ns &lt;- 10000
tr &lt;- rbinom(ns, 1, .50) #50% treat / 50% control
x2 &lt;- rnorm(ns, 0 , 2) #uncorrelated x2
ystar &lt;- 1 + 1 * tr + 1 * x2 #all get a unit weight to keep it simple
y &lt;- rbinom(ns, 1, pp(ystar))
ctable(y, tr, prop = &#39;c&#39;)</code></pre>
<pre><code>## Cross-Tabulation, Column Proportions  
## y * tr  
## 
## ------- ---- --------------- --------------- ----------------
##           tr               0               1            Total
##       y                                                      
##       0        1756 ( 35.0%)   1165 ( 23.3%)    2921 ( 29.2%)
##       1        3254 ( 65.0%)   3825 ( 76.7%)    7079 ( 70.8%)
##   Total        5010 (100.0%)   4990 (100.0%)   10000 (100.0%)
## ------- ---- --------------- --------------- ----------------</code></pre>
<p>In our example, think about y = 1 as recovered from illness and y = 0 as did not recover from illness. In the treatment group, <em>77%</em> recovered vs. <em>65%</em>. This is a difference of around 12 percentage points or a higher rate of recovery by a factor of 1.18 (77/65).</p>
<p>Note that the two predictor variables are not correlated with each other:</p>
<pre class="r"><code>df &lt;- data.frame(y, tr, x2)
round(cor(df), 3)</code></pre>
<pre><code>##        y     tr     x2
## y  1.000  0.129  0.569
## tr 0.129  1.000 -0.017
## x2 0.569 -0.017  1.000</code></pre>
<p>When running a logistic regression model, notice how the coefficient changes even if <code>x2</code> is not correlated with <code>tr</code>.</p>
<pre class="r"><code>reduced &lt;- glm(y ~ tr, family = binomial)
full &lt;- update(reduced, . ~ . + x2)
stargazer(reduced, full, star.cutoffs = c(.05, .01, .001),
          no.space = T, type = &#39;text&#39;)</code></pre>
<pre><code>## 
## ================================================
##                        Dependent variable:      
##                   ------------------------------
##                                 y               
##                         (1)            (2)      
## ------------------------------------------------
## tr                   0.572***        1.005***   
##                       (0.045)        (0.059)    
## x2                                   1.040***   
##                                      (0.023)    
## Constant             0.617***        1.019***   
##                       (0.030)        (0.041)    
## ------------------------------------------------
## Observations          10,000          10,000    
## Log Likelihood      -5,956.977      -3,883.476  
## Akaike Inf. Crit.   11,917.950      7,772.952   
## ================================================
## Note:              *p&lt;0.05; **p&lt;0.01; ***p&lt;0.001</code></pre>
<p>The true effect should be around 1.0 (these are in logit units). In this basic example, there are several things to notice:</p>
<ol style="list-style-type: decimal">
<li><p>The coefficient for <code>tr</code> is <strong>underestimated</strong> in the first model (remember, I specified that the value should be 1).</p></li>
<li><p>The coefficients for <code>tr</code> (treatment status) change with the inclusion of <code>x2</code>. <code>x2</code> is not your typical confounder which is associated both with y and the treatment status.</p></li>
<li><p>The standard error increases in the 2nd model– it does not decrease as one might expect.</p></li>
</ol>
<p>As a result, this makes a few things problematic– even in the presence of randomization.</p>
<ol style="list-style-type: decimal">
<li><p>We might conclude that <code>x2</code> is a suppressor variable– but it is not!</p></li>
<li><p>This makes comparing results across models even within the same sample problematic. Results can change as long as variables predict the outcome.</p></li>
<li><p>We will never have all predictors of interest in a model (w/c is why we don’t get an <span class="math inline">\(R^2\)</span> of 1.00).</p></li>
</ol>
<p>Another question that might be raised is if model 1 is really incorrect. We know that the coefficient is lower than 1 but then by looking at the crosstabs, this actually matches what was shown earlier:</p>
<pre class="r"><code>pp(.617) #control:: 65%</code></pre>
<pre><code>## [1] 0.6495359</code></pre>
<pre class="r"><code>pp(.617 + .572) #treatment:: 77%</code></pre>
<pre><code>## [1] 0.7665622</code></pre>
<p>If we take the model 2 coefficients (and holding <code>x2</code> at its average of zero), the coefficients are correct (controlling for <code>x2</code>) but then this is not reflected in the crosstabs:</p>
<pre class="r"><code>pp(1) #control:: 73%</code></pre>
<pre><code>## [1] 0.7310586</code></pre>
<pre class="r"><code>pp(1 + 1) #treatment: 88%</code></pre>
<pre><code>## [1] 0.8807971</code></pre>
<div id="why-does-that-happen-with-lrms" class="section level2">
<h2>Why does that happen with LRMs?</h2>
<p>In a standard OLS model, we have the intercept, slope, and the residual.</p>
<p><span class="math display">\[y = \beta_0 + \beta_1  x_1 + e\]</span></p>
<p>The residuals have a mean of 0 and variance, <span class="math inline">\(\sigma^2_e\)</span>. The residual variance depends on the model and students of OLS regression know that if a predictor is added to the model, the residual variance decreases (even if it is a useless variable; w/c is why we at times use adjusted R2 vs R2).</p>
<p>However, in a logistic regression model, the variance of the error term is fixed to <span class="math inline">\(\frac{\pi^2}{3}\)</span> or 3.29, which is the variance of a logistic distribution. Adding predictors to the model does not change the residual variance. As a result, the added variability is absorbed in the other parts of the model (at times referred to as an issue or rescaling or unobserved heterogeneity).</p>
<p>Over three decades ago, Winship and Mare (1984, p. 517) stated:</p>
<blockquote>
<p>Adding new independent variables to an equation alters the variance of Y and thus the remaining coefficients in the model, even if the new independent variables are uncorrelated with the original independent variables.</p>
</blockquote>
<p>Mood (2010) more recently indicated (based off what WM wrote) that what is actually being estimated (<span class="math inline">\(b_1\)</span>) in the first model is:</p>
<p><span class="math display">\[b_1 \approx \beta_1 \times \frac{\sqrt{3.29}}{\sqrt{3.29 + \beta^2_2var(x_2)}} = 1.0 \times \frac{\sqrt{3.29}}{\sqrt{3.29 + 1^2 (4) }}\]</span>
As long as the denominator is greater than <span class="math inline">\(\sqrt{3.29}\)</span>, the estimated coefficient with the missing <span class="math inline">\(b_2\)</span> will be underestimated. The underestimation depends on effect and variability of <span class="math inline">\(x_2\)</span>.</p>
<p>The reason why coefficients may change when variables are added to the model may be due to:</p>
<ol style="list-style-type: decimal">
<li>real confounding (the control variable is correlated with BOTH the treatment and the outcome) and/or</li>
<li>rescaling.</li>
</ol>
<p>In the full model (with both <code>tr</code> and <code>x2</code>), the variability of the latent y variable (which causes y) is: <span class="math inline">\(1 * .25 + 1 * 4 + 3.29 = 7.54\)</span>. The .25 is the variability of the treatment variable which is .5 * (1 - .5). In the reduced model (w/c omits <code>x2</code>), the variability is: <span class="math inline">\(1 * .25 + 3.29 = 3.54\)</span>. The variability– even though the same sample is being used– differs between models. <strong>This is an approximation? Does not result in the latent y var exactly.</strong></p>
</div>
<div id="how-then-do-we-compare-results-across-models" class="section level2">
<h2>How then do we compare results across models?</h2>
<p>If we stick with logistic regression, we have a few options.</p>
<div id="y-standardization" class="section level3">
<h3>Y standardization</h3>
<p>Y standardization has been around for a while (Winship &amp; Mare, 1984). Involves taking the standard deviation of the latent y and using this as a scaling factor to create standardized coefficients for the predictors. Here’s a small function <code>ystd</code> that will compute the y standardized variables to be used with the model coefficients. Basically, this takes the variance of the predicted logits from the model estimated, adds the constant of 3.29 which results in the total variance of the outcome. Then take the square root of the variance which is the standard deviation. Divide the coefficients by the SD of y* (y* is referred to as the latent y; above which some threshold, y = 1, if else, y = 0).</p>
<pre class="r"><code>ystd &lt;- function(x){
  evar &lt;- (pi ^ 2) / 3 #3.29 #constant
  vr &lt;- var(predict(x)) + evar #variance of predicted logits + evar
  sdy &lt;- sqrt(vr) #getting the sd
  coef(x) / sdy
}

ystd(reduced)</code></pre>
<pre><code>## (Intercept)          tr 
##   0.3359345   0.3115061</code></pre>
<pre class="r"><code>ystd(full)</code></pre>
<pre><code>## (Intercept)          tr          x2 
##   0.3628906   0.3578878   0.3701445</code></pre>
<p>The resulting coefficients can be interpreted as the usual standardized beta– for a one standard deviation increase in the predictor, results in a std coef change in the outcome (i.e., .31 and .36). However, for binary predictors, that doesn’t make much sense to me (e.g., a SD coefficient change in gender, race, etc.) since you have a unit change or nothing.</p>
</div>
<div id="karlson-holm-breen-khb-method" class="section level3">
<h3>Karlson, Holm, &amp; Breen (khb) method</h3>
<p>Again, this involves comparing the full and reduced models. Two ways to think about it. We need to estimate the reduced model but include <span class="math inline">\(x_2\)</span> <em>without really including it</em>. A way to do that is to get the residuals of <span class="math inline">\(x_2\)</span> regressed on treatment (can just use OLS) and then include that residual as a predictor (don’t interpret it) in the model. The output below (.93) is much closer to 1.0.</p>
<pre class="r"><code>res &lt;- resid(lm(x2 ~ tr))
reduced2 &lt;- glm(y ~ tr + res, family = binomial)
stargazer(reduced, reduced2, full,
          star.cutoffs = c(.05, .01, .001),
          no.space = T, type = &#39;text&#39;)</code></pre>
<pre><code>## 
## ==================================================
##                         Dependent variable:       
##                   --------------------------------
##                                  y                
##                      (1)        (2)        (3)    
## --------------------------------------------------
## tr                 0.572***   0.933***   1.005*** 
##                    (0.045)    (0.058)    (0.059)  
## res                           1.040***            
##                               (0.023)             
## x2                                       1.040*** 
##                                          (0.023)  
## Constant           0.617***   1.034***   1.019*** 
##                    (0.030)    (0.041)    (0.041)  
## --------------------------------------------------
## Observations        10,000     10,000     10,000  
## Log Likelihood    -5,956.977 -3,883.476 -3,883.476
## Akaike Inf. Crit. 11,917.950 7,772.952  7,772.952 
## ==================================================
## Note:                *p&lt;0.05; **p&lt;0.01; ***p&lt;0.001</code></pre>
<p>Another way to think about it is to get the predicted logits (the estimated y*) from the full model and then just use that as the outcome in a linear regression (since the predicted logits are not anymore ones and zeroes but are continuous). [I am not sure about the standard errors in these models, just the point estimates/coefficients: <code>reduced2</code> is fine though].</p>
<pre class="r"><code>predy &lt;- predict(full) #gets logits, do not use fitted which returns prob
reduced3 &lt;- glm(predy ~ tr)
stargazer(reduced, reduced2, reduced3, full,
          star.cutoffs = c(.05, .01, .001),
          no.space = T, type = &#39;text&#39;,
          omit.stat = c(&#39;f&#39;, &#39;ll&#39;, &#39;ser&#39;, &#39;rsq&#39;))</code></pre>
<pre><code>## 
## ===========================================================
##                              Dependent variable:           
##                   -----------------------------------------
##                            y             predy        y    
##                         logistic         normal   logistic 
##                      (1)        (2)       (3)        (4)   
## -----------------------------------------------------------
## tr                 0.572***  0.933***   0.933***  1.005*** 
##                    (0.045)    (0.058)   (0.042)    (0.059) 
## res                          1.040***                      
##                               (0.023)                      
## x2                                                1.040*** 
##                                                    (0.023) 
## Constant           0.617***  1.034***   1.034***  1.019*** 
##                    (0.030)    (0.041)   (0.030)    (0.041) 
## -----------------------------------------------------------
## Observations        10,000    10,000     10,000    10,000  
## Akaike Inf. Crit. 11,917.950 7,772.952 43,161.080 7,772.952
## ===========================================================
## Note:                         *p&lt;0.05; **p&lt;0.01; ***p&lt;0.001</code></pre>
<div id="use-the-khb-package" class="section level4">
<h4>Use the khb package</h4>
<p>Another way is to use the <code>khb</code> package. Compare results with what was computed earlier.</p>
<pre class="r"><code>library(khb)
compareModels(reduced, full, method = &#39;naive&#39;) #no adjustments</code></pre>
<pre><code>##             Reduced  Full    perc   
## (Intercept) 0.61685  1.0194  -65.253
## tr          0.57199  1.0053  -75.756
## x2                   1.0397         
##                                     
## Pseudo R2   0.023544 0.49965        
## Dev.        11914    7767           
## Null        12080    12080          
## Chisq       166.47   4313.5         
## Sig         ***      ***            
## Dl          1        2              
## BIC         11923    7785.4</code></pre>
<pre class="r"><code>compareModels(reduced, full, method = &#39;ystand&#39;)</code></pre>
<pre><code>##             Reduced  Full    perc    
## (Intercept) 0.33593  0.36289  -8.0242
## tr          0.31151  0.35789 -14.8895
## x2                   0.37014         
##                                      
## Pseudo R2   0.023544 0.49965         
## Dev.        11914    7767            
## Null        12080    12080           
## Chisq       166.47   4313.5          
## Sig         ***      ***             
## Dl          1        2               
## BIC         11923    7785.4</code></pre>
<pre class="r"><code>compareModels(reduced, full, method = &#39;khb&#39;)</code></pre>
<pre><code>##             Reduced Full    perc   
## (Intercept) 1.03378 1.0194   1.3950
## tr          0.93291 1.0053  -7.7599
## Resid(x2)   1.03973                
## x2                  1.0397         
##                                    
## Pseudo R2   0.49965 0.49965        
## Dev.        7767    7767           
## Null        12080   12080          
## Chisq       4313.5  4313.5         
## Sig         ***     ***            
## Dl          2       2              
## BIC         7785.4  7785.4</code></pre>
<pre class="r"><code>k &lt;- khb(reduced, full) #to compare and get SEs
print(k)</code></pre>
<pre><code>## KHB method
## Model type: glm lm (logit) 
## Variables of interest: tr
## Z variables (mediators): x2
## 
## Summary of confounding
##       Ratio Percentage Rescaling
## tr  0.92799   -7.75989     1.631
## ------------------------------------------------------------------
## tr :
##          Estimate Std. Error z value Pr(&gt;|z|)    
## Reduced  0.932912   0.058321 15.9963  &lt; 2e-16 ***
## Full     1.005305   0.058673 17.1340  &lt; 2e-16 ***
## Diff    -0.072393   0.041898 -1.7278  0.08402 .</code></pre>
<p>The <code>kb</code> results indicate that the difference between the full and reduced model shown are not statistically significant (p = .08; i.e., not different from zero).</p>
</div>
</div>
</div>
<div id="use-alternative-procedures" class="section level2">
<h2>Use alternative procedures</h2>
<p>NOTE: if we use alternative procedures such as a linear probability model (LPM) or a modified Poisson regression (I’m not adjusting for overdisperson in the example, just focusing on the regression coefficients), these are not subject to the same issues. In particular, the LPM works as we would expect where the coefficient for <code>tr</code> is relatively unchanged and the standard errors decrease. I’ve also shown this in a prior study (Huang, 2019).</p>
<pre class="r"><code>reduced.ols &lt;- glm(y ~ tr, family = gaussian) #normal
full.ols &lt;- update(reduced.ols, . ~ . + x2)
reduced.poisson &lt;- glm(y ~ tr, family = poisson) #normal
full.poisson &lt;- update(reduced.poisson, . ~ . + x2)
stargazer(reduced.ols, full.ols, 
          reduced.poisson, full.poisson,
          star.cutoffs = c(.05, .01, .001),
          no.space = T, type = &#39;text&#39;)</code></pre>
<pre><code>## 
## =============================================================
##                               Dependent variable:            
##                   -------------------------------------------
##                                        y                     
##                          normal                Poisson       
##                      (1)        (2)        (3)        (4)    
## -------------------------------------------------------------
## tr                 0.117***   0.126***   0.166***   0.180*** 
##                    (0.009)    (0.007)    (0.024)    (0.024)  
## x2                            0.129***              0.182*** 
##                               (0.002)               (0.006)  
## Constant           0.650***   0.648***  -0.432***  -0.502*** 
##                    (0.006)    (0.005)    (0.018)    (0.018)  
## -------------------------------------------------------------
## Observations        10,000     10,000     10,000     10,000  
## Log Likelihood    -6,226.338 -4,209.938 -9,500.248 -9,023.055
## Akaike Inf. Crit. 12,456.670 8,425.875  19,004.500 18,052.110
## =============================================================
## Note:                           *p&lt;0.05; **p&lt;0.01; ***p&lt;0.001</code></pre>
<p>If you compare the results above to the crosstabs shown at the start, we find that the treatment group is around 12 percentage points higher than the control (shown using OLS) and the the rate of recovery of the treatment group is higher by a factor of ~1.2 (<code>exp(.18)</code>).</p>
<p><strong>References</strong></p>
<p>Huang, F. (2019). Alternatives to logistic regression models with binary outcomes. Journal of Experimental Education. doi: 10.1080/00220973.2019.1699769</p>
<p>Karlson, KB, Holm, A, &amp; Breen, R (2012). Comparing regression coefficients between same-sample nested mModels using logit and probit: A new method. Sociological Methodology, 42(1), pp 286-313.</p>
<p>Mood, C. (2010). Logistic regression: Why we cannot do what we think we can do, and what we can do about it. European Sociological Review, 26, 67-82. <a href="https://doi.org/10.1093/esr/jcp006" class="uri">https://doi.org/10.1093/esr/jcp006</a></p>
<p>Studer, M. (2014). khb: Comparing nonlinear regression models. R package version 0.1.</p>
<p>Winship, C., &amp; Mare, R. D. (1984). Regression models with ordinal variables. American Sociological Review, 49, 512-525.</p>
</div>
