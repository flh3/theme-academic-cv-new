---
title: 🎉 Understanding instrumental variables
summary: A primer on using IVs
date: 2018-06-27

# Featured image
# Place an image named `featured.jpg/png` in this page's folder and customize its options here.
image:
  caption: 'Image credit: [**Unsplash**](https://unsplash.com)'

authors:
  - admin

tags:
  - instrumental variable
  - causality
---

<div id="instrumental-variables-itt-and-tot" class="section level2">
<h2>Instrumental variables, ITT, and TOT</h2>
<p>Based on:</p>
<p>Angrist, J. D. (2006). Instrumental variables methods in experimental criminological research: what, why and how. <em>Journal of Experimental Criminology</em>, 2, 23-44. <a href="https://doi.org/10.1007/s11292-005-5126-x" class="uri">https://doi.org/10.1007/s11292-005-5126-x</a></p>
<p>Evaluation of the Minneapolis Domestic Violence Experiment (MDVE): Evaluated the police response to domestic violence reports. Experiment conducted in 1981-82 by Lawrence Sherman and Richard Berk. Police may be reluctant to get involved for various reasons (e.g., might be viewed as a private matter, may not want to get involved).</p>
<p>Design applied only to simple misdemenors where both suspect and victim were present when the police arrived. Only included cases where the police were empowered to make a decision (but not required to make an arrest- but must have probable cause).</p>
<p>Each officer carried a pad of color coded report forms. Each time an officer encountered a situation that fit the experimental criteria, they were to take action as indicated by the color coded form (which were assigned by lottery assignment or random). To monitor the consistency of lottery assignment, research staff rode on patrols for a sample of evenings. (read the article below for further details):</p>
<p>See: <a href="https://en.wikipedia.org/wiki/Minneapolis_Domestic_Violence_Experiment" class="uri">https://en.wikipedia.org/wiki/Minneapolis_Domestic_Violence_Experiment</a>– link to the original article is there.</p>
<p>After qualifying a certain case, police officers were provided three randomized approaches that they could use:</p>
<ol style="list-style-type: decimal">
<li>Send abuser away for 8 hours</li>
<li>Advice and mediation of disputes</li>
<li>Make an arrest</li>
</ol>
<p>For purposes of the experiment, we refer to 1 and 2 as the coddling condition. The main outcome of interest was the rate of reoffending (which condition would have higher rates of reoffending: coddling or arrests?).</p>
<pre class="r"><code>x &lt;- rio::import(&#39;http://faculty.missouri.edu/huangf/data/eval/coddled.sav&#39;)
#x$t_random &lt;- factor(x$t_random, labels = c(&#39;pink&#39;,&#39;yellow&#39;,&#39;blue&#39;))
#x$t_random &lt;- factor(x$t_random, labels = c(&#39;arrest&#39;,&#39;advise&#39;,&#39;separate&#39;))
#head(x)
names(x)</code></pre>
<pre><code>##  [1] &quot;t_random&quot;    &quot;z_coddled&quot;   &quot;d_coddled&quot;   &quot;y82&quot;         &quot;q1&quot;         
##  [6] &quot;q2&quot;          &quot;q3&quot;          &quot;mixed&quot;       &quot;s_influence&quot; &quot;anyweapon&quot;  
## [11] &quot;nonwhite&quot;    &quot;fail_z&quot;</code></pre>
<p>Inspect the variables: z_coddled represents the assigned treatment. d_coddled shows if the treatment was actually delivered.</p>
<pre class="r"><code>descr::crosstab(x$z_coddled, x$d_coddled, prop.r = T, plot = F)</code></pre>
<pre><code>##    Cell Contents 
## |-------------------------|
## |                   Count | 
## |             Row Percent | 
## |-------------------------|
## 
## ====================================
##                x$d_coddled
## x$z_coddled        0       1   Total
## ------------------------------------
## 0                91       1      92 
##                98.9%    1.1%   29.3%
## ------------------------------------
## 1                45     177     222 
##                20.3%   79.7%   70.7%
## ------------------------------------
## Total           136     178     314 
## ====================================</code></pre>
<pre class="r"><code>79.7 - 1.1</code></pre>
<pre><code>## [1] 78.6</code></pre>
<p>Based on this, 80% complied with the treatment delivery of coddling when assigned to coddle. On the other hand, 1 out of 92 coddled when assigned to not coddle. More precisely, 78.6% followed treatment assignment. This can also be estimated using OLS.</p>
<pre class="r"><code>assign1 &lt;- (lm(d_coddled ~  z_coddled, data = x))
assign2 &lt;- (lm(d_coddled ~  z_coddled + anyweapon + s_influence + y82 + q1 + q2 + q3 + nonwhite + mixed, data = x))
summary(assign1)</code></pre>
<pre><code>## 
## Call:
## lm(formula = d_coddled ~ z_coddled, data = x)
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -0.79730 -0.01087  0.20270  0.20270  0.98913 
## 
## Coefficients:
##             Estimate Std. Error t value Pr(&gt;|t|)    
## (Intercept)  0.01087    0.03584   0.303    0.762    
## z_coddled    0.78643    0.04262  18.451   &lt;2e-16 ***
## ---
## Signif. codes:  0 &#39;***&#39; 0.001 &#39;**&#39; 0.01 &#39;*&#39; 0.05 &#39;.&#39; 0.1 &#39; &#39; 1
## 
## Residual standard error: 0.3438 on 312 degrees of freedom
## Multiple R-squared:  0.5218, Adjusted R-squared:  0.5203 
## F-statistic: 340.4 on 1 and 312 DF,  p-value: &lt; 2.2e-16</code></pre>
<pre class="r"><code>summary(assign2)</code></pre>
<pre><code>## 
## Call:
## lm(formula = d_coddled ~ z_coddled + anyweapon + s_influence + 
##     y82 + q1 + q2 + q3 + nonwhite + mixed, data = x)
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -0.92651 -0.03539  0.11573  0.21259  0.90118 
## 
## Coefficients:
##              Estimate Std. Error t value Pr(&gt;|t|)    
## (Intercept)  0.047312   0.068947   0.686   0.4931    
## z_coddled    0.773049   0.042849  18.041   &lt;2e-16 ***
## anyweapon   -0.064339   0.044525  -1.445   0.1495    
## s_influence -0.087523   0.040233  -2.175   0.0304 *  
## y82         -0.036886   0.048734  -0.757   0.4497    
## q1           0.053236   0.073185   0.727   0.4675    
## q2           0.063912   0.058087   1.100   0.2721    
## q3          -0.002788   0.063121  -0.044   0.9648    
## nonwhite     0.024477   0.039752   0.616   0.5385    
## mixed        0.042235   0.045305   0.932   0.3520    
## ---
## Signif. codes:  0 &#39;***&#39; 0.001 &#39;**&#39; 0.01 &#39;*&#39; 0.05 &#39;.&#39; 0.1 &#39; &#39; 1
## 
## Residual standard error: 0.3422 on 304 degrees of freedom
## Multiple R-squared:  0.5382, Adjusted R-squared:  0.5245 
## F-statistic: 39.37 on 9 and 304 DF,  p-value: &lt; 2.2e-16</code></pre>
<pre class="r"><code>#jtools::export_summs(assign1, assign2)

mean(x$d_coddled) #mean of coddling delivered</code></pre>
<pre><code>## [1] 0.566879</code></pre>
<pre class="r"><code>#match this with p. 33</code></pre>
<p>With controls, the compliance rate was 77.3%.</p>
<p>Let’s investigate the outcome, if the suspect was rearrested. On p. 28, this refers to recidivism or “the occurence of post-treatment domestic assault by the same suspect.”</p>
<p>Using simple descriptives:</p>
<pre class="r"><code>descr::crosstab(x$fail_z, x$z_coddled, prop.c = T, plot = F)</code></pre>
<pre><code>##    Cell Contents 
## |-------------------------|
## |                   Count | 
## |          Column Percent | 
## |-------------------------|
## 
## =================================
##             x$z_coddled
## x$fail_z        0       1   Total
## ---------------------------------
## 0             83     175     258 
##             90.2%   78.8%        
## ---------------------------------
## 1              9      47      56 
##              9.8%   21.2%        
## ---------------------------------
## Total         92     222     314 
##             29.3%   70.7%        
## =================================</code></pre>
<p>Using a linear model. Get the same results. The second model allows for the inclusion of covariates. (including a year dummy and quarter dummies).</p>
<pre class="r"><code>itt1 &lt;- lm(fail_z ~  z_coddled, data = x)
itt2 &lt;- lm(fail_z ~  z_coddled + anyweapon + s_influence + y82 + q1 + q2 + q3 + nonwhite + mixed  , data = x)
#jtools::export_summs(itt1, itt2)
stargazer::stargazer(itt1, itt2, star.cutoffs = c(.05,.01,.001), type = &#39;text&#39;, no.space = T)</code></pre>
<pre><code>## 
## ================================================================
##                                 Dependent variable:             
##                     --------------------------------------------
##                                        fail_z                   
##                             (1)                    (2)          
## ----------------------------------------------------------------
## z_coddled                  0.114*                0.108**        
##                           (0.047)                (0.041)        
## anyweapon                                        -0.004         
##                                                  (0.042)        
## s_influence                                       0.052         
##                                                  (0.038)        
## y82                                             -0.363***       
##                                                  (0.046)        
## q1                                              0.322***        
##                                                  (0.069)        
## q2                                              0.442***        
##                                                  (0.055)        
## q3                                               0.186**        
##                                                  (0.060)        
## nonwhite                                         -0.017         
##                                                  (0.038)        
## mixed                                             0.047         
##                                                  (0.043)        
## Constant                   0.098*                -0.093         
##                           (0.040)                (0.065)        
## ----------------------------------------------------------------
## Observations                314                    314          
## R2                         0.018                  0.307         
## Adjusted R2                0.015                  0.287         
## Residual Std. Error   0.380 (df = 312)      0.324 (df = 304)    
## F Statistic         5.827* (df = 1; 312) 14.970*** (df = 9; 304)
## ================================================================
## Note:                              *p&lt;0.05; **p&lt;0.01; ***p&lt;0.001</code></pre>
<pre class="r"><code>#summary(itt1)
#summary(itt2)</code></pre>
<p>The ITT rate is generally more conservative as this factors in noncompliance (10.8% more arrest if coddled). Often reported because this is in actuality what you get on average if a policy is implemented on a large scale basis. Not everyone complies maybe. Coddling is associated with an increase of rearrests by around 11%. This is the Intent to Treat (ITT) effect.</p>
<p>If you just look at the actual treatment delivery (d_coddled), the effect is smaller than it should be. The effect of coddling here is ~ 8.7%.</p>
<pre class="r"><code>t1 &lt;- lm(fail_z ~  d_coddled , data = x)
t2 &lt;- lm(fail_z ~  d_coddled + anyweapon + s_influence + y82 + q1 + q2 + q3 + nonwhite + mixed , data = x)
stargazer::stargazer(t1, t2, star.cutoffs = c(.05,.01,.001), type = &#39;text&#39;, no.space = T)</code></pre>
<pre><code>## 
## ================================================================
##                                 Dependent variable:             
##                     --------------------------------------------
##                                        fail_z                   
##                             (1)                    (2)          
## ----------------------------------------------------------------
## d_coddled                  0.107*                0.087*         
##                           (0.043)                (0.038)        
## anyweapon                                        -0.002         
##                                                  (0.042)        
## s_influence                                       0.057         
##                                                  (0.038)        
## y82                                             -0.360***       
##                                                  (0.046)        
## q1                                              0.316***        
##                                                  (0.069)        
## q2                                              0.437***        
##                                                  (0.055)        
## q3                                               0.185**        
##                                                  (0.060)        
## nonwhite                                         -0.022         
##                                                  (0.038)        
## mixed                                             0.045         
##                                                  (0.043)        
## Constant                  0.118***               -0.065         
##                           (0.033)                (0.062)        
## ----------------------------------------------------------------
## Observations                314                    314          
## R2                         0.019                  0.303         
## Adjusted R2                0.016                  0.282         
## Residual Std. Error   0.380 (df = 312)      0.325 (df = 304)    
## F Statistic         6.111* (df = 1; 312) 14.683*** (df = 9; 304)
## ================================================================
## Note:                              *p&lt;0.05; **p&lt;0.01; ***p&lt;0.001</code></pre>
<p>To ‘recover’ the actual effect, divide the ITT estimate (10.8%) with the compliance rate (77.3%).</p>
<pre class="r"><code>.108/.773</code></pre>
<pre><code>## [1] 0.1397154</code></pre>
<p>The effect is actually a bit higher (14.0%). However, no one really calculates it this way– just shows how this is done. The other way is to run two regressions. In the first regression (or the first stage as it is called), predict actual compliance using treatment assignment (here we use the <code>fitted</code> function). We already did this in the <code>assign2</code> model earlier (to get the compliance rate). Next, we estimate the outcome using the predicted values based on <code>assign2</code>.</p>
<pre class="r"><code>stage1 &lt;- lm(fail_z ~ fitted(assign1), data = x)
stage2 &lt;- lm(fail_z ~ fitted(assign2) + anyweapon + s_influence + y82 + q1 + q2 + q3 + nonwhite + mixed, data = x)
stargazer::stargazer(stage1, stage2, star.cutoffs = c(.05,.01,.001), type = &#39;text&#39;, no.space = T)</code></pre>
<pre><code>## 
## ================================================================
##                                 Dependent variable:             
##                     --------------------------------------------
##                                        fail_z                   
##                             (1)                    (2)          
## ----------------------------------------------------------------
## fitted(assign1)            0.145*                               
##                           (0.060)                               
## fitted(assign2)                                  0.140**        
##                                                  (0.052)        
## anyweapon                                         0.005         
##                                                  (0.043)        
## s_influence                                       0.064         
##                                                  (0.039)        
## y82                                             -0.358***       
##                                                  (0.046)        
## q1                                              0.314***        
##                                                  (0.069)        
## q2                                              0.433***        
##                                                  (0.055)        
## q3                                               0.186**        
##                                                  (0.060)        
## nonwhite                                         -0.021         
##                                                  (0.038)        
## mixed                                             0.041         
##                                                  (0.043)        
## Constant                   0.096*                -0.100         
##                           (0.040)                (0.067)        
## ----------------------------------------------------------------
## Observations                314                    314          
## R2                         0.018                  0.307         
## Adjusted R2                0.015                  0.287         
## Residual Std. Error   0.380 (df = 312)      0.324 (df = 304)    
## F Statistic         5.827* (df = 1; 312) 14.970*** (df = 9; 304)
## ================================================================
## Note:                              *p&lt;0.05; **p&lt;0.01; ***p&lt;0.001</code></pre>
<p>The point estimate is correct though the standard error (.052) may be off. In general, this is computed using canned functions in statistics programs. In this case though, the standard errors were similar.</p>
<p>The actual IV reg using two stage least squares estimation (2SLS). Can be done using the <code>sem</code> or <code>AER</code> package. Shown both ways below (one with no controls, other with controls).</p>
<pre class="r"><code>iv1 &lt;- sem::tsls(fail_z ~  d_coddled, ~ z_coddled, data = x)
iv2 &lt;- sem::tsls(fail_z ~  d_coddled + anyweapon + s_influence + y82 + q1 + q2 + q3 + nonwhite + mixed, ~ anyweapon + s_influence + y82 + q1 + q2 + q3 + nonwhite + mixed + z_coddled , data = x)
summary(iv1)</code></pre>
<pre><code>## 
##  2SLS Estimates
## 
## Model Formula: fail_z ~ d_coddled
## 
## Instruments: ~z_coddled
## 
## Residuals:
##     Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
## -0.24107 -0.24107 -0.09625  0.00000 -0.09625  0.90375 
## 
##               Estimate Std. Error t value Pr(&gt;|t|)  
## (Intercept) 0.09625202 0.04024964 2.39138 0.017378 *
## d_coddled   0.14481385 0.06003622 2.41211 0.016437 *
## ---
## Signif. codes:  0 &#39;***&#39; 0.001 &#39;**&#39; 0.01 &#39;*&#39; 0.05 &#39;.&#39; 0.1 &#39; &#39; 1
## 
## Residual standard error: 0.3807832 on 312 degrees of freedom</code></pre>
<pre class="r"><code>summary(iv2)</code></pre>
<pre><code>## 
##  2SLS Estimates
## 
## Model Formula: fail_z ~ d_coddled + anyweapon + s_influence + y82 + q1 + q2 + 
##     q3 + nonwhite + mixed
## 
## Instruments: ~anyweapon + s_influence + y82 + q1 + q2 + q3 + nonwhite + mixed + 
##     z_coddled
## 
## Residuals:
##     Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
## -0.55831 -0.20427 -0.04468  0.00000  0.11556  0.97566 
## 
##                 Estimate   Std. Error  t value   Pr(&gt;|t|)    
## (Intercept) -0.099921186  0.066920037 -1.49314  0.1364369    
## d_coddled    0.139898052  0.052777514  2.65071  0.0084529 ** 
## anyweapon    0.004955654  0.042832553  0.11570  0.9079680    
## s_influence  0.064255255  0.038909632  1.65140  0.0996901 .  
## y82         -0.358006623  0.046428421 -7.71094 1.7941e-13 ***
## q1           0.314227128  0.069686777  4.50914 9.3049e-06 ***
## q2           0.433264526  0.055436372  7.81553 9.0150e-14 ***
## q3           0.186213372  0.060104751  3.09815  0.0021293 ** 
## nonwhite    -0.020594117  0.037793842 -0.54491  0.5862172    
## mixed        0.041405215  0.043250600  0.95733  0.3391600    
## ---
## Signif. codes:  0 &#39;***&#39; 0.001 &#39;**&#39; 0.01 &#39;*&#39; 0.05 &#39;.&#39; 0.1 &#39; &#39; 1
## 
## Residual standard error: 0.3258485 on 304 degrees of freedom</code></pre>
<pre class="r"><code>iv1a &lt;- AER::ivreg(fail_z ~ d_coddled | z_coddled, data = x)
iv2a &lt;- AER::ivreg(fail_z ~ d_coddled + anyweapon + s_influence + y82 + q1 + q2 + q3 + nonwhite + mixed | anyweapon + s_influence + y82 + q1 + q2 + q3 + nonwhite + mixed + z_coddled , data = x)

summary(iv1a)</code></pre>
<pre><code>## 
## Call:
## AER::ivreg(formula = fail_z ~ d_coddled | z_coddled, data = x)
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -0.24107 -0.24107 -0.09625 -0.09625  0.90375 
## 
## Coefficients:
##             Estimate Std. Error t value Pr(&gt;|t|)  
## (Intercept)  0.09625    0.04025   2.391   0.0174 *
## d_coddled    0.14481    0.06004   2.412   0.0164 *
## ---
## Signif. codes:  0 &#39;***&#39; 0.001 &#39;**&#39; 0.01 &#39;*&#39; 0.05 &#39;.&#39; 0.1 &#39; &#39; 1
## 
## Residual standard error: 0.3808 on 312 degrees of freedom
## Multiple R-Squared: 0.01682, Adjusted R-squared: 0.01367 
## Wald test: 5.818 on 1 and 312 DF,  p-value: 0.01644</code></pre>
<pre class="r"><code>summary(iv2a)</code></pre>
<pre><code>## 
## Call:
## AER::ivreg(formula = fail_z ~ d_coddled + anyweapon + s_influence + 
##     y82 + q1 + q2 + q3 + nonwhite + mixed | anyweapon + s_influence + 
##     y82 + q1 + q2 + q3 + nonwhite + mixed + z_coddled, data = x)
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -0.55831 -0.20427 -0.04468  0.11556  0.97566 
## 
## Coefficients:
##              Estimate Std. Error t value Pr(&gt;|t|)    
## (Intercept) -0.099921   0.066920  -1.493  0.13644    
## d_coddled    0.139898   0.052778   2.651  0.00845 ** 
## anyweapon    0.004956   0.042833   0.116  0.90797    
## s_influence  0.064255   0.038910   1.651  0.09969 .  
## y82         -0.358007   0.046428  -7.711 1.79e-13 ***
## q1           0.314227   0.069687   4.509 9.30e-06 ***
## q2           0.433265   0.055436   7.816 9.01e-14 ***
## q3           0.186213   0.060105   3.098  0.00213 ** 
## nonwhite    -0.020594   0.037794  -0.545  0.58622    
## mixed        0.041405   0.043251   0.957  0.33916    
## ---
## Signif. codes:  0 &#39;***&#39; 0.001 &#39;**&#39; 0.01 &#39;*&#39; 0.05 &#39;.&#39; 0.1 &#39; &#39; 1
## 
## Residual standard error: 0.3258 on 304 degrees of freedom
## Multiple R-Squared: 0.2985,  Adjusted R-squared: 0.2777 
## Wald test: 14.79 on 9 and 304 DF,  p-value: &lt; 2.2e-16</code></pre>
<pre class="r"><code>library(ivpack)
iv.nocov &lt;- ivreg(fail_z ~ d_coddled | z_coddled, data = x)
summary(iv.nocov)</code></pre>
<pre><code>## 
## Call:
## ivreg(formula = fail_z ~ d_coddled | z_coddled, data = x)
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -0.24107 -0.24107 -0.09625 -0.09625  0.90375 
## 
## Coefficients:
##             Estimate Std. Error t value Pr(&gt;|t|)  
## (Intercept)  0.09625    0.04025   2.391   0.0174 *
## d_coddled    0.14481    0.06004   2.412   0.0164 *
## ---
## Signif. codes:  0 &#39;***&#39; 0.001 &#39;**&#39; 0.01 &#39;*&#39; 0.05 &#39;.&#39; 0.1 &#39; &#39; 1
## 
## Residual standard error: 0.3808 on 312 degrees of freedom
## Multiple R-Squared: 0.01682, Adjusted R-squared: 0.01367 
## Wald test: 5.818 on 1 and 312 DF,  p-value: 0.01644</code></pre>
<pre class="r"><code>robust.se(iv.nocov)</code></pre>
<pre><code>## [1] &quot;Robust Standard Errors&quot;</code></pre>
<pre><code>## 
## t test of coefficients:
## 
##             Estimate Std. Error t value Pr(&gt;|t|)   
## (Intercept) 0.096252   0.031498  3.0558 0.002438 **
## d_coddled   0.144814   0.052694  2.7482 0.006341 **
## ---
## Signif. codes:  0 &#39;***&#39; 0.001 &#39;**&#39; 0.01 &#39;*&#39; 0.05 &#39;.&#39; 0.1 &#39; &#39; 1</code></pre>
<pre class="r"><code>iv.wcov &lt;- ivreg(fail_z ~ d_coddled + anyweapon + s_influence + y82 + q1 + q2 + q3 + nonwhite + mixed | anyweapon + s_influence + y82 + q1 + q2 + q3 + nonwhite + mixed + z_coddled , data = x)
robust.se(iv.wcov)</code></pre>
<pre><code>## [1] &quot;Robust Standard Errors&quot;</code></pre>
<pre><code>## 
## t test of coefficients:
## 
##               Estimate Std. Error t value  Pr(&gt;|t|)    
## (Intercept) -0.0999212  0.0461484 -2.1652 0.0311488 *  
## d_coddled    0.1398981  0.0481670  2.9044 0.0039489 ** 
## anyweapon    0.0049557  0.0420239  0.1179 0.9062053    
## s_influence  0.0642553  0.0369475  1.7391 0.0830302 .  
## y82         -0.3580066  0.0421370 -8.4962 8.879e-16 ***
## q1           0.3142271  0.0537986  5.8408 1.335e-08 ***
## q2           0.4332645  0.0504189  8.5933 4.510e-16 ***
## q3           0.1862134  0.0475390  3.9171 0.0001107 ***
## nonwhite    -0.0205941  0.0367519 -0.5604 0.5756499    
## mixed        0.0414052  0.0415748  0.9959 0.3200808    
## ---
## Signif. codes:  0 &#39;***&#39; 0.001 &#39;**&#39; 0.01 &#39;*&#39; 0.05 &#39;.&#39; 0.1 &#39; &#39; 1</code></pre>
<p><em>BEWARE</em>: Even though the model may look like a strucural equation model (SEM), <em>do not</em> run it like one. Answers will not be the same.</p>
<pre class="r"><code>library(lavaan)
sem1 &lt;- &#39;
fail_z ~ a*d_coddled
d_coddled ~ b*z_coddled
ab := a*b #indirect effect
&#39;

sem1 &lt;- sem(data = x, model = sem1)
summary(sem1)</code></pre>
<pre><code>## lavaan (0.6-1.1186) converged normally after  21 iterations
## 
##   Number of observations                           314
## 
##   Estimator                                         ML
##   Model Fit Test Statistic                       0.835
##   Degrees of freedom                                 1
##   P-value (Chi-square)                           0.361
## 
## Parameter Estimates:
## 
##   Information                                 Expected
##   Information saturated (h1) model          Structured
##   Standard Errors                             Standard
## 
## Regressions:
##                    Estimate  Std.Err  z-value  P(&gt;|z|)
##   fail_z ~                                            
##     d_coddled  (a)    0.107    0.043    2.480    0.013
##   d_coddled ~                                         
##     z_coddled  (b)    0.786    0.042   18.510    0.000
## 
## Variances:
##                    Estimate  Std.Err  z-value  P(&gt;|z|)
##    .fail_z            0.144    0.011   12.530    0.000
##    .d_coddled         0.117    0.009   12.530    0.000
## 
## Defined Parameters:
##                    Estimate  Std.Err  z-value  P(&gt;|z|)
##     ab                0.084    0.034    2.458    0.014</code></pre>
</div>
