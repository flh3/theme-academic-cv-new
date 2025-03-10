---
title: ""  # Add a page title.
summary: ""  # Add a page description.
date: "2025-03-03"  # Add today's date.
type: "widget_page"  # Page type is a Widget Page
share: true 
---

{{< toc >}} 

## R packages

📦 **CR2**: Compute CR0, CR1, CR2 cluster robust standard errors with empirical-degrees of freedom adjustments. (July 2022).

This is now on CRAN so can be installed using: install.packages('CR2'). 

{{<figure src="https://cranlogs.r-pkg.org/badges/CR2" >}}{{<figure src="https://cranlogs.r-pkg.org/badges/grand-total/CR2" >}}

The SPSS version is available [here](https://github.com/flh3/CR2/tree/master/SPSS).

For more details:

Huang, F. & Li, X. (2022). Using cluster robust standard errors when analyzing group randomized trials with few clusters. Behavior Research Methods, 54, 1181-1199. doi: 10.3758/s13428-021-01627-0

Huang, F., Wiedermann, W., & Zhang, B. (2022). Accounting for heteroskedasticity resulting from between-group differences in multilevel models. Multivariate Behavioral Research. doi: 10.1080/00273171.2022.2077290.

Huang, F., Zhang, B., & Li, X. (2022). Using robust standard errors for the analysis of binary outcomes with a small number of clusters. Journal of Research on Educational Effectiveness. https://doi.org/10.1080/19345747.2022.2100301

---

📦 **MLMusingR**: Companion package to "*Practical Multilevel Modeling Using R"* (2023).

{{<figure src="https://cranlogs.r-pkg.org/badges/MLMusingR" >}}{{<figure src="https://cranlogs.r-pkg.org/badges/grand-total/MLMusingR" >}}

---

📦 **gendata**: Generate and modify synthetic datasets. Create synthetic datasets based on a correlation table. Additional functions can be used to rescale, transform, and reverse code variables.
[DOC](https://cran.r-project.org/web/packages/gendata/index.html)

{{<figure src="https://cranlogs.r-pkg.org/badges/gendata" >}}{{<figure src="https://cranlogs.r-pkg.org/badges/grand-total/gendata" >}}

---

📦 **hornpa**: A stand-alone function that generates a user specified number of random datasets and computes eigenvalues using the random datasets (i.e., implements Horn's [1965, Psychometrika] parallel analysis). 
[DOC](https://cran.r-project.org/web/packages/hornpa/index.html)
			
{{<figure src="https://cranlogs.r-pkg.org/badges/hornpa" >}}{{<figure src="https://cranlogs.r-pkg.org/badges/grand-total/hornpa" >}}

## Shiny

Testing out Shiny-- a web application framework for R. 

Conduct a [parallel analysis](https://grumble.shinyapps.io/Parallel_analysis/) online to determine the number of components/factors to retain. No need to download any syntax files or packages. _NOTE: For some reason, the EFA PA is not working properly_ (but it does in the package).

## Conducting multilevel CFA in R

I wrote a short note a while back (which I've kept updating) on conducting a multilevel confirmatory factor analysis using R (with *lavaan*). The note and the directions on using the function can be found using this [link](../docs/MCFAinRHUANG.pdf). NOTE: the updated version of *lavaan* now has a feature that automates the cumbersome multilevel setup. Additional notes can be found [here](https://francish.netlify.com/post/multilevel-cfa-mlf/).

Please cite as:

Huang, F. (2017). *Conducting multilevel confirmatory factor analysis using R*. doi: 10.13140/RG.2.2.12391.34724. Retrieved from https://francish.netlify.app/docs/MCFAinRHUANG.pdf

You can access the function at this link on https://github.com/flh3/mcfa.
