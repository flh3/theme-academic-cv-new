---
title: 'CR2'
date: 2023-10-24
type: landing

design:
  spacing: '5rem'

# Note: `username` refers to the user's folder name in `content/authors/`

# Page sections
sections:
  - block: resume-experience
    content:
      username: admin
    design:
      # Hugo date format
      date_format: 'January 2006'
      # Education or Experience section first?
      is_education_first: true
  - block: resume-skills
    content:
      title: Skills & Hobbies
      username: admin
    design:
      show_skill_percentage: false
  - block: resume-awards
    content:
      title: Awards
      username: admin
  - block: resume-languages
    content:
      title: Languages
      username: admin
---
 R Packages
 
 CR2: Compute CR0, CR1, CR2 cluster robust standard errors with empirical-degrees of freedom adjustments. (July 2022).

This is now on CRAN so can be installed using: install.packages('CR2'). The development version can also be installed using: `devtools::install_github("flh3/CR2")`.
 

{{<figure src="https://cranlogs.r-pkg.org/badges/CR2" >}}{{<figure src="https://cranlogs.r-pkg.org/badges/grand-total/CR2" >}}

The SPSS version is available [here](https://github.com/flh3/CR2/tree/master/SPSS).

For more details:

Huang, F. & Li, X. (2022). Using cluster robust standard errors when analyzing group randomized trials with few clusters. Behavior Research Methods, 54, 1181-1199. doi: 10.3758/s13428-021-01627-0

Huang, F., Wiedermann, W., & Zhang, B. (2022). Accounting for heteroskedasticity resulting from between-group differences in multilevel models. Multivariate Behavioral Research. doi: 10.1080⁄00273171.2022.2077290.

Huang, F., Zhang, B., & Li, X. (2022). Using robust standard errors for the analysis of binary outcomes with a small number of clusters. Journal of Research on Educational Effectiveness. https://doi.org/10.1080/19345747.2022.2100301
