---
title: Using cluster robust standard errors to analyze nested data with a few clusters (Korea)
event_url:

location: October 12, 2023, Seoul National Univ, Hoam Faculty House
address:
  street: 
  city: 
  region: 
  postcode: 
  country: S Korea

summary: MLM
abstract: In education, data are often clustered (e.g., students within schools) and various methods (e.g., multilevel modeling, generalized estimating equations) have been developed over the years to properly account for these nonindependent data structures. Ignoring the clustered data structure is well known to result in erroneous statistical inference tests (e.g., type I errors) due to misestimated standard errors and overly liberal degrees of freedom used. One alternative method when analyzing clustered datasets is to use cluster-robust standard errors (CRSEs; CR0) (Liang & Zeger, 1986). CRSEs are often used in various disciplines (e.g., econometrics) though are not common in educational research. A limitation of CRSEs is that, although they work well with a large number of clusters, CRSEs are known to still underestimate standard errors when there are a limited number of clusters (e.g., < 50). This is of particular importance when analyzing data from cluster randomized controlled trials (CRTs) where often, a limited number of clusters is common. However, over 20 years ago, Bell and McCaffrey (2002) proposed an adjustment to the traditional CRSEs and referred to this as the bias-reduced linearization (or the CR2) estimator used together with Satterthwaite (1946) degrees of freedom (df) adjustments. However, the CR2 has not seen much use in the applied literature due to its limited accessibility. Using Monte Carlo simulations (using R), we evaluated the CR2 estimator using conditions often found in educational research using both continuous and binary outcomes (as well as cross classified data structures). Conditions based on the number of clusters, the intraclass correlation coefficient, and group size (among others) were manipulated. Coverage probabilities, type I error rates, and power were assessed. The CR2 estimator results (with and without df adjustments) were compared to results analyzed using the traditional CR0 CRSEs and multilevel models (MLMs). Findings show that the traditional CRSEs (i.e., CR0) had issues with a few clusters but the CR2 results were comparable to those estimated using multilevel models and are a viable alternative when only a few clusters are present. To extend its use for applied researchers, we also provide a free SPSS add-on that can compute these CRSEs.

# Talk start and end times.
#   End time can optionally be hidden by prefixing the line with `#`.
date: '2023-10-12T14:00:00Z'
#date_end: '2024-05-13T15:00:00Z'
all_day: false

# Schedule page publish date (NOT talk date).
publishDate: '2024-05-01T00:00:00Z'

authors:
  - admin

tags: 
- cluster robust standard errors

# Is this a featured talk? (true/false)
featured: true

image:
  #caption: 'Image credit: [**Unsplash**](https://unsplash.com/photos/bzdhc5b3Bxs)'
  focal_point: Right

#links:
#  - icon: twitter
#    icon_pack: fab
#    name: Follow
#    url: https://twitter.com/georgecushen
# url_code: 'https://github.com'
#url_pdf: 'Huang_Presentation_ICER23.pdf'
url_slides: 'Huang_Presentation_ICER23.pdf'
#url_video: 'https://youtube.com'

# Markdown Slides (optional).
#   Associate this talk with Markdown slides.
#   Simply enter your slide deck's filename without extension.
#   E.g. `slides = "example-slides"` references `content/slides/example-slides.md`.
#   Otherwise, set `slides = ""`.
#slides: "Huang_Presentation_ICER23.pdf"

# Projects (optional).
#   Associate this post with one or more of your projects.
#   Simply enter your project's folder or file name without extension.
#   E.g. `projects = ["internal-project"]` references `content/project/deep-learning/index.md`.
#   Otherwise, set `projects = []`.
projects:
  - cluster robust standard errors
---

