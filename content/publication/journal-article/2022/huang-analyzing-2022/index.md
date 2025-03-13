---
title: Analyzing cross-sectionally clustered data using generalized estimating equations

# Authors
# A YAML list of author names
# If you created a profile for a user (e.g. the default `admin` user at `content/authors/admin/`), 
# write the username (folder name) here, and it will be replaced with their full name and linked to their profile.
authors:
- admin

# Author notes (such as 'Equal Contribution')
# A YAML list of notes for each author in the above `authors` list
author_notes: []

date: '2022-01-01'

# Date to publish webpage (NOT necessarily Bibtex publication's date).
publishDate: '2025-03-11T14:50:41.417327Z'

# Publication type.
# A single CSL publication type but formatted as a YAML list (for Hugo requirements).
publication_types:
- article-journal

# Publication name and optional abbreviated publication name.
publication: '*Journal of Educational and Behavioral Statistics*'
publication_short: 'https://doi.org/10.3102/1076998621101748'

doi: ''

abstract: 'The presence of clustered data is common in the sociobehavioral sciences. One approach that specifically deals with clustered data but has seen little use in education is the generalized estimating equations (GEEs) approach. We provide a background on GEEs, discuss why it is appropriate for the analysis of clustered data, and provide worked examples using both continuous and binary outcomes. Comparisons are made between GEEs, multilevel models, and ordinary least squares results to highlight similarities and differences between the approaches. Detailed walkthroughs are provided using both R and SPSS Version 26.'

# Summary. An optional shortened abstract.
summary: ''

tags:
- clustered data
- generalized estimating equations

# Display this page in a list of Featured pages?
featured: true

# Links
url_pdf: 'publication/journal-article/2022/huang-analyzing-2022/GEE_Jebs_appendices.pdf'
url_code: 'https://github.com/flh3/jebsgee'
url_dataset: 'cont_binary.sav'
url_poster: ''
url_project: ''
url_slides: ''
url_source: ''
url_video: ''

# Custom links (uncomment lines below)
# links:
# - name: Custom Link
#   url: http://example.org

# Publication image
# Add an image named `featured.jpg/png` to your page's folder then add a caption below.
image:
  caption: ''
  focal_point: ''
  preview_only: false

# Associated Projects (optional).
#   Associate this publication with one or more of your projects.
#   Simply enter your project's folder or file name without extension.
#   E.g. `projects: ['internal-project']` links to `content/project/internal-project/index.md`.
#   Otherwise, set `projects: []`.
projects: []
---

As of 2024.10.03, the most read article on JEBS (for the last 6 months)!

![jebs_most_read.png](jebs_most_read.png)

In the original paper draft, I had a section which showed how much more widely used mixed models (i.e., MLMs, HLMs) were compared to GEEs but was asked to remove that. I thought the usage was interesting so I am including it here:

- In psychology, mixed model studies are much more popular than studies using GEEs by a ratio of 15:1 (Bauer & Sterba, 2011). 

Citations in JEBS:

- In the Journal of Educational and Behavioral Statistics (JEBS): one article on how to use multilevel models by Singer (1998) has over 3,300 citations (as of 2020.06.11, Google Scholar)
- In the same journal, Ghisletta & Sini (2004) provided an introduction to GEEs. This article has 329 citations. GS wrote (p. 431):

Although GEEs are widely applied in biological, pharmacological, and closely related disciplines, their application in educational and social sciences remains relatively scarce.

There is a difference of 6 years but the Singer article has been cited over **10** times more! If using average citations per year, **7.5** times more.

References:

Ghisletta, P., & Spini, D. (2004). An introduction to generalized estimating equations and an application to assess selectivity effects in a longitudinal study on very old individuals. Journal of Educational and Behavioral Statistics, 29(4), 421-437.

Singer, J. D. (1998). Using SAS PROC MIXED to fit multilevel models, hierarchical models, and individual growth models. Journal of Educational and Behavioral Statistics, 23(4), 323-355.
