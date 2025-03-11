---
title: Using cluster-robust standard errors when analyzing group-randomized trials
  with few clusters

# Authors
# A YAML list of author names
# If you created a profile for a user (e.g. the default `admin` user at `content/authors/admin/`), 
# write the username (folder name) here, and it will be replaced with their full name and linked to their profile.
authors:
- admin
- Xintong Li

# Author notes (such as 'Equal Contribution')
# A YAML list of notes for each author in the above `authors` list
author_notes: []

date: '2022-01-01'

# Date to publish webpage (NOT necessarily Bibtex publication's date).
publishDate: '2025-03-11T14:50:41.440289Z'

# Publication type.
# A single CSL publication type but formatted as a YAML list (for Hugo requirements).
publication_types:
- article-journal

# Publication name and optional abbreviated publication name.
publication: '*Behavior Research Methods*'
publication_short: ''

doi: 10.3758/s13428-021-01627-0

abstract: 'Accounting for dependent observations in cluster-randomized trials (CRTs) using nested data is necessary in order to avoid misestimated standard errors resulting in questionable inferential statistics. Cluster-robust standard errors (CRSEs) are often used to address this issue. However, CRSEs are still well-known to underestimate standard errors for group-level variables when the number of clusters is low (e.g., < 50) and with CRTs, a small number of clusters, due to logistical or financial considerations, is the norm rather than the exception. Using a simulation with various conditions, we investigate the use of a small sample correction (i.e., CR2 estimator) proposed by Bell and McCaffrey (2002) together with empirically derived degrees of freedom estimates (dofBM). Findings indicate that even with as few as 10 clusters, the CR2 estimator used with dofBM yields generally unbiased results with acceptable type I error and coverage rates. Results show that coverage and type I error rates can be largely influenced by the choice of dof, not just the standard error adjustments. An applied example is provided together with R syntax to conduct the analysis. To facilitate the use of different CRSEs, a free graphical, menu-driven SPSS add-on to compute the various cluster-robust variance estimates can be downloaded from https://github.com/flh3/CR2/tree/master/SPSS.'

# Summary. An optional shortened abstract.
summary: ''

tags:
- cluster robust standard errors
- CRSE
- clustered data

# Display this page in a list of Featured pages?
featured: true

# Links
url_pdf: ''
url_code: ''
url_dataset: ''
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
links:
- name: URL
  url: https://link.springer.com/10.3758/s13428-021-01627-0
---

The SPSS version can be accessed here: https://github.com/flh3/CR2/tree/master/SPSS