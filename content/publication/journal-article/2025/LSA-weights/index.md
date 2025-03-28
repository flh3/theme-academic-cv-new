---
title: Reassessing weights in large-scale assessments and multilevel models

# Authors
# A YAML list of author names
# If you created a profile for a user (e.g. the default `admin` user at `content/authors/admin/`), 
# write the username (folder name) here, and it will be replaced with their full name and linked to their profile.
authors:
- Umut Atasever
- admin
- Leslie Rutkowski

# Author notes (such as 'Equal Contribution')
# A YAML list of notes for each author in the above `authors` list
author_notes: []

date: '2025-03-28'

# Date to publish webpage (NOT necessarily Bibtex publication's date).
publishDate: '2025-03-28T17:49:07.362198Z'

# Publication type.
# A single CSL publication type but formatted as a YAML list (for Hugo requirements).
publication_types:
- article-journal

# Publication name and optional abbreviated publication name.
publication: '*Large-scale Assessments in Education*'
publication_short: 'LSA'

doi: 10.1186/s40536-025-00245-y

abstract: 'When analyzing large-scale assessments (LSAs) that use complex sampling designs, it is important to account for probability sampling using weights. However, the use of these weights in multilevel models has been widely debated, particularly regarding their application at different levels of the model. Yet, no consensus has been reached on the best method to apply weights. To address this, we conducted a Monte Carlo simulation, modeling a typical LSA population with known true values for the variables of interest. Using repeated sampling from this population, we generated weights using a stratified two-stage cluster design, where clusters (schools) were selected using probability proportional to size (PPS) sampling from designated explicit strata. We examined both class-level and student-level sampling structures and applied a nonresponse model at both the school and student levels. For each sample drawn, we assessed bias and coverage rates across models that applied weights at two levels, only at level 2, only at level 1, and without weights. Our findings show that applying only level-2 weights produced the most precise estimates, while models with no weights or only rescaled level-1 weights led to the highest bias. Using both level-1 and level-2 weights together was acceptable, although variance components were slightly underestimated. However, scaling level-1 weights would mirror using only the level-2 weights in datasets where there is no variation of weights within clusters. An applied example using TIMSS data supports these findings. This study contributes to the literature by explaining the least biased weight methods with complex sampling scenarios and offering practical guidance on using weights in multilevel models. We provide the R syntax for both the simulation and the applied example for reproducibility.'

# Summary. An optional shortened abstract.
summary: ''

tags: 
- large-scale assessments
- weights
- multilevel models
- clustered data

# Display this page in a list of Featured pages?
featured: true

# Links
url_pdf: 'https://largescaleassessmentsineducation.springeropen.com/articles/10.1186/s40536-025-00245-y'
url_code: 'https://www.dropbox.com/scl/fo/w1sv538tpf5w1x88kbfob/AOo59MPeaU-PVBYgc1S3tRo?rlkey=cp07fzk645uwn6dhxnqnf7k4f&e=1&st=swiruq1y&dl=0'
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
  url: https://largescaleassessmentsineducation.springeropen.com/articles/10.1186/s40536-025-00245-y
---

