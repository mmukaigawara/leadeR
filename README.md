# leadeR <a href="https://github.com/mmukaigawara/leadeR"><img src="inst/figure/logo.png" align="right" height="200" /></a>

<!-- badges: start
[![CRAN
status](https://www.r-pkg.org/badges/version/leadeR)](https://CRAN.R-project.org/package=leadeR)
[![CRAN
downloads](https://cranlogs.r-pkg.org/badges/leadeR)](https://cran.r-project.org/package=leadeR)
[![CRAN total
downloads](https://cranlogs.r-pkg.org/badges/grand-total/leadeR)](https://cran.r-project.org/package=leadeR)
badges: end -->

The goal of the package
[leadeR](https://github.com/mmukaigawara/leadeR) is to profile political leaders based on 
Leadership Trait Analysis (LTA) and Operational Code Analysis (OCA). 
Users provide text data and the package performs the analyses.

## Prerequisites

- **Python** (3.8 or later)
- **spaCy** Python library with an English language model installed
- **spacyr** R package (interface between R and spaCy)

To install spaCy and the English model:

```bash
pip install spacy
python -m spacy download en_core_web_sm
```

## Installation

You can install the package
[leadeR](https://github.com/mmukaigawara/leadeR) from
[GitHub](https://github.com/mmukaigawara/leadeR) with:

```r
# install.packages("remotes")
remotes::install_github("mmukaigawara/leadeR")
```

## Usage

Before using leadeR functions, initialize spaCy:

```r
library(leadeR)
spacyr::spacy_initialize()
```

### 1. Leadership Trait Analysis (LTA)

```r
# Power
res_power <- get_power(own_entity = "Malawi", 
                       text = "We will help to improve relations with our neighbors.")
print(res_power)

# Affiliation
res_aff <- get_aff(own_entity = "Malawi", 
                   text = "We will help to improve relations with our neighbors.")
print(res_aff)

# Distrust
res_dist <- get_dist(own_entity = "Malawi",
                     text = "We will help to improve relations with our neighbors.")
print(res_dist)

# Complexity
res_complex <- get_complex(text = "We will help to improve relations with our neighbors.")
print(res_complex)

# Task
res_task <- get_task(text = "We will help to improve relations with our neighbors.")
print(res_task)
```

### 2. Operational Code Analysis (OCA)

```r
# Perform Operational Code Analysis on a speech
result <- get_opc("Malawi", "We will help to improve relations with our neighbors.")
print(result)
```
