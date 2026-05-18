# leadeR <a href="https://github.com/mmukaigawara/leadeR"><img src="man/figures/logo.png" align="right" height="200" alt="leadeR logo" /></a>

<!-- badges: start -->
[![CRAN
status](https://www.r-pkg.org/badges/version/leadeR)](https://CRAN.R-project.org/package=leadeR)
[![CRAN
downloads](https://cranlogs.r-pkg.org/badges/leadeR)](https://cran.r-project.org/package=leadeR)
[![CRAN total
downloads](https://cranlogs.r-pkg.org/badges/grand-total/leadeR)](https://cran.r-project.org/package=leadeR)
<!-- badges: end -->

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

and [CRAN](https://cran.r-project.org/package=leadeR) with:

``` r
install.packages("leadeR")
```

## User's guide

A user's guide is available [online](https://www.mitsurumukaigawara.com/leader/).
