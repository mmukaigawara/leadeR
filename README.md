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

### Initialization

Before using leadeR functions, initialize spaCy:

```r
library(leadeR)
spacyr::spacy_initialize()
```

### Data

The package includes a sample [speech][https://www.jfklibrary.org/archives/other-resources/john-f-kennedy-speeches/university-of-pennsylvania-19571101] by John F. Kennedy (November 1, 1957) as `jfk19571101`.

```r
head(jfk19571101)
```

```r
[1] "The theme of my remarks today is on the new dimensions of American Foreign Policy. 
I realize only too well the twin perils of such a title. It either stimulates the hope 
...
```

### Preprocessing

Clean transcript annotations before analysis. The `clean_text()` function removes editorial annotations in brackets, parentheses, and curly braces from text, and then normalizes whitespace. Users might need to clean the text data further as needed.

```r
jfk <- clean_text(jfk)
```

### Leadership Trait Analysis (LTA)

Users can obtain LTA scores (e.g., P and OP for power) one by one. By default, the `bootstrap` option is set to be false. Users need to set `bootstrap = T` to generate bootstrapped confidence intervals. The number of iterations is `B = 1000` by default.

```r
res_nat     <- get_nat(own_entity = own_ent, text = jfk, bootstrap = T, B = B) # Nationalism
res_ctrl    <- get_ctrl(own_entity = own_ent, text = jfk, bootstrap = T, B = B) # Control
res_power   <- get_power(own_entity = own_ent, text = jfk, bootstrap = T, B = B) # Power
res_aff     <- get_aff(own_entity = own_ent, text = jfk, bootstrap = T, B = B) # Affiliation
res_dist    <- get_dist(own_entity = own_ent, text = jfk, bootstrap = T, B = B) # Distrust
res_complex <- get_complex(text = jfk, bootstrap = T, B = B) # Complexity
res_conf    <- get_conf(text = jfk, bootstrap = T, B = B) # Confidence
res_task    <- get_task(text = jfk, bootstrap = T, B = B) # Task
```

```r
> print(res_nat)
# A tibble: 1 × 4
  meanN meanON  varN varON
  <dbl>  <dbl> <dbl> <dbl>
1  10.9   137.  10.0  203.
> print(res_ctrl)
# A tibble: 1 × 4
  meanIC meanOC varIC varOC
   <dbl>  <dbl> <dbl> <dbl>
1   19.9   30.1  21.3  28.2
> print(res_power)
# A tibble: 1 × 4
  meanP meanOP  varP varOP
  <dbl>  <dbl> <dbl> <dbl>
1  14.0   55.0  15.1  82.9
> print(res_aff)
# A tibble: 1 × 4
  meanA meanOA  varA varOA
  <dbl>  <dbl> <dbl> <dbl>
1  2.10   32.1  1.98  25.0
> print(res_dist)
# A tibble: 1 × 4
  meanS  varS meanOS varOS
  <dbl> <dbl>  <dbl> <dbl>
1  37.3  114.   155.  270.
> print(res_complex)
# A tibble: 1 × 4
  meanHC varHC meanLC varLC
   <dbl> <dbl>  <dbl> <dbl>
1   227.  336.   125.  189.
> print(res_conf)
# A tibble: 1 × 4
  meanSC meanOSC varSC varOSC
   <dbl>   <dbl> <dbl>  <dbl>
1   2.08    18.1  2.00   19.4
> print(res_task)
# A tibble: 1 × 4
  meanTI varTI meanIP varIP
   <dbl> <dbl>  <dbl> <dbl>
1   145.  243.   52.9  51.8
```

In practice, it is helpful to run these analyses all at once, with the combined scores such as `Na = meanN / (meanN + meanON)`. To do so, users can run the `get_lta()` function.

```r
res_lta <- get_lta(own_entity = own_ent, text = jfk, bootstrap = T, B = B)
print(res_lta)
```

The code generates all the raw scores as well as combined ones with their CIs (based on the Delta method).

```r
# A tibble: 1 × 50
  meanP meanOP  varP varOP meanA meanOA  varA varOA meanS  varS meanOS
  <dbl>  <dbl> <dbl> <dbl> <dbl>  <dbl> <dbl> <dbl> <dbl> <dbl>  <dbl>
1  14.1   55.0  15.3  77.1  2.05   31.9  2.04  24.4  38.4  115.   155.
# ℹ 39 more variables: ...
```

### Operational Code Analysis (OCA)

```r
res_oca <- get_oca(own_entity = own_ent, text = jfk, bootstrap = T, B = B)
print(res_oca)
```
