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

Before using leadeR functions, initialize spaCy and set the seed.

```r
library(leadeR)
library(data.table)

spacyr::spacy_initialize()

set.seed(02138)
```

### Data

The package includes three sample speech data (`jfk19560921`, `jfk19570702`, and `jfk19571101`) of President John F. Kennedy.

```r
head(jfk19571101)
```

```r
[1] "The theme of my remarks today is on the new dimensions of American Foreign Policy. 
I realize only too well the twin perils of such a title. It either stimulates the hope 
...
```

### Preprocessing

Clean transcript annotations before analysis. 
The `clean_text()` function removes editorial annotations in brackets, parentheses, and curly braces from text, and then normalizes whitespace. 
Users might need to clean the text data further as needed.

```r
jfk1 <- clean_text(jfk19560921)
jfk2 <- clean_text(jfk19570702)
jfk3 <- clean_text(jfk19571101)
```

### Leadership Trait Analysis (LTA)

Users can run LTA using multiple text data and produce all the trait scores with the `get_lta()` function. 
By default, the `bootstrap` option is set to be false. 
Users need to set `bootstrap = T` to generate bootstrapped confidence intervals. 
The number of iterations is `B = 1000` by default.

```r
B = 1000

res_lta <- data.table::rbindlist(
  lapply(c(jfk1, jfk2, jfk3), function(x) 
    get_lta(own_entity = own_ent, text = x, bootstrap = T, B = B))
  )

print(res_lta)
```

```r
    meanP meanOP     varP     varOP  meanA meanOA     varA    varOA   meanS
    <num>  <num>    <num>     <num>  <num>  <num>    <num>    <num>   <num>
1: 13.915 70.231 21.87765 132.84849  9.962 32.043 9.079636 23.33449  24.570
2: 36.615 92.010 66.68346 122.01992 11.095 44.260 9.871847 36.32873 129.960
3: 13.925 55.148 15.06244  81.72582  1.946 31.923 1.914999 27.74081  37.782
        varS  meanOS    varOS  meanHC    varHC  meanLC    varLC  meanTI
       <num>   <num>    <num>   <num>    <num>   <num>    <num>   <num>
1:  65.45255  52.014  96.5003 186.318 355.1700 103.024 172.0995 149.700
2: 502.17858 459.652 865.4944 353.134 498.0941 212.485 292.1439 242.273
3: 104.03051 154.060 265.9684 228.681 342.3216 125.186 213.4188 144.619
      varTI  meanIP     varIP meanSC meanOSC     varSC   varOSC  meanN  meanON
      <num>   <num>     <num>  <num>   <num>     <num>    <num>  <num>   <num>
1: 138.0320  53.704  72.54693  3.993  43.037  4.020972 85.95759 17.011  52.730
2: 359.8083 142.664 147.37848 15.903  40.423 28.269861 62.37044 30.840 306.555
3: 238.6745  52.939  50.58987  1.968  17.909  1.930907 17.40813 11.061 137.135
       varN     varON meanIC meanOC    varIC    varOC        Pp       varPp
      <num>     <num>  <num>  <num>    <num>    <num>     <num>       <num>
1: 15.72460  68.35145 31.948 35.917 39.77507 34.62073 0.1653673 0.002665492
2: 36.77918 508.98596 34.461 57.213 61.85233 72.10373 0.2846647 0.002660116
3: 10.77205 214.51929 19.836 29.632 20.60771 30.17875 0.2015983 0.002708605
           D        varD         C         varC        Ta        varTa
       <num>       <num>     <num>        <num>     <num>        <num>
1: 0.3208242 0.006841226 0.6439369 0.0013902574 0.7359737 0.0011823545
2: 0.2204161 0.000998866 0.6243319 0.0005756638 0.6293835 0.0007275252
3: 0.1969433 0.002103213 0.6462343 0.0010538840 0.7320331 0.0011337174
           Ss       varSs         Na        varNa         B        varB
        <num>       <num>      <num>        <num>     <num>       <num>
1: 0.08490325 0.001802497 0.24391678 0.0026842661 0.4707581 0.004084821
2: 0.28233853 0.006156402 0.09140622 0.0003040830 0.3759081 0.004078911
3: 0.09900890 0.004399267 0.07463764 0.0004744136 0.4009865 0.005004672
```

Users can also obtain LTA scores (e.g., P and OP for power) one by one. 
The usual `print()` function shows the output of each function.

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

### Operational Code Analysis (OCA)

For OCA, the `get_oca()` function generates the raw count measures as well as the 10 indices (I1-5 and P1-5).

```r
res_oca <- get_oca(own_entity = own_ent, text = jfk, bootstrap = T, B = B)
print(res_oca)
```

```r
# A tibble: 1 × 56
  meanP1 meanP2 meanP3 meanP4 meanP5 meanI1 meanI2 meanI3 meanI4a meanI4b
   <dbl>  <dbl>  <dbl>  <dbl>  <dbl>  <dbl>  <dbl>  <dbl>   <dbl>   <dbl>
1  0.638  0.547  0.217  0.220  0.952  0.788  0.539  0.248   0.212   0.762
# ℹ 46 more variables: ...
```
