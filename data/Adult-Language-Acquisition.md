---
title: "Linguistic Data"
author: "Imen"
date: "2023-02-21"
output: 
  html_document: 
    keep_md: yes
---




```r
library("tidyverse")
```

```
## ── Attaching packages ─────────────────────────────────────── tidyverse 1.3.2 ──
## ✔ ggplot2 3.4.0      ✔ purrr   1.0.0 
## ✔ tibble  3.1.8      ✔ dplyr   1.0.10
## ✔ tidyr   1.2.1      ✔ stringr 1.5.0 
## ✔ readr   2.1.3      ✔ forcats 0.5.2 
## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
## ✖ dplyr::filter() masks stats::filter()
## ✖ dplyr::lag()    masks stats::lag()
```

```r
library("tidyr")
library("here")
```

```
## here() starts at /Users/ibelhadj/Documents/GitHub/BIS15FinalProject_ibelhadj
```


```r
languages <- readr::read_csv("stex.csv")
```

```
## Rows: 50235 Columns: 16
## ── Column specification ────────────────────────────────────────────────────────
## Delimiter: ","
## chr (7): L1, C, L1L2, L2, Sex, Family, ISO639.3
## dbl (9): AaA, LoR, Edu.day, Enroll, Speaking, morph, lex, new_feat, new_sounds
## 
## ℹ Use `spec()` to retrieve the full column specification for this data.
## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
```

```r
languages
```

```
## # A tibble: 50,235 × 16
##    L1        C       L1L2  L2      AaA   LoR Edu.day Sex   Family ISO63…¹ Enroll
##    <chr>     <chr>   <chr> <chr> <dbl> <dbl>   <dbl> <chr> <chr>  <chr>    <dbl>
##  1 Afrikaans SouthA… Afri… Engl…    25     0       4 Fema… Indo-… afr         93
##  2 Afrikaans SouthA… Afri… Germ…    47     2       4 Male  Indo-… afr         93
##  3 Afrikaans SouthA… Afri… Mono…    23    23       3 Fema… Indo-… afr         93
##  4 Afrikaans SouthA… Afri… Mono…    42     2       2 Fema… Indo-… afr         93
##  5 Afrikaans SouthA… Afri… Mono…    22    19       3 Fema… Indo-… afr         93
##  6 Afrikaans SouthA… Afri… Mono…    37     2       1 Male  Indo-… afr         93
##  7 Afrikaans SouthA… Afri… Mono…    22     6       4 Fema… Indo-… afr         93
##  8 Afrikaans Sudan   Afri… Engl…    31     3       4 Male  Indo-… afr         31
##  9 Afrikaans Sudan   Afri… Engl…    25     4       4 Male  Indo-… afr         31
## 10 Afrikaans Afghan… Afri… Mono…    23     3       4 Fema… Indo-… afr         19
## # … with 50,225 more rows, 5 more variables: Speaking <dbl>, morph <dbl>,
## #   lex <dbl>, new_feat <dbl>, new_sounds <dbl>, and abbreviated variable name
## #   ¹​ISO639.3
```
 

