---
title: "Linguistic Data"
author: "Imen, Khushleen, Abigail"
date: "2023-02-21"
output: 
  html_document: 
    keep_md: yes
---


##Column Name Explanations
L1: Native language of the participant. (String)
C: Country of origin of the participant. (String)
L1L2: Linguistic similarity between the native language and the target language. (Integer)
L1L2: Linguistic similarity between the native language and the target language. (Integer)
AaA: Age at arrival in the Netherlands. (Integer)
LoR: Length of residence in the Netherlands. (Integer)
Edu.day: Formal education days in the target language. (Integer)
Sex: Gender of the participant. (String)
Family: Family status of the participant. (String)
ISO639.3: ISO 639-3 codes for the target language. (String)
Enroll: Duration enrolled in language courses. (Integer)
Speaking: Speaking proficiency test score on the State Examination of Dutch as a Second Language. (Integer)
morph: Morphological score related to knowledge structures within words. (Integer)
lex: Lexicon score indicating understanding of written words. (Integer)
new_feat: Feature score reflecting ability to acquire new sounds/grammatical structures. (Integer)
new_sounds: Sound symbols score evaluating pronunciation. (Integer)


```r
library(tidyverse)
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
library(tidyr)
library(janitor)
```

```
## 
## Attaching package: 'janitor'
## 
## The following objects are masked from 'package:stats':
## 
##     chisq.test, fisher.test
```

```r
library(naniar)
library(dplyr) 
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
languages2<- languages %>% select(-"L1L2") %>% 
  plyr::rename(c("C"="Country", "lex"="Lexicon", "morph"="Morphology","new_feat"="New_Features", "new_sounds"="New_Sounds","Edu.day"="Edu_Days"))
```
 

```r
languages2
```

```
## # A tibble: 50,235 × 15
##    L1      Country L2      AaA   LoR Edu_D…¹ Sex   Family ISO63…² Enroll Speak…³
##    <chr>   <chr>   <chr> <dbl> <dbl>   <dbl> <chr> <chr>  <chr>    <dbl>   <dbl>
##  1 Afrika… SouthA… Engl…    25     0       4 Fema… Indo-… afr         93     496
##  2 Afrika… SouthA… Germ…    47     2       4 Male  Indo-… afr         93     542
##  3 Afrika… SouthA… Mono…    23    23       3 Fema… Indo-… afr         93     585
##  4 Afrika… SouthA… Mono…    42     2       2 Fema… Indo-… afr         93     521
##  5 Afrika… SouthA… Mono…    22    19       3 Fema… Indo-… afr         93     639
##  6 Afrika… SouthA… Mono…    37     2       1 Male  Indo-… afr         93     531
##  7 Afrika… SouthA… Mono…    22     6       4 Fema… Indo-… afr         93     566
##  8 Afrika… Sudan   Engl…    31     3       4 Male  Indo-… afr         31     439
##  9 Afrika… Sudan   Engl…    25     4       4 Male  Indo-… afr         31     480
## 10 Afrika… Afghan… Mono…    23     3       4 Fema… Indo-… afr         19     505
## # … with 50,225 more rows, 4 more variables: Morphology <dbl>, Lexicon <dbl>,
## #   New_Features <dbl>, New_Sounds <dbl>, and abbreviated variable names
## #   ¹​Edu_Days, ²​ISO639.3, ³​Speaking
```

##Where the NAs are:

```r
languages2 %>% 
  summarise_all(~(sum(is.na(.))))
```

```
## # A tibble: 1 × 15
##      L1 Country    L2   AaA   LoR Edu_Days   Sex Family ISO639.3 Enroll Speaking
##   <int>   <int> <int> <int> <int>    <int> <int>  <int>    <int>  <int>    <int>
## 1     0       0     0     0     0        9     0      0        0      0        0
## # … with 4 more variables: Morphology <int>, Lexicon <int>, New_Features <int>,
## #   New_Sounds <int>
```
##Distribution of Sex

```r
#languages2 %>% count("Sex") %>% 
#  ggplot(aes(x=Sex,y=freq, fill=Sex))+geom_col()+labs(x="Sex", y="Number of Participants", title = "Representation in the Study by Sex")+geom_text(aes(label=freq),vjust = -0.5)
```

##Distribution of Second Language

```r
#languages2 %>% count("L2") %>% 
 # ggplot(aes(x=L2,y=freq, fill=L2))+geom_col()+labs(x="L2", y="count", title = "Representation of Second Language in the Study Language")+scale_y_log10()+coord_flip()+geom_text(aes(label=freq))
```


```r
languages2 %>% 
  mutate(AaA=as.factor(AaA))
```

```
## # A tibble: 50,235 × 15
##    L1      Country L2    AaA     LoR Edu_D…¹ Sex   Family ISO63…² Enroll Speak…³
##    <chr>   <chr>   <chr> <fct> <dbl>   <dbl> <chr> <chr>  <chr>    <dbl>   <dbl>
##  1 Afrika… SouthA… Engl… 25        0       4 Fema… Indo-… afr         93     496
##  2 Afrika… SouthA… Germ… 47        2       4 Male  Indo-… afr         93     542
##  3 Afrika… SouthA… Mono… 23       23       3 Fema… Indo-… afr         93     585
##  4 Afrika… SouthA… Mono… 42        2       2 Fema… Indo-… afr         93     521
##  5 Afrika… SouthA… Mono… 22       19       3 Fema… Indo-… afr         93     639
##  6 Afrika… SouthA… Mono… 37        2       1 Male  Indo-… afr         93     531
##  7 Afrika… SouthA… Mono… 22        6       4 Fema… Indo-… afr         93     566
##  8 Afrika… Sudan   Engl… 31        3       4 Male  Indo-… afr         31     439
##  9 Afrika… Sudan   Engl… 25        4       4 Male  Indo-… afr         31     480
## 10 Afrika… Afghan… Mono… 23        3       4 Fema… Indo-… afr         19     505
## # … with 50,225 more rows, 4 more variables: Morphology <dbl>, Lexicon <dbl>,
## #   New_Features <dbl>, New_Sounds <dbl>, and abbreviated variable names
## #   ¹​Edu_Days, ²​ISO639.3, ³​Speaking
```
##Distribution of Age at Arrival

```r
#languages2 %>% count("AaA") %>% 
#  ggplot(aes(x=AaA,y=freq))+geom_point()+labs(x="Age at Arrival", y="Number of Participants", title = "Representation of Age at Arrival in the Netherlands")+scale_y_log10()
```

```r
languages2 %>% 
  group_by(AaA) %>% 
  summarise(mean_age_at_arrival=mean(AaA))
```

```
## # A tibble: 73 × 2
##      AaA mean_age_at_arrival
##    <dbl>               <dbl>
##  1     0                   0
##  2     1                   1
##  3     2                   2
##  4     3                   3
##  5     4                   4
##  6     5                   5
##  7     6                   6
##  8     7                   7
##  9     8                   8
## 10     9                   9
## # … with 63 more rows
```


