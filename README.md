Project 2
================
Yuying Zhou
10/12/2020

  - [Data](#data)

\#Package List

``` r
library(tidyverse)
library(caret)
```

# Data

``` r
Data<-read_csv("../OnlineNewsPopularity.csv")
Monday<-Data%>%filter(weekday_is_monday==1)
```
