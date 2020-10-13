Project 2
================
Yuying Zhou
10/12/2020

  - [Introduction](#introduction)
  - [Data](#data)
  - [Summarizations](#summarizations)

\#Package List

``` r
library(tidyverse)
library(caret)
library(GGally)
library(knitr)
```

# Introduction

The data analyzed in the project is a heterogeneous set of features
about articles published by Mashable in a period of two years. It
includes 61 variables. The response varialbe is share. The purpose of
this project is to build two models- a tree-based model and a boosted
tree model- to see which model performs better at predicting the number
of shares in social networks. This data set include data for Monday to
Sunday, the analysis conducted the same analysis for each day and to see
which model suit them.

This analysis did not include all of the predicators. I used the
following

  - n\_tokens\_title: Number of words in the title  
  - n\_unique\_tokens: Rate of unique words in the content  
  - num\_hrefs: Number of links  
  - num\_imgs: Number of images  
  - average\_token\_length: Average length of the words in the content  
  - data\_channel\_is\_lifestyle: Is data channel ‘Lifestyle’?  
  - global\_rate\_positive\_words: Rate of positive words in the
    content  
  - avg\_positive\_polarity: Avg. polarity of positive words  
  - abs\_title\_subjectivity: Absolute subjectivity level  
  - self\_reference\_avg\_sharess: Avg. shares of referenced articles in
    Mashable  
  - kw\_avg\_min: Worst keyword (avg. shares)  
  - kw\_avg\_max: Best keyword (avg. shares)  
  - kw\_avg\_avg: Avg. keyword (avg. shares)
  - shares: Number of shares (target)

According to the correlation table, mullinearity is not an issue for the
data set.

``` r
Data<-read_csv("OnlineNewsPopularity.csv")
Data<-Data%>%select(n_tokens_title, n_unique_tokens,num_hrefs, num_imgs, average_token_length, data_channel_is_lifestyle, global_rate_positive_words,avg_positive_polarity, abs_title_subjectivity,self_reference_avg_sharess, kw_avg_min, kw_avg_max, kw_avg_avg, shares, starts_with("weekday") )
res<-cor(Data)
tab<-round(res,2)
kable(tab)
```

|                               | n\_tokens\_title | n\_unique\_tokens | num\_hrefs | num\_imgs | average\_token\_length | data\_channel\_is\_lifestyle | global\_rate\_positive\_words | avg\_positive\_polarity | abs\_title\_subjectivity | self\_reference\_avg\_sharess | kw\_avg\_min | kw\_avg\_max | kw\_avg\_avg | shares | weekday\_is\_monday | weekday\_is\_tuesday | weekday\_is\_wednesday | weekday\_is\_thursday | weekday\_is\_friday | weekday\_is\_saturday | weekday\_is\_sunday |
| :---------------------------- | ---------------: | ----------------: | ---------: | --------: | ---------------------: | ---------------------------: | ----------------------------: | ----------------------: | -----------------------: | ----------------------------: | -----------: | -----------: | -----------: | -----: | ------------------: | -------------------: | ---------------------: | --------------------: | ------------------: | --------------------: | ------------------: |
| n\_tokens\_title              |             1.00 |            \-0.01 |     \-0.05 |    \-0.01 |                 \-0.07 |                       \-0.07 |                        \-0.06 |                  \-0.05 |                   \-0.15 |                          0.00 |       \-0.03 |         0.12 |         0.00 |   0.01 |                0.00 |                 0.01 |                   0.01 |                \-0.02 |                0.00 |                \-0.02 |                0.01 |
| n\_unique\_tokens             |           \-0.01 |              1.00 |       0.00 |      0.02 |                   0.03 |                         0.00 |                          0.00 |                    0.00 |                   \-0.01 |                          0.00 |         0.00 |         0.00 |         0.00 |   0.00 |                0.00 |                 0.01 |                   0.00 |                  0.00 |                0.00 |                  0.00 |                0.00 |
| num\_hrefs                    |           \-0.05 |              0.00 |       1.00 |      0.34 |                   0.22 |                         0.05 |                          0.06 |                    0.19 |                     0.01 |                          0.03 |         0.01 |       \-0.02 |         0.12 |   0.05 |              \-0.01 |               \-0.01 |                 \-0.03 |                \-0.01 |                0.00 |                  0.05 |                0.04 |
| num\_imgs                     |           \-0.01 |              0.02 |       0.34 |      1.00 |                   0.03 |                         0.01 |                        \-0.04 |                    0.10 |                   \-0.01 |                          0.02 |       \-0.01 |         0.00 |         0.15 |   0.04 |              \-0.01 |                 0.00 |                 \-0.02 |                \-0.01 |              \-0.01 |                  0.03 |                0.04 |
| average\_token\_length        |           \-0.07 |              0.03 |       0.22 |      0.03 |                   1.00 |                         0.01 |                          0.32 |                    0.54 |                     0.03 |                          0.04 |         0.01 |       \-0.16 |       \-0.14 | \-0.02 |                0.00 |                 0.00 |                   0.00 |                  0.00 |                0.00 |                  0.00 |                0.01 |
| data\_channel\_is\_lifestyle  |           \-0.07 |              0.00 |       0.05 |      0.01 |                   0.01 |                         1.00 |                          0.06 |                    0.07 |                     0.01 |                          0.00 |         0.04 |       \-0.13 |         0.05 |   0.01 |              \-0.01 |               \-0.02 |                   0.00 |                \-0.01 |                0.00 |                  0.02 |                0.03 |
| global\_rate\_positive\_words |           \-0.06 |              0.00 |       0.06 |    \-0.04 |                   0.32 |                         0.06 |                          1.00 |                    0.33 |                   \-0.14 |                          0.01 |         0.03 |       \-0.11 |       \-0.01 |   0.00 |              \-0.01 |                 0.00 |                 \-0.01 |                  0.00 |              \-0.02 |                  0.02 |                0.02 |
| avg\_positive\_polarity       |           \-0.05 |              0.00 |       0.19 |      0.10 |                   0.54 |                         0.07 |                          0.33 |                    1.00 |                     0.02 |                          0.04 |         0.02 |       \-0.07 |         0.04 |   0.01 |                0.00 |               \-0.01 |                 \-0.01 |                \-0.01 |                0.00 |                  0.01 |                0.03 |
| abs\_title\_subjectivity      |           \-0.15 |            \-0.01 |       0.01 |    \-0.01 |                   0.03 |                         0.01 |                        \-0.14 |                    0.02 |                     1.00 |                          0.00 |         0.00 |       \-0.02 |       \-0.02 |   0.00 |                0.00 |                 0.01 |                   0.01 |                  0.00 |                0.01 |                \-0.02 |              \-0.03 |
| self\_reference\_avg\_sharess |             0.00 |              0.00 |       0.03 |      0.02 |                   0.04 |                         0.00 |                          0.01 |                    0.04 |                     0.00 |                          1.00 |         0.03 |         0.09 |         0.17 |   0.06 |                0.00 |                 0.00 |                   0.00 |                  0.00 |                0.00 |                \-0.01 |                0.00 |
| kw\_avg\_min                  |           \-0.03 |              0.00 |       0.01 |    \-0.01 |                   0.01 |                         0.04 |                          0.03 |                    0.02 |                     0.00 |                          0.03 |         1.00 |       \-0.13 |         0.38 |   0.03 |                0.00 |                 0.00 |                   0.00 |                  0.00 |                0.00 |                  0.00 |                0.00 |
| kw\_avg\_max                  |             0.12 |              0.00 |     \-0.02 |      0.00 |                 \-0.16 |                       \-0.13 |                        \-0.11 |                  \-0.07 |                   \-0.02 |                          0.09 |       \-0.13 |         1.00 |         0.43 |   0.04 |                0.00 |                 0.01 |                   0.01 |                  0.01 |                0.00 |                \-0.01 |              \-0.03 |
| kw\_avg\_avg                  |             0.00 |              0.00 |       0.12 |      0.15 |                 \-0.14 |                         0.05 |                        \-0.01 |                    0.04 |                   \-0.02 |                          0.17 |         0.38 |         0.43 |         1.00 |   0.11 |              \-0.02 |                 0.00 |                 \-0.01 |                  0.00 |                0.00 |                  0.03 |                0.03 |
| shares                        |             0.01 |              0.00 |       0.05 |      0.04 |                 \-0.02 |                         0.01 |                          0.00 |                    0.01 |                     0.00 |                          0.06 |         0.03 |         0.04 |         0.11 |   1.00 |                0.01 |               \-0.01 |                   0.00 |                \-0.01 |                0.00 |                  0.02 |                0.01 |
| weekday\_is\_monday           |             0.00 |              0.00 |     \-0.01 |    \-0.01 |                   0.00 |                       \-0.01 |                        \-0.01 |                    0.00 |                     0.00 |                          0.00 |         0.00 |         0.00 |       \-0.02 |   0.01 |                1.00 |               \-0.22 |                 \-0.22 |                \-0.21 |              \-0.18 |                \-0.12 |              \-0.12 |
| weekday\_is\_tuesday          |             0.01 |              0.01 |     \-0.01 |      0.00 |                   0.00 |                       \-0.02 |                          0.00 |                  \-0.01 |                     0.01 |                          0.00 |         0.00 |         0.01 |         0.00 | \-0.01 |              \-0.22 |                 1.00 |                 \-0.23 |                \-0.23 |              \-0.20 |                \-0.12 |              \-0.13 |
| weekday\_is\_wednesday        |             0.01 |              0.00 |     \-0.03 |    \-0.02 |                   0.00 |                         0.00 |                        \-0.01 |                  \-0.01 |                     0.01 |                          0.00 |         0.00 |         0.01 |       \-0.01 |   0.00 |              \-0.22 |               \-0.23 |                   1.00 |                \-0.23 |              \-0.20 |                \-0.12 |              \-0.13 |
| weekday\_is\_thursday         |           \-0.02 |              0.00 |     \-0.01 |    \-0.01 |                   0.00 |                       \-0.01 |                          0.00 |                  \-0.01 |                     0.00 |                          0.00 |         0.00 |         0.01 |         0.00 | \-0.01 |              \-0.21 |               \-0.23 |                 \-0.23 |                  1.00 |              \-0.19 |                \-0.12 |              \-0.13 |
| weekday\_is\_friday           |             0.00 |              0.00 |       0.00 |    \-0.01 |                   0.00 |                         0.00 |                        \-0.02 |                    0.00 |                     0.01 |                          0.00 |         0.00 |         0.00 |         0.00 |   0.00 |              \-0.18 |               \-0.20 |                 \-0.20 |                \-0.19 |                1.00 |                \-0.11 |              \-0.11 |
| weekday\_is\_saturday         |           \-0.02 |              0.00 |       0.05 |      0.03 |                   0.00 |                         0.02 |                          0.02 |                    0.01 |                   \-0.02 |                        \-0.01 |         0.00 |       \-0.01 |         0.03 |   0.02 |              \-0.12 |               \-0.12 |                 \-0.12 |                \-0.12 |              \-0.11 |                  1.00 |              \-0.07 |
| weekday\_is\_sunday           |             0.01 |              0.00 |       0.04 |      0.04 |                   0.01 |                         0.03 |                          0.02 |                    0.03 |                   \-0.03 |                          0.00 |         0.00 |       \-0.03 |         0.03 |   0.01 |              \-0.12 |               \-0.13 |                 \-0.13 |                \-0.13 |              \-0.11 |                \-0.07 |                1.00 |

# Data

I read data from the folder and then split daily data set into train and
test set.

``` r
dayData<-Data%>%filter(weekday_is_monday==1)

set.seed(1)
train <- sample(1:nrow(dayData), size = nrow(dayData)*0.7)
test <- dplyr::setdiff(1:nrow(dayData), train)
dayDataTrain <- dayData[train, ]
dayDataTest <- dayData[test, ]
```

# Summarizations

Based on the summary table, no missing values. Most articles have zero
shares.

``` r
summary(dayDataTrain)
```

    ##  n_tokens_title  n_unique_tokens    num_hrefs         num_imgs     
    ##  Min.   : 2.00   Min.   :0.0000   Min.   :  0.00   Min.   : 0.000  
    ##  1st Qu.: 9.00   1st Qu.:0.4738   1st Qu.:  4.00   1st Qu.: 1.000  
    ##  Median :10.00   Median :0.5427   Median :  7.00   Median : 1.000  
    ##  Mean   :10.42   Mean   :0.5308   Mean   : 10.62   Mean   : 4.382  
    ##  3rd Qu.:12.00   3rd Qu.:0.6088   3rd Qu.: 13.00   3rd Qu.: 3.000  
    ##  Max.   :18.00   Max.   :1.0000   Max.   :162.00   Max.   :93.000  
    ##  average_token_length data_channel_is_lifestyle global_rate_positive_words
    ##  Min.   :0.000        Min.   :0.00000           Min.   :0.00000           
    ##  1st Qu.:4.475        1st Qu.:0.00000           1st Qu.:0.02820           
    ##  Median :4.656        Median :0.00000           Median :0.03817           
    ##  Mean   :4.536        Mean   :0.04719           Mean   :0.03900           
    ##  3rd Qu.:4.840        3rd Qu.:0.00000           3rd Qu.:0.04975           
    ##  Max.   :6.513        Max.   :1.00000           Max.   :0.12139           
    ##  avg_positive_polarity abs_title_subjectivity self_reference_avg_sharess
    ##  Min.   :0.0000        Min.   :0.0000         Min.   :     0            
    ##  1st Qu.:0.3052        1st Qu.:0.1500         1st Qu.:  1000            
    ##  Median :0.3586        Median :0.5000         Median :  2168            
    ##  Mean   :0.3540        Mean   :0.3391         Mean   :  6321            
    ##  3rd Qu.:0.4121        3rd Qu.:0.5000         3rd Qu.:  5200            
    ##  Max.   :1.0000        Max.   :0.5000         Max.   :690400            
    ##    kw_avg_min        kw_avg_max       kw_avg_avg        shares      
    ##  Min.   :   -1.0   Min.   :     0   Min.   :    0   Min.   :     4  
    ##  1st Qu.:  136.2   1st Qu.:173315   1st Qu.: 2355   1st Qu.:   913  
    ##  Median :  230.5   Median :242336   Median : 2832   Median :  1400  
    ##  Mean   :  317.1   Mean   :257156   Mean   : 3074   Mean   :  3641  
    ##  3rd Qu.:  352.6   3rd Qu.:330765   3rd Qu.: 3535   3rd Qu.:  2700  
    ##  Max.   :29946.9   Max.   :798220   Max.   :33536   Max.   :652900  
    ##  weekday_is_monday weekday_is_tuesday weekday_is_wednesday weekday_is_thursday
    ##  Min.   :1         Min.   :0          Min.   :0            Min.   :0          
    ##  1st Qu.:1         1st Qu.:0          1st Qu.:0            1st Qu.:0          
    ##  Median :1         Median :0          Median :0            Median :0          
    ##  Mean   :1         Mean   :0          Mean   :0            Mean   :0          
    ##  3rd Qu.:1         3rd Qu.:0          3rd Qu.:0            3rd Qu.:0          
    ##  Max.   :1         Max.   :0          Max.   :0            Max.   :0          
    ##  weekday_is_friday weekday_is_saturday weekday_is_sunday
    ##  Min.   :0         Min.   :0           Min.   :0        
    ##  1st Qu.:0         1st Qu.:0           1st Qu.:0        
    ##  Median :0         Median :0           Median :0        
    ##  Mean   :0         Mean   :0           Mean   :0        
    ##  3rd Qu.:0         3rd Qu.:0           3rd Qu.:0        
    ##  Max.   :0         Max.   :0           Max.   :0

``` r
g<-ggplot(dayDataTrain,aes(x=shares))
g+geom_histogram(binwidth = 100000)+labs(caption = "Shares Histogram")
```

![](project_yz_files/figure-gfm/unnamed-chunk-4-1.png)<!-- -->
