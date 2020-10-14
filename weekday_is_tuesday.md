Project 2
================
Yuying Zhou
10/12/2020

  - [Package List](#package-list)
  - [Introduction](#introduction)
  - [Data](#data)
  - [Summarizations](#summarizations)
  - [Automation](#automation)

# Package List

``` r
library(tidyverse)
library(caret)
library(knitr)
library(rmarkdown)
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

Additionally, I included the variables weekday\_is\_\* in the dataset
for generating a seperate report for each weekday.

``` r
Data<-read_csv("../OnlineNewsPopularity.csv")
Data<-Data%>%select(n_unique_tokens,num_hrefs, num_imgs, average_token_length, data_channel_is_lifestyle, global_rate_positive_words,avg_positive_polarity, abs_title_subjectivity,self_reference_avg_sharess, kw_avg_min, kw_avg_max, kw_avg_avg, shares, starts_with("weekday") )
Data<-gather(Data, weekday, weekdayvalue,14:20) %>% filter(weekdayvalue==1)
```

# Data

I read data from the folder and then split daily data set into train and
test set. The train set include 70% of the data and the test set include
30% of the data.

``` r
dayData<-Data%>%filter(weekday==params$weekday)%>%select(-c(weekday,weekdayvalue))

set.seed(1)

#try a subset
sub <- sample(1:nrow(dayData), size = nrow(dayData)*0.1)
dayData<-dayData[sub,]

train <- sample(1:nrow(dayData), size = nrow(dayData)*0.7)
test <- dplyr::setdiff(1:nrow(dayData), train)
dayDataTrain <- dayData[train, ]
dayDataTest <- dayData[test, ]
```

# Summarizations

Based on the summary table, no missing values. As shown in the histogram
for shares, most articles had zero shares. According to the correlation
table, mullinearity is not an issue for the data set. There isn’t much
variations as the global rate positive words change.

``` r
summary(dayDataTrain)  
```

    ##  n_unique_tokens    num_hrefs         num_imgs     average_token_length data_channel_is_lifestyle
    ##  Min.   :0.0000   Min.   :  0.00   Min.   : 0.00   Min.   :0.000        Min.   :0.00000          
    ##  1st Qu.:0.4703   1st Qu.:  4.00   1st Qu.: 1.00   1st Qu.:4.448        1st Qu.:0.00000          
    ##  Median :0.5314   Median :  7.00   Median : 1.00   Median :4.613        Median :0.00000          
    ##  Mean   :0.5225   Mean   : 10.48   Mean   : 4.19   Mean   :4.452        Mean   :0.03288          
    ##  3rd Qu.:0.6082   3rd Qu.: 12.00   3rd Qu.: 3.00   3rd Qu.:4.815        3rd Qu.:0.00000          
    ##  Max.   :0.8396   Max.   :103.00   Max.   :92.00   Max.   :5.609        Max.   :1.00000          
    ##  global_rate_positive_words avg_positive_polarity abs_title_subjectivity self_reference_avg_sharess
    ##  Min.   :0.00000            Min.   :0.0000        Min.   :0.0000         Min.   :     0            
    ##  1st Qu.:0.02736            1st Qu.:0.2979        1st Qu.:0.1500         1st Qu.:   881            
    ##  Median :0.03937            Median :0.3555        Median :0.5000         Median :  2400            
    ##  Mean   :0.03931            Mean   :0.3435        Mean   :0.3372         Mean   :  5137            
    ##  3rd Qu.:0.05157            3rd Qu.:0.3983        3rd Qu.:0.5000         3rd Qu.:  5300            
    ##  Max.   :0.10619            Max.   :0.6727        Max.   :0.5000         Max.   :104100            
    ##    kw_avg_min        kw_avg_max       kw_avg_avg        shares     
    ##  Min.   :   -1.0   Min.   : 11050   Min.   : 1109   Min.   :  264  
    ##  1st Qu.:  147.1   1st Qu.:167900   1st Qu.: 2330   1st Qu.:  923  
    ##  Median :  243.5   Median :242850   Median : 2793   Median : 1400  
    ##  Mean   :  334.9   Mean   :256470   Mean   : 3086   Mean   : 2682  
    ##  3rd Qu.:  365.6   3rd Qu.:332613   3rd Qu.: 3532   3rd Qu.: 2400  
    ##  Max.   :15851.2   Max.   :798220   Max.   :17839   Max.   :47300

``` r
res<-cor(dayDataTrain)
tab<-round(res,2)
kable(tab, caption = "Correlation Table for the Train Data")
```

|                               | n\_unique\_tokens | num\_hrefs | num\_imgs | average\_token\_length | data\_channel\_is\_lifestyle | global\_rate\_positive\_words | avg\_positive\_polarity | abs\_title\_subjectivity | self\_reference\_avg\_sharess | kw\_avg\_min | kw\_avg\_max | kw\_avg\_avg | shares |
| :---------------------------- | ----------------: | ---------: | --------: | ---------------------: | ---------------------------: | ----------------------------: | ----------------------: | -----------------------: | ----------------------------: | -----------: | -----------: | -----------: | -----: |
| n\_unique\_tokens             |              1.00 |     \-0.02 |    \-0.18 |                   0.74 |                         0.00 |                          0.42 |                    0.55 |                     0.00 |                          0.11 |         0.04 |       \-0.14 |       \-0.08 |   0.03 |
| num\_hrefs                    |            \-0.02 |       1.00 |      0.31 |                   0.25 |                         0.02 |                          0.01 |                    0.16 |                     0.02 |                          0.09 |       \-0.03 |         0.05 |         0.11 | \-0.02 |
| num\_imgs                     |            \-0.18 |       0.31 |      1.00 |                 \-0.01 |                       \-0.03 |                        \-0.09 |                    0.05 |                   \-0.01 |                          0.03 |       \-0.02 |         0.01 |         0.13 |   0.05 |
| average\_token\_length        |              0.74 |       0.25 |    \-0.01 |                   1.00 |                         0.03 |                          0.40 |                    0.63 |                     0.02 |                          0.10 |         0.03 |       \-0.23 |       \-0.20 | \-0.03 |
| data\_channel\_is\_lifestyle  |              0.00 |       0.02 |    \-0.03 |                   0.03 |                         1.00 |                          0.06 |                    0.08 |                     0.05 |                        \-0.01 |         0.17 |       \-0.16 |         0.04 |   0.01 |
| global\_rate\_positive\_words |              0.42 |       0.01 |    \-0.09 |                   0.40 |                         0.06 |                          1.00 |                    0.43 |                   \-0.17 |                          0.11 |         0.02 |       \-0.20 |       \-0.06 | \-0.06 |
| avg\_positive\_polarity       |              0.55 |       0.16 |      0.05 |                   0.63 |                         0.08 |                          0.43 |                    1.00 |                     0.01 |                          0.11 |       \-0.02 |       \-0.17 |       \-0.10 |   0.04 |
| abs\_title\_subjectivity      |              0.00 |       0.02 |    \-0.01 |                   0.02 |                         0.05 |                        \-0.17 |                    0.01 |                     1.00 |                        \-0.02 |         0.02 |       \-0.07 |       \-0.07 |   0.03 |
| self\_reference\_avg\_sharess |              0.11 |       0.09 |      0.03 |                   0.10 |                       \-0.01 |                          0.11 |                    0.11 |                   \-0.02 |                          1.00 |         0.08 |         0.10 |         0.20 |   0.02 |
| kw\_avg\_min                  |              0.04 |     \-0.03 |    \-0.02 |                   0.03 |                         0.17 |                          0.02 |                  \-0.02 |                     0.02 |                          0.08 |         1.00 |       \-0.14 |         0.53 |   0.00 |
| kw\_avg\_max                  |            \-0.14 |       0.05 |      0.01 |                 \-0.23 |                       \-0.16 |                        \-0.20 |                  \-0.17 |                   \-0.07 |                          0.10 |       \-0.14 |         1.00 |         0.42 |   0.04 |
| kw\_avg\_avg                  |            \-0.08 |       0.11 |      0.13 |                 \-0.20 |                         0.04 |                        \-0.06 |                  \-0.10 |                   \-0.07 |                          0.20 |         0.53 |         0.42 |         1.00 |   0.11 |
| shares                        |              0.03 |     \-0.02 |      0.05 |                 \-0.03 |                         0.01 |                        \-0.06 |                    0.04 |                     0.03 |                          0.02 |         0.00 |         0.04 |         0.11 |   1.00 |

Correlation Table for the Train Data

``` r
g<-ggplot(dayDataTrain,aes(x=shares))
g+geom_histogram(binwidth = 100000)+labs(x="Shares", y="Count", title = "Shares Histogram")
```

![](weekday_is_tuesday_files/figure-gfm/summarization-1.png)<!-- -->

``` r
g2<-ggplot(dayDataTrain, aes(x=global_rate_positive_words, y=shares))
g2+geom_jitter()+labs(x="Global Rate Postitive Words", y="Shares", title="Global Rate Postitive Words vs Shares")
```

![](weekday_is_tuesday_files/figure-gfm/summarization-2.png)<!-- --> \#
Models  
This step will create two models. One is a regression tree model and the
second one is a boosted tree model. Both models include all predictors
included in the data set. The tuning paramter “cp” for the regression
tree model was chosen using the leave one out cross validation. I picked
the best tree model using the smallest MAE. The boosted tree model used
cross-validation with 10 folds. The final chose model for the boosted
tree model was determined using the smallest MAE.

Once the optimal model was picked, I used the test data set to see which
model was better at predicting shares. The model with the smallest MAE
values was considered better at predicting.

The table below shows the RMSE, R-squared, MAE for these two models’
predictions using the test set.

``` r
tree_fit<-train(shares~., data=dayDataTrain, method="rpart",
                trControl=trainControl(method = "LOOCV"),
                preProcess = c("center", "scale"),
                tuneLength=10,
                metric="MAE")
plot(tree_fit)
```

![](weekday_is_tuesday_files/figure-gfm/models-1.png)<!-- -->

``` r
bt_fit<-train(shares~., data=dayDataTrain, method="gbm",
              preProcess = c("center", "scale"),
              trControl=trainControl(method="cv", number = 10),
              tuneGrid=expand.grid(n.trees=c(1,5,10), interaction.depth=1:3, shrinkage=c(0.1,0.5,0.9), n.minobsinnode=10),
              verbose = FALSE,
              metric="MAE")
plot(bt_fit)
```

![](weekday_is_tuesday_files/figure-gfm/models-2.png)<!-- -->

``` r
pred_tree<-predict(tree_fit, newdata = dayDataTest)   
pred_bt<-predict(bt_fit, newdata = dayDataTest)
pred_tree_metric<-postResample(pred_tree,obs = dayDataTest$shares)
pred_bt_metric<-postResample(pred_bt,obs = dayDataTest$shares)
Metric_Table<-data.frame(pred_tree_metric, pred_bt_metric)
kable(Metric_Table, caption = "Prediction Metric for Two Potential Models", col.names = c("Regressio Tree"," Boosted Tree"))
```

|          | Regressio Tree | Boosted Tree |
| :------- | -------------: | -----------: |
| RMSE     |   9082.5795562 | 9010.8038263 |
| Rsquared |      0.0008116 |    0.0000459 |
| MAE      |   2761.6335744 | 2781.9677405 |

Prediction Metric for Two Potential Models

# Automation

First, I got unique weekday from the weekday column of the Data2 data
set using unique() function. And then, I created filenames. and put
filename for each day in a dateframe.
