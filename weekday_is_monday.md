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
30% of the data. The date was filter to only include values for

``` r
dayData<-Data%>%filter(weekday==params$weekday)%>%select(-c(weekday,weekdayvalue))

set.seed(1)

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

    ##  n_unique_tokens    num_hrefs         num_imgs      average_token_length
    ##  Min.   :0.0000   Min.   :  0.00   Min.   : 0.000   Min.   :0.000       
    ##  1st Qu.:0.4738   1st Qu.:  4.00   1st Qu.: 1.000   1st Qu.:4.475       
    ##  Median :0.5427   Median :  7.00   Median : 1.000   Median :4.656       
    ##  Mean   :0.5308   Mean   : 10.62   Mean   : 4.382   Mean   :4.536       
    ##  3rd Qu.:0.6088   3rd Qu.: 13.00   3rd Qu.: 3.000   3rd Qu.:4.840       
    ##  Max.   :1.0000   Max.   :162.00   Max.   :93.000   Max.   :6.513       
    ##  data_channel_is_lifestyle global_rate_positive_words avg_positive_polarity
    ##  Min.   :0.00000           Min.   :0.00000            Min.   :0.0000       
    ##  1st Qu.:0.00000           1st Qu.:0.02820            1st Qu.:0.3052       
    ##  Median :0.00000           Median :0.03817            Median :0.3586       
    ##  Mean   :0.04719           Mean   :0.03900            Mean   :0.3540       
    ##  3rd Qu.:0.00000           3rd Qu.:0.04975            3rd Qu.:0.4121       
    ##  Max.   :1.00000           Max.   :0.12139            Max.   :1.0000       
    ##  abs_title_subjectivity self_reference_avg_sharess   kw_avg_min     
    ##  Min.   :0.0000         Min.   :     0             Min.   :   -1.0  
    ##  1st Qu.:0.1500         1st Qu.:  1000             1st Qu.:  136.2  
    ##  Median :0.5000         Median :  2168             Median :  230.5  
    ##  Mean   :0.3391         Mean   :  6321             Mean   :  317.1  
    ##  3rd Qu.:0.5000         3rd Qu.:  5200             3rd Qu.:  352.6  
    ##  Max.   :0.5000         Max.   :690400             Max.   :29946.9  
    ##    kw_avg_max       kw_avg_avg        shares      
    ##  Min.   :     0   Min.   :    0   Min.   :     4  
    ##  1st Qu.:173315   1st Qu.: 2355   1st Qu.:   913  
    ##  Median :242336   Median : 2832   Median :  1400  
    ##  Mean   :257156   Mean   : 3074   Mean   :  3641  
    ##  3rd Qu.:330765   3rd Qu.: 3535   3rd Qu.:  2700  
    ##  Max.   :798220   Max.   :33536   Max.   :652900

``` r
res<-cor(dayDataTrain)
tab<-round(res,2)
kable(tab, caption = "Correlation Table for the Train Data")
```

|                               | n\_unique\_tokens | num\_hrefs | num\_imgs | average\_token\_length | data\_channel\_is\_lifestyle | global\_rate\_positive\_words | avg\_positive\_polarity | abs\_title\_subjectivity | self\_reference\_avg\_sharess | kw\_avg\_min | kw\_avg\_max | kw\_avg\_avg | shares |
| :---------------------------- | ----------------: | ---------: | --------: | ---------------------: | ---------------------------: | ----------------------------: | ----------------------: | -----------------------: | ----------------------------: | -----------: | -----------: | -----------: | -----: |
| n\_unique\_tokens             |              1.00 |     \-0.11 |    \-0.23 |                   0.67 |                         0.00 |                          0.30 |                    0.42 |                     0.01 |                          0.05 |         0.01 |       \-0.04 |       \-0.04 | \-0.01 |
| num\_hrefs                    |            \-0.11 |       1.00 |      0.33 |                   0.22 |                         0.04 |                          0.07 |                    0.18 |                     0.00 |                          0.02 |         0.02 |       \-0.03 |         0.12 |   0.03 |
| num\_imgs                     |            \-0.23 |       0.33 |      1.00 |                   0.05 |                         0.00 |                        \-0.04 |                    0.08 |                   \-0.01 |                          0.02 |         0.00 |       \-0.03 |         0.13 |   0.02 |
| average\_token\_length        |              0.67 |       0.22 |      0.05 |                   1.00 |                         0.01 |                          0.34 |                    0.54 |                     0.03 |                          0.05 |         0.02 |       \-0.17 |       \-0.14 | \-0.04 |
| data\_channel\_is\_lifestyle  |              0.00 |       0.04 |      0.00 |                   0.01 |                         1.00 |                          0.07 |                    0.05 |                     0.02 |                          0.01 |         0.04 |       \-0.11 |         0.04 |   0.00 |
| global\_rate\_positive\_words |              0.30 |       0.07 |    \-0.04 |                   0.34 |                         0.07 |                          1.00 |                    0.31 |                   \-0.12 |                          0.01 |         0.03 |       \-0.12 |       \-0.04 | \-0.01 |
| avg\_positive\_polarity       |              0.42 |       0.18 |      0.08 |                   0.54 |                         0.05 |                          0.31 |                    1.00 |                     0.01 |                          0.05 |         0.02 |       \-0.07 |         0.04 | \-0.01 |
| abs\_title\_subjectivity      |              0.01 |       0.00 |    \-0.01 |                   0.03 |                         0.02 |                        \-0.12 |                    0.01 |                     1.00 |                          0.01 |       \-0.02 |         0.02 |       \-0.02 |   0.02 |
| self\_reference\_avg\_sharess |              0.05 |       0.02 |      0.02 |                   0.05 |                         0.01 |                          0.01 |                    0.05 |                     0.01 |                          1.00 |         0.07 |         0.10 |         0.25 |   0.16 |
| kw\_avg\_min                  |              0.01 |       0.02 |      0.00 |                   0.02 |                         0.04 |                          0.03 |                    0.02 |                   \-0.02 |                          0.07 |         1.00 |       \-0.10 |         0.46 |   0.02 |
| kw\_avg\_max                  |            \-0.04 |     \-0.03 |    \-0.03 |                 \-0.17 |                       \-0.11 |                        \-0.12 |                  \-0.07 |                     0.02 |                          0.10 |       \-0.10 |         1.00 |         0.44 |   0.07 |
| kw\_avg\_avg                  |            \-0.04 |       0.12 |      0.13 |                 \-0.14 |                         0.04 |                        \-0.04 |                    0.04 |                   \-0.02 |                          0.25 |         0.46 |         0.44 |         1.00 |   0.12 |
| shares                        |            \-0.01 |       0.03 |      0.02 |                 \-0.04 |                         0.00 |                        \-0.01 |                  \-0.01 |                     0.02 |                          0.16 |         0.02 |         0.07 |         0.12 |   1.00 |

Correlation Table for the Train Data

``` r
g<-ggplot(dayDataTrain,aes(x=shares))
g+geom_histogram(binwidth = 100000)+labs(x="Shares", y="Count", title = "Shares Histogram")
```

![](weekday_is_monday_files/figure-gfm/summarization-1.png)<!-- -->

``` r
g2<-ggplot(dayDataTrain, aes(x=global_rate_positive_words, y=shares))
g2+geom_jitter()+labs(x="Global Rate Postitive Words", y="Shares", title="Global Rate Postitive Words vs Shares")
```

![](weekday_is_monday_files/figure-gfm/summarization-2.png)<!-- -->

``` r
g3<-ggplot(dayDataTrain, aes(x=kw_avg_min, y=shares))
g3+geom_jitter()+labs(x="Worst keyword", y="Shares", title="Worst keyword vs Shares")
```

![](weekday_is_monday_files/figure-gfm/summarization-3.png)<!-- --> \#
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

![](weekday_is_monday_files/figure-gfm/models-1.png)<!-- -->

``` r
bt_fit<-train(shares~., data=dayDataTrain, method="gbm",
              preProcess = c("center", "scale"),
              trControl=trainControl(method="cv", number = 10),
              tuneGrid=expand.grid(n.trees=c(1,5,10), interaction.depth=1:3, shrinkage=c(0.1,0.5,0.9), n.minobsinnode=10),
              verbose = FALSE,
              metric="MAE")
plot(bt_fit)
```

![](weekday_is_monday_files/figure-gfm/models-2.png)<!-- -->

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
| RMSE     |   17737.134719 | 1.751812e+04 |
| Rsquared |       0.002703 | 6.013200e-03 |
| MAE      |    3618.427937 | 3.567622e+03 |

Prediction Metric for Two Potential Models

# Automation

First, I got unique weekday from the weekday column of the Data2 data
set using unique() function. And then, I created filenames. and put
filename for each day in a dateframe.
