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

    ##  n_unique_tokens    num_hrefs         num_imgs       average_token_length
    ##  Min.   :0.0000   Min.   :  0.00   Min.   :  0.000   Min.   :0.000       
    ##  1st Qu.:0.4618   1st Qu.:  5.00   1st Qu.:  1.000   1st Qu.:4.486       
    ##  Median :0.5275   Median :  9.00   Median :  1.000   Median :4.683       
    ##  Mean   :0.5268   Mean   : 12.52   Mean   :  5.897   Mean   :4.577       
    ##  3rd Qu.:0.6073   3rd Qu.: 16.00   3rd Qu.:  9.000   3rd Qu.:4.873       
    ##  Max.   :0.9796   Max.   :153.00   Max.   :111.000   Max.   :5.994       
    ##  data_channel_is_lifestyle global_rate_positive_words avg_positive_polarity
    ##  Min.   :0.0000            Min.   :0.00000            Min.   :0.0000       
    ##  1st Qu.:0.0000            1st Qu.:0.02817            1st Qu.:0.3098       
    ##  Median :0.0000            Median :0.03951            Median :0.3667       
    ##  Mean   :0.0799            Mean   :0.04099            Mean   :0.3636       
    ##  3rd Qu.:0.0000            3rd Qu.:0.05283            3rd Qu.:0.4250       
    ##  Max.   :1.0000            Max.   :0.13542            Max.   :1.0000       
    ##  abs_title_subjectivity self_reference_avg_sharess   kw_avg_min     
    ##  Min.   :0.0000         Min.   :     0.0           Min.   :   -1.0  
    ##  1st Qu.:0.1333         1st Qu.:   984.5           1st Qu.:  158.9  
    ##  Median :0.4000         Median :  2150.8           Median :  243.8  
    ##  Mean   :0.3218         Mean   :  6125.8           Mean   :  320.3  
    ##  3rd Qu.:0.5000         3rd Qu.:  5000.0           3rd Qu.:  376.3  
    ##  Max.   :0.5000         Max.   :843300.0           Max.   :27123.0  
    ##    kw_avg_max       kw_avg_avg          shares     
    ##  Min.   :  7328   Min.   :  743.5   Min.   :  171  
    ##  1st Qu.:168580   1st Qu.: 2497.0   1st Qu.: 1300  
    ##  Median :231000   Median : 3040.8   Median : 1900  
    ##  Mean   :242959   Mean   : 3306.3   Mean   : 3876  
    ##  3rd Qu.:307667   3rd Qu.: 3855.7   3rd Qu.: 3700  
    ##  Max.   :762840   Max.   :15336.1   Max.   :82000

``` r
res<-cor(dayDataTrain)
tab<-round(res,2)
kable(tab, caption = "Correlation Table for the Train Data")
```

|                               | n\_unique\_tokens | num\_hrefs | num\_imgs | average\_token\_length | data\_channel\_is\_lifestyle | global\_rate\_positive\_words | avg\_positive\_polarity | abs\_title\_subjectivity | self\_reference\_avg\_sharess | kw\_avg\_min | kw\_avg\_max | kw\_avg\_avg | shares |
| :---------------------------- | ----------------: | ---------: | --------: | ---------------------: | ---------------------------: | ----------------------------: | ----------------------: | -----------------------: | ----------------------------: | -----------: | -----------: | -----------: | -----: |
| n\_unique\_tokens             |              1.00 |     \-0.13 |    \-0.33 |                   0.58 |                       \-0.01 |                          0.33 |                    0.38 |                   \-0.05 |                          0.04 |         0.02 |       \-0.06 |         0.03 | \-0.02 |
| num\_hrefs                    |            \-0.13 |       1.00 |      0.32 |                   0.24 |                         0.09 |                          0.06 |                    0.18 |                     0.03 |                          0.03 |       \-0.02 |         0.01 |         0.13 |   0.09 |
| num\_imgs                     |            \-0.33 |       0.32 |      1.00 |                 \-0.03 |                         0.07 |                        \-0.13 |                    0.09 |                   \-0.02 |                          0.00 |       \-0.01 |       \-0.02 |         0.15 |   0.07 |
| average\_token\_length        |              0.58 |       0.24 |    \-0.03 |                   1.00 |                         0.00 |                          0.29 |                    0.52 |                     0.03 |                          0.03 |         0.00 |       \-0.08 |       \-0.09 | \-0.04 |
| data\_channel\_is\_lifestyle  |            \-0.01 |       0.09 |      0.07 |                   0.00 |                         1.00 |                          0.10 |                    0.14 |                     0.00 |                          0.01 |         0.02 |       \-0.11 |         0.13 |   0.00 |
| global\_rate\_positive\_words |              0.33 |       0.06 |    \-0.13 |                   0.29 |                         0.10 |                          1.00 |                    0.29 |                   \-0.18 |                          0.02 |         0.04 |       \-0.10 |         0.04 | \-0.03 |
| avg\_positive\_polarity       |              0.38 |       0.18 |      0.09 |                   0.52 |                         0.14 |                          0.29 |                    1.00 |                     0.01 |                          0.03 |         0.00 |       \-0.04 |         0.14 |   0.04 |
| abs\_title\_subjectivity      |            \-0.05 |       0.03 |    \-0.02 |                   0.03 |                         0.00 |                        \-0.18 |                    0.01 |                     1.00 |                        \-0.01 |         0.01 |       \-0.02 |       \-0.02 |   0.01 |
| self\_reference\_avg\_sharess |              0.04 |       0.03 |      0.00 |                   0.03 |                         0.01 |                          0.02 |                    0.03 |                   \-0.01 |                          1.00 |         0.00 |         0.04 |         0.08 |   0.05 |
| kw\_avg\_min                  |              0.02 |     \-0.02 |    \-0.01 |                   0.00 |                         0.02 |                          0.04 |                    0.00 |                     0.01 |                          0.00 |         1.00 |       \-0.14 |         0.25 |   0.05 |
| kw\_avg\_max                  |            \-0.06 |       0.01 |    \-0.02 |                 \-0.08 |                       \-0.11 |                        \-0.10 |                  \-0.04 |                   \-0.02 |                          0.04 |       \-0.14 |         1.00 |         0.40 |   0.05 |
| kw\_avg\_avg                  |              0.03 |       0.13 |      0.15 |                 \-0.09 |                         0.13 |                          0.04 |                    0.14 |                   \-0.02 |                          0.08 |         0.25 |         0.40 |         1.00 |   0.14 |
| shares                        |            \-0.02 |       0.09 |      0.07 |                 \-0.04 |                         0.00 |                        \-0.03 |                    0.04 |                     0.01 |                          0.05 |         0.05 |         0.05 |         0.14 |   1.00 |

Correlation Table for the Train Data

``` r
g<-ggplot(dayDataTrain,aes(x=shares))
g+geom_histogram(binwidth = 100000)+labs(x="Shares", y="Count", title = "Shares Histogram")
```

![](weekday_is_sunday_files/figure-gfm/summarization-1.png)<!-- -->

``` r
g2<-ggplot(dayDataTrain, aes(x=global_rate_positive_words, y=shares))
g2+geom_jitter()+labs(x="Global Rate Postitive Words", y="Shares", title="Global Rate Postitive Words vs Shares")
```

![](weekday_is_sunday_files/figure-gfm/summarization-2.png)<!-- -->

``` r
g3<-ggplot(dayDataTrain, aes(x=kw_avg_min, y=shares))
g3+geom_jitter()+labs(x="Worst keyword", y="Shares", title="Worst keyword vs Shares")
```

![](weekday_is_sunday_files/figure-gfm/summarization-3.png)<!-- --> \#
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

![](weekday_is_sunday_files/figure-gfm/models-1.png)<!-- -->

``` r
bt_fit<-train(shares~., data=dayDataTrain, method="gbm",
              preProcess = c("center", "scale"),
              trControl=trainControl(method="cv", number = 10),
              tuneGrid=expand.grid(n.trees=c(1,5,10), interaction.depth=1:3, shrinkage=c(0.1,0.5,0.9), n.minobsinnode=10),
              verbose = FALSE,
              metric="MAE")
plot(bt_fit)
```

![](weekday_is_sunday_files/figure-gfm/models-2.png)<!-- -->

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
| RMSE     |   5319.6606384 | 5290.7559219 |
| Rsquared |      0.0122941 |    0.0225258 |
| MAE      |   2888.2519524 | 2860.6805951 |

Prediction Metric for Two Potential Models

# Automation

First, I got unique weekday from the weekday column of the Data2 data
set using unique() function. And then, I created filenames. and put
filename for each day in a dateframe.
