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
weekday\_is\_thursday

``` r
dayData<-Data%>%filter(weekday==params$weekday)%>%select(-c(weekday,weekdayvalue))
print(params$weekday)
```

    ## [1] "weekday_is_thursday"

``` r
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
    ##  1st Qu.:0.4720   1st Qu.:  4.00   1st Qu.:  1.000   1st Qu.:4.483       
    ##  Median :0.5408   Median :  7.00   Median :  1.000   Median :4.674       
    ##  Mean   :0.5314   Mean   : 10.56   Mean   :  4.413   Mean   :4.550       
    ##  3rd Qu.:0.6093   3rd Qu.: 13.00   3rd Qu.:  3.000   3rd Qu.:4.866       
    ##  Max.   :0.9545   Max.   :140.00   Max.   :100.000   Max.   :6.198       
    ##  data_channel_is_lifestyle global_rate_positive_words avg_positive_polarity
    ##  Min.   :0.00000           Min.   :0.00000            Min.   :0.0000       
    ##  1st Qu.:0.00000           1st Qu.:0.02862            1st Qu.:0.3048       
    ##  Median :0.00000           Median :0.03910            Median :0.3580       
    ##  Mean   :0.05112           Mean   :0.03953            Mean   :0.3519       
    ##  3rd Qu.:0.00000           3rd Qu.:0.05034            3rd Qu.:0.4119       
    ##  Max.   :1.00000           Max.   :0.15278            Max.   :0.8500       
    ##  abs_title_subjectivity self_reference_avg_sharess   kw_avg_min     
    ##  Min.   :0.0000         Min.   :     0             Min.   :   -1.0  
    ##  1st Qu.:0.1667         1st Qu.:   926             1st Qu.:  143.8  
    ##  Median :0.5000         Median :  2185             Median :  237.2  
    ##  Mean   :0.3429         Mean   :  6162             Mean   :  317.4  
    ##  3rd Qu.:0.5000         3rd Qu.:  5040             3rd Qu.:  355.9  
    ##  Max.   :0.5000         Max.   :690400             Max.   :21516.0  
    ##    kw_avg_max       kw_avg_avg        shares      
    ##  Min.   :  3120   Min.   :  489   Min.   :     8  
    ##  1st Qu.:173769   1st Qu.: 2381   1st Qu.:   901  
    ##  Median :246802   Median : 2865   Median :  1400  
    ##  Mean   :262204   Mean   : 3126   Mean   :  3188  
    ##  3rd Qu.:336051   3rd Qu.: 3569   3rd Qu.:  2600  
    ##  Max.   :843300   Max.   :24260   Max.   :298400

``` r
res<-cor(dayDataTrain)
tab<-round(res,2)
kable(tab, caption = "Correlation Table for the Train Data")
```

|                               | n\_unique\_tokens | num\_hrefs | num\_imgs | average\_token\_length | data\_channel\_is\_lifestyle | global\_rate\_positive\_words | avg\_positive\_polarity | abs\_title\_subjectivity | self\_reference\_avg\_sharess | kw\_avg\_min | kw\_avg\_max | kw\_avg\_avg | shares |
| :---------------------------- | ----------------: | ---------: | --------: | ---------------------: | ---------------------------: | ----------------------------: | ----------------------: | -----------------------: | ----------------------------: | -----------: | -----------: | -----------: | -----: |
| n\_unique\_tokens             |              1.00 |     \-0.12 |    \-0.25 |                   0.67 |                         0.00 |                          0.28 |                    0.43 |                   \-0.03 |                          0.04 |         0.01 |       \-0.05 |       \-0.05 |   0.00 |
| num\_hrefs                    |            \-0.12 |       1.00 |      0.35 |                   0.21 |                         0.02 |                          0.08 |                    0.19 |                     0.01 |                          0.02 |         0.02 |       \-0.01 |         0.12 |   0.06 |
| num\_imgs                     |            \-0.25 |       0.35 |      1.00 |                   0.00 |                       \-0.01 |                        \-0.05 |                    0.08 |                     0.02 |                          0.02 |       \-0.01 |         0.03 |         0.16 |   0.05 |
| average\_token\_length        |              0.67 |       0.21 |      0.00 |                   1.00 |                         0.01 |                          0.34 |                    0.54 |                     0.00 |                          0.04 |         0.00 |       \-0.16 |       \-0.17 | \-0.02 |
| data\_channel\_is\_lifestyle  |              0.00 |       0.02 |    \-0.01 |                   0.01 |                         1.00 |                          0.04 |                    0.05 |                     0.00 |                        \-0.01 |         0.05 |       \-0.13 |         0.06 |   0.01 |
| global\_rate\_positive\_words |              0.28 |       0.08 |    \-0.05 |                   0.34 |                         0.04 |                          1.00 |                    0.35 |                   \-0.16 |                        \-0.01 |         0.03 |       \-0.09 |       \-0.03 |   0.00 |
| avg\_positive\_polarity       |              0.43 |       0.19 |      0.08 |                   0.54 |                         0.05 |                          0.35 |                    1.00 |                     0.00 |                          0.03 |         0.01 |       \-0.07 |         0.03 |   0.00 |
| abs\_title\_subjectivity      |            \-0.03 |       0.01 |      0.02 |                   0.00 |                         0.00 |                        \-0.16 |                    0.00 |                     1.00 |                        \-0.02 |         0.01 |       \-0.04 |       \-0.02 | \-0.02 |
| self\_reference\_avg\_sharess |              0.04 |       0.02 |      0.02 |                   0.04 |                       \-0.01 |                        \-0.01 |                    0.03 |                   \-0.02 |                          1.00 |         0.01 |         0.08 |         0.10 |   0.04 |
| kw\_avg\_min                  |              0.01 |       0.02 |    \-0.01 |                   0.00 |                         0.05 |                          0.03 |                    0.01 |                     0.01 |                          0.01 |         1.00 |       \-0.16 |         0.35 |   0.16 |
| kw\_avg\_max                  |            \-0.05 |     \-0.01 |      0.03 |                 \-0.16 |                       \-0.13 |                        \-0.09 |                  \-0.07 |                   \-0.04 |                          0.08 |       \-0.16 |         1.00 |         0.44 |   0.05 |
| kw\_avg\_avg                  |            \-0.05 |       0.12 |      0.16 |                 \-0.17 |                         0.06 |                        \-0.03 |                    0.03 |                   \-0.02 |                          0.10 |         0.35 |         0.44 |         1.00 |   0.15 |
| shares                        |              0.00 |       0.06 |      0.05 |                 \-0.02 |                         0.01 |                          0.00 |                    0.00 |                   \-0.02 |                          0.04 |         0.16 |         0.05 |         0.15 |   1.00 |

Correlation Table for the Train Data

``` r
g<-ggplot(dayDataTrain,aes(x=shares))
g+geom_histogram(binwidth = 100000)+labs(x="Shares", y="Count", title = "Shares Histogram")
```

![](weekday_is_thursday_files/figure-gfm/summarization-1.png)<!-- -->

``` r
g2<-ggplot(dayDataTrain, aes(x=global_rate_positive_words, y=shares))
g2+geom_jitter()+labs(x="Global Rate Postitive Words", y="Shares", title="Global Rate Postitive Words vs Shares")
```

![](weekday_is_thursday_files/figure-gfm/summarization-2.png)<!-- -->

``` r
g3<-ggplot(dayDataTrain, aes(x=kw_avg_min, y=shares))
g3+geom_jitter()+labs(x="Worst keyword", y="Shares", title="Worst keyword vs Shares")
```

![](weekday_is_thursday_files/figure-gfm/summarization-3.png)<!-- --> \#
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

![](weekday_is_thursday_files/figure-gfm/models-1.png)<!-- -->

``` r
bt_fit<-train(shares~., data=dayDataTrain, method="gbm",
              preProcess = c("center", "scale"),
              trControl=trainControl(method="cv", number = 10),
              tuneGrid=expand.grid(n.trees=c(1,5,10), interaction.depth=1:3, shrinkage=c(0.1,0.5,0.9), n.minobsinnode=10),
              verbose = FALSE,
              metric="MAE")
plot(bt_fit)
```

![](weekday_is_thursday_files/figure-gfm/models-2.png)<!-- -->

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
| RMSE     |   9830.9766642 | 9443.7541808 |
| Rsquared |      0.0108709 |    0.0218885 |
| MAE      |   2879.0050416 | 2811.0671496 |

Prediction Metric for Two Potential Models

# Automation

First, I got unique weekday from the weekday column of the Data data set
using unique() function. And then, I created filenames. and put filename
for each day in a dateframe.

``` r
DayofWeek<-unique(Data$weekday)
render_one<-function(weekday){
  rmarkdown::render(
    "Project2.Rmd",output_file = paste0(weekday,".md"), params = list(weekday=weekday)
  )
}

for (weekday in DayofWeek){
  render_one(weekday)
}
```
