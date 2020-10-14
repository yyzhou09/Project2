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

    ##  n_unique_tokens    num_hrefs        num_imgs     average_token_length data_channel_is_lifestyle
    ##  Min.   :0.0000   Min.   : 0.00   Min.   : 0.00   Min.   :0.000        Min.   :0.00000          
    ##  1st Qu.:0.4558   1st Qu.: 5.00   1st Qu.: 1.00   1st Qu.:4.496        1st Qu.:0.00000          
    ##  Median :0.5136   Median : 9.00   Median : 1.00   Median :4.614        Median :0.00000          
    ##  Mean   :0.5102   Mean   :12.12   Mean   : 4.83   Mean   :4.536        Mean   :0.04678          
    ##  3rd Qu.:0.5886   3rd Qu.:16.00   3rd Qu.: 7.00   3rd Qu.:4.848        3rd Qu.:0.00000          
    ##  Max.   :0.7778   Max.   :60.00   Max.   :46.00   Max.   :5.625        Max.   :1.00000          
    ##  global_rate_positive_words avg_positive_polarity abs_title_subjectivity self_reference_avg_sharess
    ##  Min.   :0.00000            Min.   :0.0000        Min.   :0.0000         Min.   :     0            
    ##  1st Qu.:0.02935            1st Qu.:0.3271        1st Qu.:0.1667         1st Qu.:  1105            
    ##  Median :0.04006            Median :0.3760        Median :0.5000         Median :  2700            
    ##  Mean   :0.04084            Mean   :0.3695        Mean   :0.3346         Mean   :  8515            
    ##  3rd Qu.:0.05322            3rd Qu.:0.4274        3rd Qu.:0.5000         3rd Qu.:  7500            
    ##  Max.   :0.09266            Max.   :0.6583        Max.   :0.5000         Max.   :141705            
    ##    kw_avg_min       kw_avg_max       kw_avg_avg        shares     
    ##  Min.   :  -1.0   Min.   : 17200   Min.   : 1360   Min.   :  258  
    ##  1st Qu.: 137.5   1st Qu.:174853   1st Qu.: 2536   1st Qu.: 1400  
    ##  Median : 227.6   Median :224870   Median : 3028   Median : 2100  
    ##  Mean   : 265.1   Mean   :253848   Mean   : 3457   Mean   : 4642  
    ##  3rd Qu.: 337.2   3rd Qu.:313997   3rd Qu.: 3782   3rd Qu.: 4200  
    ##  Max.   :1810.0   Max.   :741367   Max.   :36717   Max.   :96100

``` r
res<-cor(dayDataTrain)
tab<-round(res,2)
kable(tab, caption = "Correlation Table for the Train Data")
```

|                               | n\_unique\_tokens | num\_hrefs | num\_imgs | average\_token\_length | data\_channel\_is\_lifestyle | global\_rate\_positive\_words | avg\_positive\_polarity | abs\_title\_subjectivity | self\_reference\_avg\_sharess | kw\_avg\_min | kw\_avg\_max | kw\_avg\_avg | shares |
| :---------------------------- | ----------------: | ---------: | --------: | ---------------------: | ---------------------------: | ----------------------------: | ----------------------: | -----------------------: | ----------------------------: | -----------: | -----------: | -----------: | -----: |
| n\_unique\_tokens             |              1.00 |     \-0.05 |    \-0.33 |                   0.67 |                         0.02 |                          0.22 |                    0.44 |                     0.00 |                          0.14 |       \-0.05 |       \-0.02 |       \-0.02 | \-0.03 |
| num\_hrefs                    |            \-0.05 |       1.00 |      0.25 |                   0.28 |                         0.07 |                          0.12 |                    0.25 |                   \-0.06 |                          0.18 |         0.12 |       \-0.14 |         0.19 | \-0.06 |
| num\_imgs                     |            \-0.33 |       0.25 |      1.00 |                 \-0.07 |                         0.07 |                        \-0.04 |                    0.02 |                   \-0.17 |                        \-0.03 |       \-0.04 |         0.01 |         0.14 |   0.03 |
| average\_token\_length        |              0.67 |       0.28 |    \-0.07 |                   1.00 |                         0.00 |                          0.28 |                    0.54 |                     0.02 |                          0.09 |         0.01 |       \-0.17 |       \-0.05 | \-0.13 |
| data\_channel\_is\_lifestyle  |              0.02 |       0.07 |      0.07 |                   0.00 |                         1.00 |                          0.07 |                    0.07 |                   \-0.01 |                        \-0.03 |       \-0.05 |       \-0.05 |         0.04 |   0.11 |
| global\_rate\_positive\_words |              0.22 |       0.12 |    \-0.04 |                   0.28 |                         0.07 |                          1.00 |                    0.42 |                   \-0.16 |                        \-0.04 |         0.14 |       \-0.11 |       \-0.01 |   0.03 |
| avg\_positive\_polarity       |              0.44 |       0.25 |      0.02 |                   0.54 |                         0.07 |                          0.42 |                    1.00 |                     0.01 |                          0.03 |         0.10 |       \-0.04 |         0.05 | \-0.06 |
| abs\_title\_subjectivity      |              0.00 |     \-0.06 |    \-0.17 |                   0.02 |                       \-0.01 |                        \-0.16 |                    0.01 |                     1.00 |                          0.03 |         0.04 |       \-0.05 |       \-0.14 |   0.03 |
| self\_reference\_avg\_sharess |              0.14 |       0.18 |    \-0.03 |                   0.09 |                       \-0.03 |                        \-0.04 |                    0.03 |                     0.03 |                          1.00 |         0.31 |         0.13 |         0.59 |   0.09 |
| kw\_avg\_min                  |            \-0.05 |       0.12 |    \-0.04 |                   0.01 |                       \-0.05 |                          0.14 |                    0.10 |                     0.04 |                          0.31 |         1.00 |       \-0.31 |         0.55 |   0.02 |
| kw\_avg\_max                  |            \-0.02 |     \-0.14 |      0.01 |                 \-0.17 |                       \-0.05 |                        \-0.11 |                  \-0.04 |                   \-0.05 |                          0.13 |       \-0.31 |         1.00 |         0.14 | \-0.03 |
| kw\_avg\_avg                  |            \-0.02 |       0.19 |      0.14 |                 \-0.05 |                         0.04 |                        \-0.01 |                    0.05 |                   \-0.14 |                          0.59 |         0.55 |         0.14 |         1.00 |   0.08 |
| shares                        |            \-0.03 |     \-0.06 |      0.03 |                 \-0.13 |                         0.11 |                          0.03 |                  \-0.06 |                     0.03 |                          0.09 |         0.02 |       \-0.03 |         0.08 |   1.00 |

Correlation Table for the Train Data

``` r
g<-ggplot(dayDataTrain,aes(x=shares))
g+geom_histogram(binwidth = 100000)+labs(x="Shares", y="Count", title = "Shares Histogram")
```

![](weekday_is_saturday_files/figure-gfm/summarization-1.png)<!-- -->

``` r
g2<-ggplot(dayDataTrain, aes(x=global_rate_positive_words, y=shares))
g2+geom_jitter()+labs(x="Global Rate Postitive Words", y="Shares", title="Global Rate Postitive Words vs Shares")
```

![](weekday_is_saturday_files/figure-gfm/summarization-2.png)<!-- --> \#
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

![](weekday_is_saturday_files/figure-gfm/models-1.png)<!-- -->

``` r
bt_fit<-train(shares~., data=dayDataTrain, method="gbm",
              preProcess = c("center", "scale"),
              trControl=trainControl(method="cv", number = 10),
              tuneGrid=expand.grid(n.trees=c(1,5,10), interaction.depth=1:3, shrinkage=c(0.1,0.5,0.9), n.minobsinnode=10),
              verbose = FALSE,
              metric="MAE")
plot(bt_fit)
```

![](weekday_is_saturday_files/figure-gfm/models-2.png)<!-- -->

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
| RMSE     |       6183.509 | 5961.8134041 |
| Rsquared |             NA |    0.0682651 |
| MAE      |       3475.377 | 3223.7926638 |

Prediction Metric for Two Potential Models

# Automation

First, I got unique weekday from the weekday column of the Data2 data
set using unique() function. And then, I created filenames. and put
filename for each day in a dateframe.
