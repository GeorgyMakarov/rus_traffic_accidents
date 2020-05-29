---
title: "Predicting traffic accidents in Russia"
author: "Georgy Makarov"
date: 'May 28, 2020'
output: 
    html_document:
            keep_md: true
---



## Introduction

Traffic accidents in Russia caused 16981 deaths and 210877 injuries in 2019 
according to official statistics. Russian government had developed a strategy
to reduce the number of traffic accidents in 2016. The strategy targets 1.5 - 2
deaths per 100000 population by the end of 2024. While much effort has focused
on predicting whether Russia is able to reach the assigned level, what 
influences the severity of traffic accident remains unknown.

There are a variety of weather and traffic conditions that contribute to 
accidents. This paper attempts to find out if we could use quantitative 
characteristics to predict the severity of traffic accident in Russia?

This research is based on the data for Saint-Petersburg, Russia.


## Desired data

Ideal data set has to include both weather and traffic conditions. Weather
conditions include variables: air temperature, road temperature, rain, snow, 
ice, fog. Traffic conditions include variables: date, time, logitude, latitude,
type of accident, road type, place type, road condition, road lights, number of
vehicles, number of participants, fatal, injury, class of vehicles, model of 
vehicles, category of participants, sex of participants, cause of accident.

## Available data

We downloand the data from official [police](http://stat.gibdd.ru). It 
contains the following columns: type of accident, date, city district, info
about an accident, number of vehicles, number of participants, ID of an 
accident, fatalities, injured, time of an accident. The column with the info
about an accident is a string without separators, which might contain other
desired parameters like temperature, rain, snow etc.

## Prerequisite packages


```r
library(reshape2)
library(ggplot2)
library(dplyr)
library(tidyr)
library(lubridate)
library(XML)
```

## Get the data

Downloand from the official police source is available in *.xml* format. We 
downlaod the data for 2019. The data was broken by 14 days intervals by police
server. Total 2019 is 22 files - the data for December is not available yet. 


```r
files <- list.files("./raw_data")
files <- as.list(files)
length(files)
```

```
## [1] 22
```

We download them with the loop:


```r
setwd("./raw_data")
list_file <- list.files(pattern = "*.xml") %>% 
    lapply(xmlToDataFrame, stringsAsFactors = F) %>% bind_rows
setwd("C:/Users/Георгий/Documents/GitHub/rus_traffic_accidents/")
```

## Clean the data

The data file contains general information: region of observations, official
government data, government classifications. Those are not desired in the 
dataset. We filter the rows, which contain *NA* on *DTPV* column. Column *text* 
does not contain any information at all and returns *NA*. We delete this column
from the dataset. Then we arrange the data by the date and time of accident.


```r
list_file <- list_file %>% 
    filter(!(is.na(DTPV))) %>% select(-text)
list_file$date <- dmy(list_file$date)
list_file$time <- as.POSIXct(list_file$time, format = "%H:%M")
xmldf <- list_file %>% arrange(date, time)
rm(list_file)
dim(xmldf)
```

```
## [1] 5860   11
```

The columns KTS, KUCH, POG, RAN are characters. They need to be
transformed to numbers.


```r
summary(xmldf)
```

```
##      DTPV                date              district           infoDtp         
##  Length:5860        Min.   :2019-01-01   Length:5860        Length:5860       
##  Class :character   1st Qu.:2019-04-11   Class :character   Class :character  
##  Mode  :character   Median :2019-06-28   Mode  :character   Mode  :character  
##                     Mean   :2019-06-24                                        
##                     3rd Qu.:2019-09-11                                        
##                     Max.   :2019-11-26                                        
##      KTS                KUCH              kartId              POG           
##  Length:5860        Length:5860        Length:5860        Length:5860       
##  Class :character   Class :character   Class :character   Class :character  
##  Mode  :character   Mode  :character   Mode  :character   Mode  :character  
##                                                                             
##                                                                             
##                                                                             
##      RAN               rowNum               time                    
##  Length:5860        Length:5860        Min.   :2020-05-28 00:00:00  
##  Class :character   Class :character   1st Qu.:2020-05-28 10:20:00  
##  Mode  :character   Mode  :character   Median :2020-05-28 15:00:00  
##                                        Mean   :2020-05-28 14:22:47  
##                                        3rd Qu.:2020-05-28 19:00:00  
##                                        Max.   :2020-05-28 23:59:00
```


```r
xmldf$KTS <- as.numeric(xmldf$KTS)
xmldf$KUCH <- as.numeric(xmldf$KUCH)
xmldf$POG <- as.numeric(xmldf$POG)
xmldf$RAN <- as.numeric(xmldf$RAN)
```


```r
summary(xmldf)
```

```
##      DTPV                date              district           infoDtp         
##  Length:5860        Min.   :2019-01-01   Length:5860        Length:5860       
##  Class :character   1st Qu.:2019-04-11   Class :character   Class :character  
##  Mode  :character   Median :2019-06-28   Mode  :character   Mode  :character  
##                     Mean   :2019-06-24                                        
##                     3rd Qu.:2019-09-11                                        
##                     Max.   :2019-11-26                                        
##       KTS             KUCH           kartId               POG         
##  Min.   :1.000   Min.   : 1.000   Length:5860        Min.   :0.00000  
##  1st Qu.:1.000   1st Qu.: 2.000   Class :character   1st Qu.:0.00000  
##  Median :2.000   Median : 2.000   Mode  :character   Median :0.00000  
##  Mean   :1.686   Mean   : 2.442                      Mean   :0.03396  
##  3rd Qu.:2.000   3rd Qu.: 3.000                      3rd Qu.:0.00000  
##  Max.   :9.000   Max.   :17.000                      Max.   :2.00000  
##       RAN            rowNum               time                    
##  Min.   : 0.000   Length:5860        Min.   :2020-05-28 00:00:00  
##  1st Qu.: 1.000   Class :character   1st Qu.:2020-05-28 10:20:00  
##  Median : 1.000   Mode  :character   Median :2020-05-28 15:00:00  
##  Mean   : 1.193                      Mean   :2020-05-28 14:22:47  
##  3rd Qu.: 1.000                      3rd Qu.:2020-05-28 19:00:00  
##  Max.   :15.000                      Max.   :2020-05-28 23:59:00
```


