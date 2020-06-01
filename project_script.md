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
ice, fog. Traffic conditions include variables: date, time, longitude, latitude,
type of accident, road type, place type, road condition, road lights, number of
vehicles, number of participants, fatal, injury, class of vehicles, model of 
vehicles, category of participants, sex of participants, cause of accident.

## Available data

We downloand the data from official [police site](http://stat.gibdd.ru). Police 
describes each accident in unique accident card. It contains the following data: 
type of accident, date, city district, info about an accident, number of 
vehicles, number of participants, ID of an accident, fatalities, injured, 
time of an accident etc. The data is available in *.pdf*, *.xls*, *.csv*, *.xml*
formats. We use *.csv* and *.xml* files, because we cannot read the data from
*.pdf* files and *.xls* files representation is not suitable for automated 
reading. Each card is given as separate spreadsheet in one file and different
number of rows and columns.

## Prerequisite packages


```r
library(reshape2)
library(ggplot2)
library(dplyr)
library(tidyr)
library(lubridate)
library(XML)
library(readr)
library(stringi)
library(stringr)
```

## Get the data

We start from reading in *.xml* format. We downlaod the data for 2019. The data 
was broken by 14 days intervals by police server. Total 2019 is 22 files. It 
covers the period from January 1, 2019 to November 26, 2019. The data for 
December is not available yet.


```r
files <- list.files("./raw_data")
files <- as.list(files)

setwd("./raw_data")
list_file <- list.files(pattern = "*.xml") %>% 
    lapply(xmlToDataFrame, stringsAsFactors = F) %>% bind_rows
setwd("C:/Users/Георгий/Documents/GitHub/rus_traffic_accidents/")
```

Each accident cards starts from a description of region and a name of the
document. This is not relevant to our research - we filter rows, which contain
that information. We also tranform the date to *date - month - year* format and
time to *time* format. Then we arrange the data by date, time, type of accident,
district of accident - we need these columns to create a key later.


```r
list_file <- list_file %>% 
    filter(!(is.na(DTPV))) %>% select(-text)
list_file$date <- dmy(list_file$date)
list_file$time <- as.POSIXct(list_file$time, format = "%H:%M")
xmldf <- list_file %>% arrange(date, time, DTPV, district)
rm(list_file)
```

Summary of the dataset shows that columns *KTS*, *KUCH*, *POG*, *RAN* downloaded
as characters, while they are numeric values. 


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
##  Length:5860        Length:5860        Min.   :2020-06-01 00:00:00  
##  Class :character   Class :character   1st Qu.:2020-06-01 10:20:00  
##  Mode  :character   Mode  :character   Median :2020-06-01 15:00:00  
##                                        Mean   :2020-06-01 14:22:47  
##                                        3rd Qu.:2020-06-01 19:00:00  
##                                        Max.   :2020-06-01 23:59:00
```

We tranform those to numeric.


```r
xmldf$KTS <- as.numeric(xmldf$KTS)
xmldf$KUCH <- as.numeric(xmldf$KUCH)
xmldf$POG <- as.numeric(xmldf$POG)
xmldf$RAN <- as.numeric(xmldf$RAN)
```

Check if there are missing values in the dataset after all transformations.


```r
sum(complete.cases(xmldf))
```

```
## [1] 5860
```

Column *infoDtp* contains descriptions of accidents: weather conditions, road
conditions, coordinates etc. Each description is a string with no delimiters
and different length. The order of description items differs by lines also.
This limits our ability to get the data from the column. We start from getting
driving mode after an accident, road type and coordinates.


```r
driving_mode <- xmldf %>% 
    separate(infoDtp, into = c("text1"), 
             sep = "([:digit:]+\\.[:digit:]+\\.[:digit:]+)")
driving_mode <- driving_mode %>% 
    separate(text1, into = c("text1"), 
             sep = "([:digit:]+\\.[:digit:]+)") %>% select(text1)


xmldf <- cbind(xmldf, driving_mode)
xmldf$driving_mode <- xmldf$text1
xmldf <- xmldf %>% select(-text1)

coords_dtp <- xmldf %>% 
    select(date, infoDtp) %>% 
    extract(infoDtp, c("text1"), 
            "([:digit:]+\\.[:digit:]+\\.[:digit:]+)", remove = TRUE) %>% 
    select(text1)

xmldf <- cbind(xmldf, coords_dtp)
xmldf$coords_dtp <- xmldf$text1
xmldf <- xmldf %>% select(-text1)

road_type <- xmldf %>% select(infoDtp) %>% 
    separate(infoDtp, into = c("text1", "text2"), 
             sep = "([:digit:]+\\.[:digit:]+\\.[:digit:]+)") %>% 
    select(text2)

road_type <- road_type %>% 
    extract(text2, c("text1"), "([^A-z]+[:punct:])", remove = FALSE)

road_type <- road_type %>% 
    extract(text1, c("text1"), "([:alnum:]+[:alnum:]+)", remove = TRUE)

xmldf$road_type <- road_type$text1
```

This is as far as we could get with sensible regular expressions - there should
be a more simple way to get the required data. We found out that *.csv* files
contain some of the information from accidents card in a format that we could
use for our analysis. The idea is to create a dataset with weather and road
conditions and join it with the first dataset on a key. The key is the date, 
time, type of accident and a district.

We download *.csv* files and merge them. Then make the columns and join both
datasets.


```r
### read all the csv files

setwd("./csv_data")
list_file <- list.files(pattern = "*.csv") %>% 
    lapply(read.csv, sep = ";", encoding = "UTF-8") %>% bind_rows
setwd("C:/Users/Георгий/Documents/GitHub/rus_traffic_accidents/")
raw_data <- data.frame(list_file)

### transform .csv file to split the columns

raw_data <- raw_data %>% filter(!is.na(Номер))
raw_data <- unique(raw_data)
raw_data <- raw_data %>% select(Номер, Дата, Время, Схема, Широта, Вид.ДТП,
                                     Адрес, Дорога, Категория.дороги, Состояние.погоды.1, 
                                     Состояние.проезжей.части, Освещение)

raw_data$weather_cond <- paste(raw_data$Состояние.погоды.1,
                               raw_data$Состояние.проезжей.части)
raw_data$road_cond <- raw_data$Освещение
raw_data$date <- raw_data$Дата
raw_data$time <- raw_data$Время
raw_data$id <- raw_data$Схема
raw_data$latitude <- raw_data$Широта
raw_data$longitude <- raw_data$Вид.ДТП
raw_data$road_cat <- raw_data$Категория.дороги
raw_data$type <- raw_data$Адрес
raw_data$district <- raw_data$Дорога

raw_data <- raw_data %>% select(date, time, type, district, latitude, longitude, 
                                road_cat, road_cond, weather_cond)

raw_data$date <- dmy(raw_data$date)
raw_data <- raw_data %>% arrange(date, time, type, district)
raw_data$time <- as.POSIXct(raw_data$time, format = "%H:%M")
raw_data$key <- paste(raw_data$date, raw_data$time, raw_data$type, raw_data$district)

### make key in xml file

xmldf$key <- paste(xmldf$date, xmldf$time, xmldf$DTPV, xmldf$district)

### check if all rows match by the key

set_raw <- raw_data
set_xml <- xmldf
d1 <- data.frame(set_raw, set_xml)
d1$check <- d1$key == d1$key.1
sum(d1$check == TRUE)
```

```
## [1] 5860
```

```r
rm(set_raw, set_xml, d1)
rm(files, coords_dtp, driving_mode, road_type)

raw_data_sel <- raw_data %>% select(key.1 = key, latitude, longitude, road_cat,
                                    road_cond, weather_cond)

### merge two datasets

d2 <- cbind(xmldf, raw_data_sel)
rm(list_file, xmldf, raw_data, raw_data_sel)
```

Then we kick temporary columns and write the file as our raw data.


```r
### kick temporary columns

d2 <- d2 %>% select(dtpv = DTPV, date, time, district, kts = KTS, kuch = KUCH, 
                    fatal = POG, injury = RAN, driving_mode, latitude, 
                    longitude, road_cat, road_cond, weather_cond)

# save raw data

write.csv(d2, "raw_data.csv")
```

## Cleaning data

